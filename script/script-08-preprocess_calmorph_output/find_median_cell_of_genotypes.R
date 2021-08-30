
# Author: Fekete Gergo
###############################################################################



options(device=function() {x11(title="R-GR",xpos=0, ypos=0, width=9.55,height=12)} , encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)
library(cowplot)

source("script/script-03-tryGLMM/function-my_xlsx_save.R")
source("script/script-04-preprocess_calmorph_output/function-find_a_cell_image-v2.R")
####################################x####################################x####################################x



plates_tbl<-xlsx::read.xlsx(file = "/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/DATA_of_microscope/plates_tbl.xlsx",sheetIndex = 1,stringsAsFactors=FALSE) %>%
  as_tibble()  %>% 
  mutate_at(.vars=grep("use_",names(. ), ignore.case = TRUE,value =TRUE), .funs = as.logical) %>% 
  filter(!is.na(filename))

plate_map_tbl<-read_rds("data/preprocess_calmorph_output/plate_map_tbl.rds")


outlier_filter_which_traits_are_failed <- read_rds("data/preprocess_calmorph_output/outlier_filter_which_traits_are_failed.rds")

big_tbl <- readRDS("data/preprocess_calmorph_output/smaller_big_tbl.rds") %>% 
  filter(is_ok)

big_tbl <- big_tbl %>%
  #semi_join(outlier_filter_which_traits_are_failed,by=c("plateID", "well", "genotypeID",  "cell_id")) %>% 
  semi_join(plate_map_tbl %>% filter(use_for_paper  ),by=c("plateID", "well") )

gc(reset = TRUE, full = TRUE)
#rm(big_tbl)
#####################

data0<-read_rds("data/data_actual/data0.rds")

out_dir="out/2020_02_04-median_of_cells/"
dir.create(out_dir)



#########################################################################################

trait_list<-grep("^[CD]\\d+(-\\d+)?$",names(outlier_filter_which_traits_are_failed), value = TRUE) 

lft1<-big_tbl %>% select( plateID,well, genotypeID,Dgroup,  cell_id, trait_list) %>% 
  gather(key="trait", value="value",-plateID,-well, -genotypeID, -Dgroup,  -cell_id)

lft1 <- lft1 %>%  group_by(trait, Dgroup) %>% filter(mean(!is.na(value))>0.97 ) %>% ungroup()
lft1 <- lft1 %>%  group_by(plateID,well,cell_id) %>% filter(sum(is.na(value))==0 ) %>% ungroup()
gc()

stopifnot(0==sum(is.na(lft1$value)))
# long format table
lft1<-lft1 %>%
  group_by(plateID, genotypeID, Dgroup) %>%
  filter(n>100) %>% 
  ungroup() %>% 
  group_by(plateID, genotypeID, trait, Dgroup) %>%
  mutate(q=rank(value,  ties.method = "average")/n()) %>%
  ungroup()

# sejtenként de trait-ek nelkul
lft2<-lft1 %>%
  group_by(plateID, genotypeID, Dgroup,well, cell_id) %>%
  summarise(centrality=max(abs(0.5-q))) %>%
  ungroup()


lft3<-lft2 %>%
  group_by(plateID, genotypeID, Dgroup) %>%
  filter(centrality==min(centrality)) %>%
  ungroup() %>%
  left_join(plate_map_tbl %>% select(plateID,genotypeID, genotype_label) %>%  distinct()) %>%
  arrange(plateID, genotypeID, Dgroup)
# 
# 
# lft3 %>% arrange(desc(centrality)) %>%  head(10)
# 
# lft3 %>% ggplot(aes(x=centrality))+geom_density() + geom_rug()
gc(reset = TRUE, full = TRUE)


# tmp<-lft1 %>%  group_by(trait, Dgroup) %>% summarise(length(unique(value)))

if(file.exists( "traits_tbl2.rds"))
{
  traits_tbl2<-read_rds( "traits_tbl2.rds")
}else{
  
  traits_tbl1<-lft1 %>%  select(trait, Dgroup) %>% distinct()
  traits_tbl2<- full_join(traits_tbl1 , traits_tbl1, by="Dgroup") %>%
    filter(trait.x<trait.y) %>% 
    mutate(max_centrality=as.double(NA), plateID=as.character(NA),  genotypeID=as.character(NA) )
}

for( i in 1:nrow(traits_tbl2))
{
  r1<-traits_tbl2[i,]
  if(is.na(r1$max_centrality)){
    
    cat(i,"/", nrow(traits_tbl2),  r1$Dgroup, r1$trait.x, r1$trait.y, " ")
    
    tmp1<-lft1 %>% filter(Dgroup==r1$Dgroup & (trait==r1$trait.x | trait==r1$trait.y) )
    
    tmp2<-tmp1 %>%
      group_by(plateID, genotypeID,Dgroup,well, cell_id) %>%
      summarise(centrality=max(abs(0.5-q))) %>%
      ungroup()
    
    
    tmp3<-tmp2 %>%
      group_by(plateID, genotypeID, Dgroup) %>%
      summarise(centrality=min(centrality)) %>%
      ungroup() 
    #left_join(plate_map_tbl %>% select(plateID,genotypeID, genotype_label) %>%  distinct()) %>% 
    #arrange(plateID, genotypeID, Dgroup)
    
    r2<-tmp3[max(which.max(tmp3$centrality)),]
    
    traits_tbl2$max_centrality[i]<- r2$centrality
    traits_tbl2$plateID[i]<-r2$plateID
    traits_tbl2$genotypeID[i]<-r2$genotypeID
      
    cat(traits_tbl2$max_centrality[i],"\n")
    
    if( i%%50==0){
      cat("write subresult to file\n")
      write_rds(traits_tbl2, "traits_tbl2.rds", compress = "gz")
      gc(reset = TRUE, full = TRUE)
    }
  }
}

