# ez valami félkész dolog. Egyelore nem csinal semmit
#
# Az volt a terv, hog yugy keresek reprezentanms sejtet minden wellbe ugy, hogy körbe körbe megyek a 


options( encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)
#source("script/script-03-tryGLMM/function-my_xlsx_save.R")

source("script/script-08-preprocess_calmorph_output/my_parameters.R")


big_tbl<-read_rds("data/preprocess_calmorph_output/smaller_big_tbl.rds")
# big_tbl<-big_tbl %>%  filter(is_ok) %>% 
#   mutate(Dgroup2= case_when( (Dgroup=="A1"  | Dgroup=="B" ) ~ "A1B", TRUE~ Dgroup)) %>% 
#   select(Dgroup2, everything())

trait_names<-grep("^[CD]\\d\\d.*$",names(big_tbl), value = TRUE)
# 
trait_description_tbl<-read_rds("data/preprocess_calmorph_output/trait_description_tbl.rds")
trait_tbl<-trait_description_tbl %>% filter(is_used) %>%  select(trait) %>%
  rename(trait0=trait) %>% 
  mutate(trait=gsub("^([^_]+)_([ABC1]+)$","\\1",trait0),
         Dgroup2=gsub("^([^_]+)_([ABC1]+)$","\\2",trait0)
  )



out_dir1<-"out/2021_07_21-select_representent_cells_for_each_well-mutidim_median/"
dir.create(out_dir1,recursive = TRUE)


well_tbl<-big_tbl %>% select(plateID, well, Dgroup2) %>% distinct()
for(i in 1:nrow(well_tbl))
{
 # i=100  
  tbl1<-big_tbl %>%  semi_join(well_tbl[i,])
  stopifnot(  all(!duplicated(tbl1$cell_id))) # ellenorzom, hogyx a cell_id unique -e egy well-en belül
  tbl1<-tbl1 %>%  filter(is_ok)

  trait_list<-trait_tbl %>%  semi_join(well_tbl[i,], by = "Dgroup2") %>% .$trait
  shuffled_trait_list<- sample(trait_list,replace = FALSE)

  for(i_tr in seq(shuffled_trait_list)){
    # i_tr =1
    if(nrow(tbl1) <= 1){
      break;
      }
    
    current_trait<-shuffled_trait_list[i_tr]
    x<- tbl1[[current_trait]]
    idx0<-rep(TRUE,length(x))
    idx1<-which.max(x)
    idx2<-which.min(x)
    idx0[idx1]<-FALSE
    idx0[idx2]<-FALSE
    
    tbl2<-tbl1[!idx0,]
    tbl2 %>%  add_column(.before = 1, mdm_idx= xx)
    
    tbl1<-tbl1[idx0,]
    
    }

  
}
