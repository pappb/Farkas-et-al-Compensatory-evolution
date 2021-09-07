# minden well-hez keres egy-egy reprezentáns sejtet az A, A1B és C csoportokból.
# valamiféle sokdimenzios mediánt számol: minden trait oszlopot rang tarnaszformál,
# aztán [0,1] közé skálázza, 
# kivon 0.5-öt, hogy a 0 érték jelölje a mdeiánt, majd négzetes középértéket szánmol soronként.
# minél kisebb az így kapott score annál inkább a medián környékén van sok traitben az adott sorhoz tartozó sejt.
#
# A smaller_big_tbl-be betesz 2 új oszlopot:
# A well_represantant_score nevű oszlop tartalmazza a scoret,
# a well_represantant nevű TRUE/FALSe oszlop csak a reprezentás sejteknél TRUE


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

result_list<-list()
well_tbl<-big_tbl %>% select(plateID, well, Dgroup2) %>% distinct()
for(i in 1:nrow(well_tbl))
{
  # i=100  
  if(i%%100==0) {cat(i,"\n")}
  tbl1<-big_tbl %>%  semi_join(well_tbl[i,], by = c("plateID", "well", "Dgroup2"))
  stopifnot(  all(!duplicated(tbl1$cell_id))) # ellenorzom, hogyx a cell_id unique -e egy well-en belül
  tbl1_not_ok<-tbl1 %>%  filter(!is_ok)
  
  tbl1<-tbl1 %>%  filter(is_ok)
  
  if(nrow(tbl1)>0)
  {
    trait_list<-trait_tbl %>%  semi_join(well_tbl[i,], by = "Dgroup2") %>% .$trait
    mx<-tbl1 %>%  select(trait_list) %>%  as.matrix()
    rownames(mx)<-tbl1$cell_id
    
    stopifnot(all(!is.na(mx)))
    
    
    mx2<-apply(mx, MARGIN = 2, FUN = function(x) {rank(x)/length(x)  })
    mx2<-apply(mx2, MARGIN = c(1,2), FUN = function(x) {(x-0.5)^2})
    mm_score<-apply(mx2, MARGIN = 1, FUN = mean) ^(1/2)
    
    
    tf<-rep(FALSE, nrow(tbl1))
    tf[which.min(mm_score)]<-TRUE
    tbl1<-tbl1 %>%  mutate(well_represantant_score=mm_score) %>% 
      mutate(well_represantant=tf) 
    
    
    result_list[[length(result_list)+1]] <- tbl1
  }
  
  if(nrow(tbl1_not_ok)>0)
  {
    
    tbl1_not_ok<-tbl1_not_ok %>%  add_column(well_represantant_score=1) %>% 
      add_column(well_represantant=FALSE) 
    
    result_list[[length(result_list)+1]] <- tbl1_not_ok
  }
}

big_tbl2<-do.call(bind_rows,result_list)
rm(result_list)
stopifnot(nrow(big_tbl)==nrow(big_tbl2))
#stopifnot(col(big_tbl)+2==ncol(big_tbl2))

big_tbl<-write_rds(big_tbl2,"data/preprocess_calmorph_output/smaller_big_tbl.rds")

cat("Done\n")

tbl1<-big_tbl %>%  semi_join(well_tbl[100,], by = c("plateID", "well", "Dgroup2"))
tbl1 %>% ggplot(aes(x=D106))+geom_histogram()+
  geom_vline(data=tbl1 %>%  filter(well_represantant), mapping =aes(xintercept=D106) )


