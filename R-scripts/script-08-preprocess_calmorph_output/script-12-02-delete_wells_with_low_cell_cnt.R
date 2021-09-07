# I delete wells with less then 40 cells in any of  A, A1B,C  group

options( encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)
source("script/script-03-tryGLMM/function-my_xlsx_save.R")

#source("script/script-03-tryGLMM/function-my_xlsx_save.R")



big_tbl<-read_rds("data/preprocess_calmorph_output/smaller_big_tbl.rds")
big_tbl<-big_tbl %>% mutate(Dgroup2=ifelse(Dgroup=="A1"|Dgroup=="B" , "A1B",Dgroup))
#big_tbl<-big_tbl %>% mutate(row_idx_tmp=1:n())


strain_tbl<-read_csv(file = "data/preprocess_calmorph_output/YMP_tables/YMP_strain_list.csv")
plate_layout_tbl<-big_tbl %>%
  select(plateID, well, genotypeID) %>% distinct() %>%
  left_join(strain_tbl %>%
              select(genotype_ID, unique_ID,deletion_gene) %>%
              rename(genotypeID=genotype_ID) ,
            by = "genotypeID")
  







  cell_cnt_tbl <- big_tbl %>%  group_by(plateID,well, Dgroup2) %>% summarise(cnt=n(), .groups="drop")
  
  mark_for_deltion_tbl<-cell_cnt_tbl %>%
    mutate(drop_it= (cnt<=40) ) %>%
    group_by(plateID,well) %>% summarise(is_marked_for_deletion_beacuse_low_cell_cnt_in_well=any(drop_it) , .groups="drop")

  
  # just for log into the morphology.xls
 cell_cnt_of_wells<- cell_cnt_tbl %>%
   spread(key="Dgroup2", value="cnt") %>% 
    full_join(mark_for_deltion_tbl, by = c("plateID", "well")) %>% 
    left_join(plate_layout_tbl, by = c("plateID", "well")) %>% 
    arrange(!is_marked_for_deletion_beacuse_low_cell_cnt_in_well)
 
 my_xlsx_save(cell_cnt_of_wells,file = "data/preprocess_calmorph_output/morphology_tbls.xlsx", sheet = "cell_cnt_of_wells")
 
  
big_tbl2<-big_tbl %>% full_join(mark_for_deltion_tbl, by = c("plateID", "well"))
############
# check consistency

stopifnot(nrow(big_tbl)==nrow(big_tbl2))
stopifnot(ncol(big_tbl)+1 ==ncol(big_tbl2))
stopifnot(all(!is.na(big_tbl2$is_marked_for_deletion_beacuse_low_cell_cnt_in_well)))
#big_tbl2 %>%  filter(is.na(is_marked_for_deletion_because_contains_NA)) %>% View()

big_tbl<-big_tbl2
rm(big_tbl2)
##########

big_tbl <- big_tbl %>%mutate(is_ok= is_ok & (!is_marked_for_deletion_beacuse_low_cell_cnt_in_well) ) 

###
# rendezem az oszlopok sorrendjet a konnyebb attekinthetoseg kedvejert

namelist0<-names(big_tbl)
first_columns<-c("plateID", "well","genotypeID","image_number", "Dgroup2","is_ok"  )
is_named_columns<-grep("^is_.+$",setdiff(namelist0,first_columns), value=TRUE)
rest_of_columns<-setdiff(namelist0,c(first_columns, is_named_columns))

namelist_orderd<- c(first_columns, is_named_columns, rest_of_columns)

big_tbl<-big_tbl %>%  select(all_of(namelist_orderd))




write_rds(big_tbl,  "data/preprocess_calmorph_output/smaller_big_tbl.rds", compress = "gz")

cat("Done.\n")
