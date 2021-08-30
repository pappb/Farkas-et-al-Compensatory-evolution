

options( encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)
#source("script/script-03-tryGLMM/function-my_xlsx_save.R")



big_tbl<-read_rds("data/preprocess_calmorph_output/smaller_big_tbl.rds")



outlier_well_tbl <- read_rds( "data/preprocess_calmorph_output/outlier_wells-score_tbl.rds")
outlier_well_tbl2<- outlier_well_tbl %>% 
  mutate(is_marked_for_deletion_as_outlier_well=!is_ok) %>% 
  select(plateID, well,is_marked_for_deletion_as_outlier_well )

N<-nrow(big_tbl)
is_ok_cnt1<-sum(big_tbl$is_ok)


big_tbl<-left_join(big_tbl, outlier_well_tbl2, by = c("plateID", "well"))
stopifnot(N==nrow(big_tbl))


###
# rendezem az oszlopok sorrendjet a konnyebb attekinthetoseg kedvejert


namelist0<-names(big_tbl)
first_columns<-c("plateID", "well","genotypeID","image_number", "Dgroup2","is_ok"  )
is_named_columns<-grep("^is_.+$",setdiff(namelist0,first_columns), value=TRUE)
rest_of_columns<-setdiff(namelist0,c(first_columns, is_named_columns))

namelist_orderd<- c(first_columns, is_named_columns, rest_of_columns)

big_tbl<-big_tbl %>%  select(all_of(namelist_orderd))
#####################

is_not_ok<-big_tbl %>% select(is_named_columns) %>% as.matrix() %>%  apply(MARGIN = 1,FUN = any)
big_tbl<-big_tbl %>%   mutate(is_ok =  !is_not_ok)
stopifnot(!any(is.na(big_tbl$is_ok)))
  

is_ok_cnt2<-sum(big_tbl$is_ok)
cat( sprintf("A sejtek %0.1f %% -a lett torolve, azert mert outlier well-ben van.\n", 100* (1-(is_ok_cnt2/is_ok_cnt1))))

stopifnot(is_ok_cnt1 >= is_ok_cnt2)

write_rds(big_tbl, "data/preprocess_calmorph_output/smaller_big_tbl.rds", compress = "gz")

cat("Done.\n")
