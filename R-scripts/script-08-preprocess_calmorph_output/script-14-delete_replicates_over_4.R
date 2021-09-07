# Ha valamibol 4-nel tobb replikatum van, akkor törli a folosleget

options( encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)
source("script/script-03-tryGLMM/function-my_xlsx_save.R")



big_tbl<-read_rds("data/preprocess_calmorph_output/smaller_big_tbl.rds")
N1<-nrow(big_tbl)


tbl1<-big_tbl %>%  filter(is_ok) %>% select(plateID, well, genotypeID) %>% distinct() %>% 
  group_by(genotypeID) %>% mutate(N=n(), rep_num=1:n()) %>% 
  arrange(genotypeID) %>% 
  ungroup()

tbl2<-tbl1 %>% 
  mutate(is_marked_for_deletion_because_more_than_4_replicates=
           rep_num>4 & genotypeID!="SZ2014_YMP_255") %>% 
  arrange(desc(N),genotypeID)  

tbl3<-tbl2 %>%  select(plateID, well, is_marked_for_deletion_because_more_than_4_replicates)
# beteszem azokat a well-eket is, amikbol mar minden torolve van
tbl3_rest<- big_tbl %>%
  select(plateID, well) %>%  distinct() %>%
  anti_join(tbl3) %>% 
  mutate( is_marked_for_deletion_because_more_than_4_replicates=FALSE)

tbl3<-bind_rows(tbl3, tbl3_rest)

big_tbl<-big_tbl %>% left_join(tbl3 , by=c("plateID", "well"))
N2<-nrow(big_tbl)

stopifnot(N1==N2)

cnt1<-sum(big_tbl$is_ok)
big_tbl<-big_tbl %>%mutate(is_ok= is_ok &
                             (!is_marked_for_deletion_because_more_than_4_replicates) 
                            ) 
cnt2<-sum(big_tbl$is_ok)
stopifnot(cnt2<=cnt1)


namelist0<-names(big_tbl)
first_columns<-c("plateID", "well","genotypeID","image_number", "is_ok"  )
is_named_columns<-grep("^is_.+$",setdiff(namelist0,first_columns), value=TRUE)
rest_of_columns<-setdiff(namelist0,c(first_columns, is_named_columns))

namelist_orderd<- c(first_columns, is_named_columns, rest_of_columns)

big_tbl<-big_tbl %>%  select(all_of(namelist_orderd))


write_rds(big_tbl, path = "data/preprocess_calmorph_output/smaller_big_tbl.rds", compress = "gz")

cat("Done.\n")

sum_tbl1<-big_tbl %>% group_by_at(.vars = c("is_ok", is_named_columns)) %>%  summarise(n=n(), ratio=n()/nrow(big_tbl))
my_xlsx_save(sum_tbl1,file = "data/preprocess_calmorph_output/morphology_tbls.xlsx", sheet = "cnt_of_cell_deletions_while_data_filtering")
