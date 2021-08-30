

options( encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)
#source("script/script-03-tryGLMM/function-my_xlsx_save.R")



big_tbl<-read_rds("data/preprocess_calmorph_output/smaller_big_tbl.rds")
big_tbl<-big_tbl %>% mutate(Dgroup2=ifelse(Dgroup=="A1"|Dgroup=="B" , "A1B",Dgroup))
big_tbl<-big_tbl %>% mutate(row_idx_tmp=1:n())
trait_description_tbl<-read_rds("data/preprocess_calmorph_output/trait_description_tbl.rds")
trait_description_tbl1<-
  trait_description_tbl %>% 
    select(trait,is_used) %>% 
    mutate(trait0=gsub("^([^_]+)_([^_]+)$","\\1", trait),
           Dgroup2=gsub("^([^_]+)_([^_]+)$","\\2", trait)
           )


res1<-list()
for( abc in c("A","A1B","C"))
{
  trait_list1<- trait_description_tbl1 %>% filter(is_used) %>%  filter(Dgroup2==abc) %>% .$trait0
  
  tbl1<-big_tbl %>%  filter(Dgroup2==abc) 
  
   cnt_of_NAs<- tbl1 %>% select(all_of(trait_list1)) %>% mutate_all(.funs = is.na) %>%  as.matrix() %>%  apply( MARGIN = 1,FUN = sum)
  # mean(cnt_of_NAs!=0) 

  
   tbl1<-tbl1 %>% mutate(  is_marked_for_deletion_because_contains_NA = ( cnt_of_NAs != 0 ))
   
   res1[[abc]]<-tbl1
}

big_tbl2<-do.call(bind_rows, res1)
rm(res1)
big_tbl2<- big_tbl2 %>%  arrange(row_idx_tmp) %>% select( - row_idx_tmp) # restore the original row order

############
# check consistency

stopifnot(nrow(big_tbl)==nrow(big_tbl2))
stopifnot(ncol(big_tbl) ==ncol(big_tbl2))
stopifnot(all(!is.na(big_tbl2$is_marked_for_deletion_because_contains_NA)))
#big_tbl2 %>%  filter(is.na(is_marked_for_deletion_because_contains_NA)) %>% View()

big_tbl<-big_tbl2
rm(big_tbl2)
##########

big_tbl <- big_tbl %>%mutate(is_ok= is_ok & (!is_marked_for_deletion_because_contains_NA) ) 

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
