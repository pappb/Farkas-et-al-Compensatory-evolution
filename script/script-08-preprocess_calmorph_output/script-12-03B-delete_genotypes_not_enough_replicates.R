# megnezem mely genotipusoknak nincs eleg replikatuma
# vagy nincs ancestora, vagy evolvaltja, akkor torlom

options( encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)
source("script/script-03-tryGLMM/function-my_xlsx_save.R")




big_tbl<-read_rds("data/preprocess_calmorph_output/smaller_big_tbl.rds")
big_tbl<-big_tbl %>% mutate(Dgroup2=ifelse(Dgroup=="A1"|Dgroup=="B" , "A1B",Dgroup))
#big_tbl<-big_tbl %>% mutate(row_idx_tmp=1:n())


strain_tbl<-read_csv(file = "data/preprocess_calmorph_output/YMP_tables/YMP_strain_list.csv")
tbl1<-big_tbl %>%
  filter(is_ok) %>% 
  select(plateID, well, genotypeID) %>% distinct() %>%
  left_join(strain_tbl %>%
              #select(genotype_ID, unique_ID,deletion_gene) %>%
              rename(genotypeID=genotype_ID) ,
            by = "genotypeID")
  



rep_cnt_tbl<- tbl1 %>%
  group_by( deletion_gene, deletion_ORF, ANCvsEVO , genotypeID) %>%  summarise(replicated_well_cnt=n(), .groups="drop") 


rep_cnt_tbl<- rep_cnt_tbl %>%
  mutate(is_marked_for_deletion_not_enough_rplicates=  (replicated_well_cnt<=2)) %>%
  arrange(!is_marked_for_deletion_not_enough_rplicates)



my_xlsx_save(rep_cnt_tbl,file = "data/preprocess_calmorph_output/morphology_tbls.xlsx", sheet = "cnt_repliates_of_genotype")




big_tbl2<- big_tbl %>% left_join(rep_cnt_tbl %>%  select(genotypeID,is_marked_for_deletion_not_enough_rplicates), by = "genotypeID")

# azok a genotipusok, amik mas miatt torolve vannak NA erteket kaptak. Ezeket atirom FALSE-ra
big_tbl2<-big_tbl2 %>%  mutate(is_marked_for_deletion_not_enough_rplicates = ifelse(is.na(is_marked_for_deletion_not_enough_rplicates), FALSE,is_marked_for_deletion_not_enough_rplicates))


############
# check consistency

stopifnot(nrow(big_tbl)==nrow(big_tbl2))
stopifnot(ncol(big_tbl)+1 ==ncol(big_tbl2))
stopifnot(all(!is.na(big_tbl2$is_marked_for_deletion_not_enough_rplicates)))

big_tbl<-big_tbl2
rm(big_tbl2)
##########


big_tbl <- big_tbl %>%mutate(is_ok= is_ok & (!is_marked_for_deletion_not_enough_rplicates) ) 

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
