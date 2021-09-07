

options( encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)
#source("script/script-03-tryGLMM/function-my_xlsx_save.R")



big_tbl<-read_rds("data/preprocess_calmorph_output/smaller_big_tbl.rds")

blacklist_genotype_tbl<-read_csv("/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/DATA_of_microscope/YMP_tables/YMP_strain_blacklist.csv")
blacklist_genotype_tbl<-blacklist_genotype_tbl %>% rename(genotypeID=genotype_ID)

# ellenorzom a blacklist-ek konzisztenciáját
#stopifnot( all(unique(blacklist_genotype_tbl$genotypeID) %in% unique(big_tbl$genotypeID)))


###
# toroltnek jelölöm ami a balcklist-en van


tmp<-blacklist_genotype_tbl %>%  select(genotypeID) %>% distinct() %>%  add_column(is_marked_for_deletion_by_blacklist_of_genotypes=TRUE)
big_tbl<-big_tbl %>% 
  left_join(tmp, by=c("genotypeID")) %>% 
  mutate(is_marked_for_deletion_by_blacklist_of_genotypes =
           ifelse(is.na(is_marked_for_deletion_by_blacklist_of_genotypes), FALSE,TRUE) )


big_tbl<-big_tbl %>%mutate(is_ok= is_ok & (!is_marked_for_deletion_by_blacklist_of_genotypes)    ) 

###
# rendezem az oszlopok sorrendjet a konnyebb attekinthetoseg kedvejert

namelist0<-names(big_tbl)
first_columns<-c("plateID", "well","genotypeID","image_number", "is_ok"  )
is_named_columns<-grep("^is_.+$",setdiff(namelist0,first_columns), value=TRUE)
rest_of_columns<-setdiff(namelist0,c(first_columns, is_named_columns))

namelist_orderd<- c(first_columns, is_named_columns, rest_of_columns)

big_tbl<-big_tbl %>%  select(all_of(namelist_orderd))


cat( sprintf("A sejtek %0.1f %% -a lett torolve, azert mert genotype balcklis-en van.\n",
             100* mean(big_tbl$is_marked_for_deletion_by_blacklist_of_genotypes)))

cat( sprintf("A sejtek %0.1f %% -a lett torolve, osszesen (is_ok).\n",
             100* mean(!big_tbl$is_ok)))

stopifnot ( !any(is.na(big_tbl$is_ok)))

write_rds(big_tbl,  "data/preprocess_calmorph_output/smaller_big_tbl.rds", compress = "gz")

cat("Done.\n")
