# megnezem mely genotipusoknak nincs eleg replikatuma
# vagy nincs ancestora, vagy evolvaltja, akkor torlom

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
tbl1<-big_tbl %>%
  filter(is_ok) %>% 
  select(plateID, well, genotypeID) %>% distinct() %>%
  left_join(strain_tbl %>%
              #select(genotype_ID, unique_ID,deletion_gene) %>%
              rename(genotypeID=genotype_ID) ,
            by = "genotypeID")
  



rep_cnt_tbl<- tbl1 %>%
  group_by( deletion_gene, deletion_ORF, ANCvsEVO , genotypeID) %>%  summarise(replicated_well_cnt=n(), .groups="drop") 




rep_cnt_tbl_2<-full_join(
  rep_cnt_tbl %>%  filter(ANCvsEVO=="ancestor") %>% select(-ANCvsEVO) %>% rename(ancestor_replicated_well_cnt=replicated_well_cnt),
  rep_cnt_tbl %>%  filter(ANCvsEVO=="evolved") %>% select(-ANCvsEVO) %>% rename(evolved_replicated_well_cnt=replicated_well_cnt),
  by=c("deletion_gene", "deletion_ORF")
) %>% 
  mutate(
    evolved_replicated_well_cnt = replace(evolved_replicated_well_cnt,is.na(evolved_replicated_well_cnt),0),
    ancestor_replicated_well_cnt = replace(ancestor_replicated_well_cnt,is.na(ancestor_replicated_well_cnt),0)
    )   


# combine rows of the paralel evolved lines
rep_cnt_tbl_3<-
  rep_cnt_tbl_2 %>%
  group_by(deletion_gene,deletion_ORF) %>%
  summarise(
    anc_genotype=paste(unique(sprintf("%s[%d]", genotypeID.x, ancestor_replicated_well_cnt)), collapse=","),
    evo_genotype=paste(unique(sprintf("%s[%d]", genotypeID.y, evolved_replicated_well_cnt )), collapse=","),
    ancestor_replicated_well_cnt=min(ancestor_replicated_well_cnt),
    min_evolved_replicated_well_cnt=min(evolved_replicated_well_cnt),
    max_evolved_replicated_well_cnt=max(evolved_replicated_well_cnt),
    evolved_strain_cnt=n()
    
    ) 

rep_cnt_tbl_3<-rep_cnt_tbl_3 %>%
  mutate(delete_something_from_it = (ancestor_replicated_well_cnt<=2) | (min_evolved_replicated_well_cnt<=2) ) %>% 
  arrange(!delete_something_from_it,deletion_gene)



# delete from the evolved strains
delete_list1<-rep_cnt_tbl_2 %>%  filter(evolved_replicated_well_cnt<=2) %>% .$genotypeID.y
filter_tbl2<-strain_tbl %>% select(genotype_ID,  unique_ID ,   deletion_gene, deletion_ORF) %>%  filter(genotype_ID %in% delete_list1) %>%  left_join(rep_cnt_tbl_3,  by = c("deletion_gene", "deletion_ORF")) %>% rename(genotypeID=genotype_ID)

# delete ancestors and evolved versions of them
tmp<-rep_cnt_tbl_3 %>%  filter(delete_something_from_it)
tmp<-tmp %>% filter( (ancestor_replicated_well_cnt<=2) | (max_evolved_replicated_well_cnt<=2) )
filter_tbl1<-strain_tbl %>% select(genotype_ID,  unique_ID ,   deletion_gene, deletion_ORF) %>% right_join(tmp,  by = c("deletion_gene", "deletion_ORF")) %>% rename(genotypeID=genotype_ID)



# azok  a genotipusok vannak itt, amiket törölni kell. Csak a genotypeID a fontos oszlop 
filter_tbl<-bind_rows(filter_tbl1, filter_tbl2) %>% distinct()

my_xlsx_save(filter_tbl,file = "data/preprocess_calmorph_output/morphology_tbls.xlsx", sheet = "cnt_repliates_of_genotype")



big_tbl<- big_tbl %>% mutate(row_idx_tmp=1:n()) 

big_tbl2<-bind_rows(
  big_tbl %>%  semi_join(filter_tbl,by="genotypeID") %>%  mutate(is_marked_for_deletion_not_enough_rplicates_or_no_ancestor=TRUE),
  big_tbl %>%  anti_join(filter_tbl,by="genotypeID") %>%  mutate(is_marked_for_deletion_not_enough_rplicates_or_no_ancestor=FALSE)
)

############
# check consistency

stopifnot(nrow(big_tbl)==nrow(big_tbl2))
stopifnot(ncol(big_tbl)+1 ==ncol(big_tbl2))
stopifnot(all(!is.na(big_tbl2$is_marked_for_deletion_not_enough_rplicates_or_no_ancestor)))
#big_tbl2 %>%  filter(is.na(is_marked_for_deletion_because_contains_NA)) %>% View()

big_tbl<-big_tbl2
rm(big_tbl2)
##########


big_tbl <- big_tbl %>%mutate(is_ok= is_ok & (!is_marked_for_deletion_not_enough_rplicates_or_no_ancestor) ) 

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
