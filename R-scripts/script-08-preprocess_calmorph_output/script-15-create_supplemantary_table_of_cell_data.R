# csak a színtiszta adatoknak csinalaok 3 tablazatot
#
# Ezt a nagy táblázatot szetdi szét a program 3 részre (A, A1B, és C),
#

graphics.off()

rm(list=ls())
gc()

library(tidyverse)
####################################x####################################x####################################x

big_tbl<- read_rds("data/preprocess_calmorph_output/smaller_big_tbl.rds")
big_tbl<-big_tbl %>%  filter(is_ok)


trait_description_tbl<-read_rds("data/preprocess_calmorph_output/trait_description_tbl.rds")
trait_description_tbl<-trait_description_tbl %>%  filter(is_used)

header_names<-c("plateID","well", "genotypeID")


for(abc in c("A","A1B","C"))
{
  
  cat("Dgroup=" ,abc,"\n", sep="")
  
  trait_list1 <- trait_description_tbl %>% filter(is_used) %>% filter(grepl(paste0("_",abc,"$"),trait)) %>% .$trait
  
  colname_tbl1<-tibble(orig=colnames(big_tbl))
  colname_tbl1<-colname_tbl1 %>%
    mutate(renamed=case_when(
      (paste0(orig,"_", abc) %in% trait_list1) ~ paste0(orig,"_", abc),
      (orig %in% header_names)  ~ orig,
      TRUE~"DELETE_IT"
    ))
  colname_tbl1<-colname_tbl1 %>% filter(renamed!="DELETE_IT")
  
  tbl1<-big_tbl %>%filter(Dgroup2==abc) %>%  select(all_of(colname_tbl1$orig))
  write_csv(tbl1,  paste0("data/preprocess_calmorph_output/calmorph_preprocessed_data-",abc,".csv"))
}  



cat("Done.\n")

