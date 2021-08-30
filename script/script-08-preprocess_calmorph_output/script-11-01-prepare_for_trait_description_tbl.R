## MEgszamolom mely trait-eknél mekkora az NA-k araánya
## ahol tobb mint 3% azokat kiteszem egy tablazatba


options(device=function() {x11(title="R-GR",xpos=0, ypos=0, width=9.55,height=12)} , encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)
source("script/script-03-tryGLMM/function-my_xlsx_save.R")





big_tbl<-read_rds("data/preprocess_calmorph_output/smaller_big_tbl.rds")
big_tbl<-big_tbl %>% mutate(Dgroup2=ifelse(Dgroup=="A1" | Dgroup=="B", "A1B", Dgroup)) %>% select(Dgroup2, everything())

trait_names<-grep("^[CD]\\d\\d.*$", value = TRUE, names(big_tbl))
colnames1<-setdiff(names(big_tbl), trait_names)


#abc="A1B"
res1<-c()
for( abc in c("A","A1B","C"))
{
  tbl1<-big_tbl %>%  filter(is_ok) %>%  filter(Dgroup2==abc)
  idx1<-tbl1 %>% summarise_at(.vars = trait_names, .funs = function(x) {any(!is.na(x))}) %>% unlist()
  sum(!idx1)  
  sum(idx1)  
  
  na_ratio<-tbl1 %>% summarise_at(.vars = trait_names[idx1], .funs = function(x) {mean(is.na(x))})
  names(na_ratio)<-paste0(names(na_ratio),"_",abc)
  na_ratio<-na_ratio %>% unlist()
  res1<-c(res1,na_ratio)
 #print(  t(na_ratio[na_ratio>0.03]))
}

idx1<-res1>0.10

na_ratio_tbl <- tibble(trait=names(res1[idx1]), comment= sprintf("more then 10 %% of data are NA (%0.1f %%)", 100*res1[idx1] ))

write_rds(x = na_ratio_tbl,  "data/preprocess_calmorph_output/traits_width_too_many_NAs.rds")
write_csv(x = na_ratio_tbl,  "data/preprocess_calmorph_output/traits_width_too_many_NAs.csv")
my_xlsx_save(na_ratio_tbl,file = "data/preprocess_calmorph_output/morphology_tbls.xlsx", sheet = "traits_width_too_many_NAs")



# for( abc in c("A","A1B","C"))
# {
#   tbl1<-big_tbl %>%    filter(Dgroup2==abc)
#   
#   trait_list2<-names(res1[!idx1])
#   trait_list2<-grep(paste0("^([^_]+)_",abc,"$"),trait_list2, value = TRUE)
#   trait_list2<-gsub(paste0("^([^_]+)_",abc,"$"),"\\1",trait_list2)
#   
#   mx<-tbl1 %>% select(all_of(trait_list2)) %>% as.matrix()
#   idx_row<-!apply(mx,1,FUN = function(x) all(is.finite(x))) 
#   
#   stopifnot(nrow(tbl1)==length(idx_row))
#   
#   mean(idx_row)  
#   tbl1
#   
#   na_ratio<-tbl1 %>% summarise_at(.vars = trait_names[idx1], .funs = function(x) {mean(is.na(x))})
#   names(na_ratio)<-paste0(names(na_ratio),"_",abc)
#   na_ratio<-na_ratio %>% unlist()
#   res1<-c(res1,na_ratio)
#   #print(  t(na_ratio[na_ratio>0.03]))
# }
# 
# 
