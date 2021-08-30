# Ez a script egy áttekintést készit  az általunk mért  morphologiai parameterekrol. Mindegyiknek megnézi a range-ét,
# megszámolja az NA-kat, megnézi integer tipusu-e,
# ugyanezt a CV-kkel.
# csinal egy 'range' nevu oszlopot, ahol konnyen olvashato formaban kerekitve irja a változok értékének a range-ét. Ez human-readable-format,  vagyis ábrákra való.
# csinál range-et a sejtszintű adatokra, és az well-szintű átlagokra is.
#
# Hozzáteszi a paraméterek leírását, ráadásul reguláris kifejezésekkel egyszerusiti a leirast ahol lehet, és ezt a táblázatként kimenti.
#
# létrehoz egy trait_type nevű oszlopot, aminek a lehetséges értékei:size, angle, brightness, roundness, fitness és else.  
# Ezeket a description alapján állítottem be olyan módszerrel , hogy ha pl szerepelt a descriptionben az area/diameter/distance/radius... szavak valemyike akkor 'size' típusú.
#
#
# Hozáteszi a GLM-hez használhato link fuggvenyeket és eloszlasokat, amiket a nemtudom melyik cikkben javasoltak
# 
#
# A dolog célja az volt, hogy amikor GLM-et (Generalised linearized Mixed Model) illesztek akkor könnyebben áttekintheto legyen.
#
# Author: Fekete Gergo
###############################################################################



options(device=function() {x11(title="R-GR",xpos=0, ypos=0, width=9.55,height=12)} , encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)
source("script/script-03-tryGLMM/function-my_xlsx_save.R")






#plate_ID_tbl <- read_rds(path = "data/2018_05_04-our-dataset/plate_ID_tbl.rds")

list1 <- list()
for( in_file_name in sprintf("data/preprocess_calmorph_output/calmorph_data-step1-outlier_images_filtered-data_%s.rds", c("A","A1B","C")))
{
  
  #in_file_name <- "/home/feketeg/eclipse-workspaces/ws3-forEclipseOxigen/Morphology-Karcsi-2018_01/data/preprocess_calmorph_output/calmorph_data-step1-outlier_images_filtered-data_A1B.rds"
  cat(in_file_name,"\n")
  
  # data <- read_rds(path="data/2018_05_04-our-dataset/CalMorph.raw.data.c-VG.rds")
  data <- read_rds(in_file_name)
  
  #data <-	left_join(data %>% rename(plate_filename=plateID) , plate_ID_tbl, by=c("plate_filename"="filename")) %>%  select(-plate_filename);
  
  
  
  list_of_traits<- grep("^[CDA]\\d+.+$",names(data), value = TRUE)
  
  
  wellwise_means <- data %>%  group_by(plateID, well) %>% select(plateID, well,list_of_traits) %>% summarise_all(mean, na.rm=TRUE) %>% ungroup() 
  tmp_minOfMeans <- wellwise_means %>% select(list_of_traits) %>% summarise_all(min, na.rm=TRUE) %>% gather( key="trait",value = "min_of_well_means")
  tmp_maxOfMeans <- wellwise_means %>% select(list_of_traits) %>% summarise_all(max, na.rm=TRUE) %>% gather( key="trait",value = "max_of_well_means")
  
  data1<-data %>% select(list_of_traits)
  data2<-data %>% filter(is_ok)%>% select(list_of_traits)
  
  tmp_min <- data1 %>% summarise_all(min, na.rm=TRUE) %>% gather( key="trait",value = "cellwise_min")
  tmp_max <- data1 %>% summarise_all(max, na.rm=TRUE) %>% gather( key="trait",value = "cellwise_max")
  
  tmp_min_no_outliers <- data2 %>% summarise_all(min, na.rm=TRUE) %>% gather( key="trait",value = "cellwise_min_without_outliers")
  tmp_max_no_outliers <- data2 %>% summarise_all(max, na.rm=TRUE) %>% gather( key="trait",value = "cellwise_max_without_outliers")
  
  tmp_q005 <- data1 %>% summarise_all(quantile, probs=0.005 , na.rm=TRUE) %>% gather( key="trait",value = "cellwise_quantile005")
  tmp_q995 <- data1 %>% summarise_all(quantile, probs=0.995 , na.rm=TRUE) %>% gather( key="trait",value = "cellwise_quantile995")
  tmp_ratio_of_NAs <- data %>% select(list_of_traits) %>% summarise_all(function(x) mean(is.na(x))) %>% gather( key="trait",value = "ratio_of_NAs")
  
  is_integer <- function(x) {all(is.na(x) | round(x) ==x)}
  tmp_is_integer <- data %>% select(list_of_traits) %>% summarise_all(is_integer) %>% gather( key="trait",value = "is_integer")
  
  
  tmp_r<- tmp_min %>%
    full_join(tmp_max,by = "trait")  %>%
    full_join(tmp_ratio_of_NAs,by = "trait")  %>%
    full_join(tmp_is_integer,by = "trait") %>%
    full_join(tmp_minOfMeans,by = "trait") %>%
    full_join(tmp_maxOfMeans,by = "trait") %>%
    full_join(tmp_min_no_outliers,by = "trait") %>% 
    full_join(tmp_max_no_outliers,by = "trait") %>% 
    full_join(tmp_q005,by = "trait") %>% 
    full_join(tmp_q995,by = "trait")
  
  list1[[length(list1)+1]] <- tmp_r
  
  
  rm(data1,data2)	
  ###################
  
  # 
  # tmp <- data  %>% select(well, plateID,list_of_traits) %>% group_by(well, plateID) %>% summarise_all(function(x) {sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)} )
  # tmp <- tmp %>% ungroup() 
  # tmp_min <- tmp %>% select(list_of_traits) %>% summarise_all(min, na.rm=TRUE) %>% gather( key="trait",value = "min_value")
  # tmp_max <- data %>% select(list_of_traits) %>% summarise_all(max, na.rm=TRUE) %>% gather( key="trait",value = "max_value")
  # 
  # 
  # list1[[length(list1)+1]] <- tmp_min %>% full_join(tmp_max,by = "trait")  %>% mutate(ratio_of_NAs=NA, is_integer=NA )%>%mutate(trait=gsub("(.)(.+)","\\1CV\\2",trait))
  
}

tbl1 <- do.call(bind_rows, list1)
tbl1$number_type<-"float"
tbl1$number_type[tbl1$is_integer]<-"integer"
tbl1$is_integer <-  NULL



# 
# saveRDS(tbl1, file = "data/data_actual/range_of_traits.rds")
# write_csv(tbl1, path = "data/data_actual/range_of_traits.csv")


tbl2 <- read_tsv("data-source/Ohya_parameters-utf8.txt")
tbl2$description2 <-tbl2$description
tbl2$description2 <- gsub("_", " ",tbl2$description2) 
tbl2$description2 <- gsub("[io]n nucleus A$", " ",tbl2$description2)
tbl2$description2 <- gsub("[io]n nucleus A1B$", " ",tbl2$description2)
tbl2$description2 <- gsub("[io]n nucleus C$", " ",tbl2$description2)
tbl2$description2 <- gsub("[io]n stage A$", " ",tbl2$description2)
tbl2$description2 <- gsub("[io]n stage A1B$", " ",tbl2$description2)
tbl2$description2 <- gsub("[io]n stage C$", " ",tbl2$description2)

tbl2$targeted_group <- gsub(" \\(Nucleus\\)$", "",tbl2$targeted_group)
tbl2$targeted_group <- gsub("^A1 & B$", "A1B",tbl2$targeted_group)

tbl2$is_CV <- grepl("^.CV.+$",tbl2$parameter_name)
tbl2$description2[tbl2$is_CV] <- paste0("CV of ",tbl2$description2[tbl2$is_CV])


tbl2$description <-tbl2$description2
tbl2$description2 <-NULL


tbl3 <- full_join(tbl1, tbl2, by=c("trait"="parameter_name"))

tbl3 <- tbl3 %>% arrange(is_CV, targeted_group,trait)



glm_links <- read_csv("data-source/glm_link_functions.csv" , col_types = cols(
  Names = col_character(),
  Distribution = col_character(),
  Link_function = col_character(),
  Description = col_character()
))

tbl3 <- full_join(tbl3,glm_links %>% rename(description2=Description), by=c("trait"="Names")) %>% arrange(is_CV, targeted_group,trait)

######################################################
####  RANGE                                       ####

tbl3$range <- sprintf("[%g, %g]", signif(tbl3$min_of_well_means,2), signif(tbl3$max_of_well_means,2))
tbl3$range <- gsub("\\[NA, NA\\]","",tbl3$range)

tbl3$range_cellwise <- sprintf("[%g, %g]", signif(tbl3$cellwise_min,2), signif(tbl3$cellwise_max,2))
tbl3$range_cellwise <- gsub("\\[NA, NA\\]","",tbl3$range_cellwise)

tbl3$range_cellwise <- sprintf("[%g, %g]", signif(tbl3$cellwise_min,2), signif(tbl3$cellwise_max,2))
tbl3$range_cellwise <- gsub("\\[NA, NA\\]","",tbl3$range_cellwise)

tbl3$range_cellwise_without_outliers <- sprintf("[%g, %g]", signif(tbl3$cellwise_min_without_outliers,2), signif(tbl3$cellwise_max_without_outliers,2))
tbl3$range_cellwise_without_outliers <- gsub("\\[NA, NA\\]","",tbl3$range_cellwise_without_outliers)

tbl3$range_IQR99 <- sprintf("[%g, %g]", signif(tbl3$cellwise_quantile005,2), signif(tbl3$cellwise_quantile995,2))
tbl3$range_IQR99 <- gsub("\\[NA, NA\\]","",tbl3$range_IQR99)

######################################################


######################################################
####         Associate TRAIT_TYPE              #######

tbl3<-tbl3 %>% 
  mutate(trait_type=case_when(
    grepl("area",description,ignore.case = TRUE) ~"size",
    grepl("diameter",description,ignore.case = TRUE) ~"size",
    grepl("distance",description,ignore.case = TRUE) ~"size",
    grepl("length",description,ignore.case = TRUE) ~"size",
    grepl("size",description,ignore.case = TRUE) ~"size",
    grepl("radius",description,ignore.case = TRUE) ~"size",
    
    grepl("angle",description,ignore.case = TRUE) ~"angle",
    grepl("direction",description,ignore.case = TRUE) ~"angle",
    grepl("slope",description,ignore.case = TRUE) ~"angle",
    grepl("neck position",description,ignore.case = TRUE) ~"angle",
    
    grepl("brightness",description,ignore.case = TRUE) ~"brightness",
    grepl("roundness",description,ignore.case = TRUE) ~"roundness",
    grepl("fitness",description,ignore.case = TRUE) ~"fitness",
    TRUE~"else"
  )
  )

######################################################
# Ezt a reszt toroltem, mert
# a script_01_02-convert_Karcsis_data_to_Gergo_style.R script mar torli ezeket a trait-eket , 
# hogy az outlier szures elott ki legyenek veve
# 
# ## Ezt a rész az Ohya vagy a Seagal scriptből vettem át. ezeket atrait-eket kizárták
# ##
# # Remove problematic pheontypes.  C127 and D188/9 measure distances that are usually on the order of 1 pixel
# #                                 D160, D164, D171, are not annotated phenotypes and contain many NAs
# #   A Cells - { C127_A, D188_A }
# # A1B Cells - { C127_A1B, D160_A1B, D164_A1B, D171_A1B, D188_A1B}
# #   C Cells - { C127_C, D188_C, D189_C }
# 
# msg1<- "Remove problematic pheontypes. C127 and D188/9 measure distances that are usually on the order of 1 pixel"
# msg2<-  "Remove problematic pheontypes. D160, D164, D171, are not annotated phenotypes and contain many NAs"
# 
# 
# 
# tbl3<-tbl3 %>% mutate(is_used= ! (trait %in% strsplit("C127_A,D188_A,C127_A1B,D160_A1B,D164_A1B,D171_A1B,D188_A1B,C127_C,D188_C,D189_C",split = ",")[[1]])    ) 
# 
# tbl3<-tbl3 %>% mutate(comment= case_when(grepl("C127", trait) ~ msg1,
#                                          grepl("D18[89]", trait) ~ msg1,
#                                          grepl("D16[04]", trait) ~ msg2,
#                                          grepl("D171", trait) ~ msg2,
#                                          TRUE~""))
# 

###########################################################
# 
# write_csv(tbl3, path = "data/data_actual/trait_description_tbl.csv")
# saveRDS(tbl3, file = "data/data_actual/trait_description_tbl.rds")
# xlsx::write.xlsx(tbl3, file = "data/data_actual/trait_description_tbl.xlsx", sheetName = "sheet1")
# 
# 
# cat("Done.\n")
# 
# 
# 
# #tbl3 <- read_rds("data/data_actual/trait_description_tbl.rds")
# 
# 
# #trait_description_tbl <- read_rds(path = "data/2018_05_04-our-dataset/trait_description_tbl.rds")


#Ezzel a réssezl modosítottam, hogy 2/logit helyett 1/logit-et használjon link függvénynek
# Emellett átrendeztem a táblázat oszlopait és sorait, hogy embernek könnyebb legyen olvasni


tbl3<-tbl3 %>% mutate(x=case_when(Link_function=="loga" ~ "log",
                                  Link_function=="logitb" ~"logit",
                                  Link_function=="1/logitc" ~"1/logit",
                                  Link_function=="2/logitd" ~"2/logit",
                                  Link_function=="identitye" ~"identity")) %>%
  rename(OLD_Link_function=Link_function) %>%
  rename(Link_function=x)

##################xx##################xx##################xx

exclude_tbl1<-read_rds("data/preprocess_calmorph_output/traits_width_too_many_NAs.rds")
exclude_tbl2<-read_csv(file="data-source/traits_to_exclude.csv", col_types = cols(  trait = col_character(), comment = col_character() ) )

exclude_tbl<-full_join(
  exclude_tbl1 %>% rename(comment1=comment),
  exclude_tbl2 %>% rename(comment2=comment),
  by="trait") %>%
  mutate(comment=case_when(is.na(comment1) ~ comment2,
                           is.na(comment2) ~ comment1,
                           TRUE~paste0(comment1, "; ", comment2)
  )) %>% 
  select(-comment1, -comment2) %>% 
  mutate(is_used=FALSE) 

tbl3 <- full_join(tbl3, exclude_tbl, by = "trait")

tbl3 <- tbl3 %>% mutate(comment=case_when(is.na(comment)~"", TRUE~comment))
tbl3 <- tbl3 %>% mutate(is_used=case_when(is.na(is_used)~TRUE, TRUE~is_used))

#tbl3 <- tbl3 %>% mutate(is_used = (is_used & !is.na(tbl3$cellwise_min)) ) 
idx1<- tbl3$is_used & is.na(tbl3$cellwise_min) # ilyenkor egyik mezo sincs kitoltve
tbl3$is_used[idx1]<-FALSE
tbl3$comment[idx1]<-paste("Empty column",tbl3$comment[idx1], sep = ";")


tbl3 <- tbl3 %>% mutate(is_used = (is_used & !grepl("^A.*$",trait)) )


idx1<-is.na(tbl3 $Link_function) | is.na(tbl3$Distribution)
tbl3$is_used[idx1]<-FALSE
tbl3$comment[idx1]<-paste0("No description;",tbl3$comment[idx1])

idx1<- !is.na(tbl3$trait_type) & tbl3$trait_type=="brightness"
tbl3$is_used[idx1]<-FALSE
tbl3$comment[idx1]<-paste0("Do not use brightness;",tbl3$comment[idx1])


rm(exclude_tbl, exclude_tbl1,exclude_tbl2)
##################xx##################xx##################xx

stopifnot( all(!duplicated(tbl3$trait)) )

#################################################
# drop ACTIN based traits
tbl3 <- tbl3 %>%  filter(! grepl("^A.+$",tbl3$trait) )

#######################################

tbl3 <- tbl3 %>% select(trait,
                        number_type,trait_type,
                        range, range_cellwise,
                        Distribution,Link_function, OLD_Link_function,
                        description,
                        is_used,
                        category,
                        ratio_of_NAs,
                        range_cellwise_without_outliers, range_IQR99,
                        targeted_group, is_CV, description2,
                        everything())            %>%
  mutate(is_CV2=grepl("CV",trait)) %>%
  arrange(desc(is_used),is_CV2,is.na(number_type),trait)

write_csv(tbl3,  "data/preprocess_calmorph_output/trait_description_tbl.csv")
write_rds(tbl3,  "data/preprocess_calmorph_output/trait_description_tbl.rds", compress = "gz")
my_xlsx_save(tbl = tbl3,file="data/preprocess_calmorph_output/morphology_tbls.xlsx", sheet = "trait_description_tbl")


cat("Done.\n")

