# A smaller_big_tbl.rds file már nem tartalmazza Cgroup=complex sorokatat,
# az outlier jpg képekhez tartozó sorokat
# és a grafikus path-okat tratalmazó oszlopokat. 
# 
# Ezt a nagy táblázatot szetdi szét a program 3 részre (A, A1B, és C),
#úgy hogy az oszlopokról edönti hogy melyik táblázatba kellenek, és suffixként hozzárakja A,A1B vagy C-t az oszlopnévhez

graphics.off()

rm(list=ls())
gc()

library(tidyverse)
source("script/script-03-tryGLMM/function-my_xlsx_save.R")
####################################x####################################x####################################x

big_tbl<- read_rds("data/preprocess_calmorph_output/smaller_big_tbl.rds")

# grep("^[CD]\\d+(-\\d+)?$",names(big_tbl), value = TRUE)
# 
# names(big_tbl)[!grepl("^[CD]\\d+(-\\d+)?$",names(big_tbl))]

#cat('"',paste(names(big_tbl)[!grepl("^[CD]\\d+(-\\d+)?$",names(big_tbl))], collapse = '",\n"'),'"', sep="")


header_names<-c("plateID",
                "well",
                "genotypeID",
                "image_number",
                "Cgroup",
                "Dgroup",
                "cell_id",
                "is_ok",
                "is_marked_for_deletion_by_quality_control",
                "is_marked_for_deletion_by_outlier_filter",
                # "mother_cell_size",
                # "bud_cell_size",
                # "bud_ratio",
                # "axis_ratio_in_mother",
                # "axis_ratio_in_bud",
                # "bud_direction",
                # "neck_position",
                # "neck_width",
                # "nuclear_number",
                # "nuclear_size_in_mother",
                # "nuclear_size_in_bud",
                # "nuclear_size_in_cell",
                # "nuclear_axis_ratio_in_mother",
                # "nuclear_axis_ratio_in_bud",
                # "nuclear_axis_ratio_in_cell",
                # "hip_nuclear",
                # "mother_cell_center_nuclear",
                # "neck_nuclear_in_mother",
                # "bud_top_nuclear",
                # "bud_cell_center_nuclear",
                # "neck_nuclear_in_bud",
                # "length_between_nucleus",
                "x1",
                "y1",
                "x2",
                "y2")


trait_names<-grep("^[CD]\\d+(-\\d+)?$",names(big_tbl), value = TRUE)




tbl1<-big_tbl %>%
  mutate(Dgroup2= ifelse(Dgroup=="A1" | Dgroup=="B" ,yes = "A1B",Dgroup)) %>% 
  group_by(Dgroup2) %>% summarise_at(.vars = trait_names, .funs = function(x) mean(is.na(x)) )

tbl_of_traits<-tbl1 %>%
  gather(value = "na_ratio", key="trait", -Dgroup2) %>% 
  filter(na_ratio<0.99) %>% 
  mutate(trait_name=paste0(trait,"_",Dgroup2))

#unique(tbl_of_traits$Dgroup2)

tblX<-tribble(~suffix, ~Dgroup2, ~regexp,
              "a","A", "^A$",
              "b","A1B","^(A1)?(B)?$",
              "c","C", "^C$")
for(i in 1:nrow(tblX))
{
  r<-tblX[i,]
  
  
  cat(i,":" , r$Dgroup2,"\n")
  
  tbl_of_traits2<-tbl_of_traits %>%  filter( Dgroup2==r$Dgroup2) 
  name_list1<-c(header_names, tbl_of_traits2$trait)
  name_list2<-c(header_names, tbl_of_traits2$trait_name)
  
  tmp<-big_tbl %>%  filter(grepl(r$regexp, Dgroup)) %>%   select(name_list1)
  names(tmp)<-name_list2
  
  write_rds(tmp,  paste0("data/preprocess_calmorph_output/calmorph_data-step1-outlier_images_filtered-data_",r$Dgroup2,".rds"), compress = "gz")
}  



cat("Done.\n")

