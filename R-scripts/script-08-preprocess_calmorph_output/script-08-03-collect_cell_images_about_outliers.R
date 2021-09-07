
# Author: Fekete Gergo
###############################################################################



options(device=function() {x11(title="R-GR",xpos=0, ypos=0, width=9.55,height=12)} , encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)
library(cowplot)

source("script/script-03-tryGLMM/function-my_xlsx_save.R")
source("script/script-04-preprocess_calmorph_output/function-find_a_cell_image-v2.R")
####################################x####################################x####################################x



plates_tbl<-xlsx::read.xlsx(file = "/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/DATA_of_microscope/plates_tbl.xlsx",sheetIndex = 1,stringsAsFactors=FALSE) %>%
  as_tibble()  %>% 
  mutate_at(.vars=grep("use_",names(. ), ignore.case = TRUE,value =TRUE), .funs = as.logical) %>% 
  filter(!is.na(filename))

plate_map_tbl<-read_rds( "data/preprocess_calmorph_output/plate_map_tbl.rds")


outlier_filter_which_traits_are_failed <- read_rds("data/preprocess_calmorph_output/outlier_filter_which_traits_are_failed.rds")
 
big_tbl <- readRDS("data/preprocess_calmorph_output/smaller_big_tbl.rds")

big_tbl2 <- big_tbl %>%
  semi_join(outlier_filter_which_traits_are_failed,by=c("plateID", "well", "genotypeID",  "cell_id")) %>% 
  semi_join(plate_map_tbl %>% filter(use_for_paper  ),by=c("plateID", "well") )
rm(big_tbl)
#####################

 data0<-read_rds("/home/feketeg/eclipse-workspaces/ws3-forEclipseOxigen/Morphology-Karcsi-2018_01/data/data_actual/data0.rds")

out_dir="out/2020_01_23-outlier_cell_images/"
dir.create(out_dir)



#########################################################################################

tbl_of_cells<- big_tbl2 %>% 
  sample_n(size=2000, replace = FALSE) %>%
  arrange(plateID, genotypeID, well,image_number)

# tbl_of_cells<-tibble()
# range1<-c(seq(0,10, by=0.2 ), +Inf)
# for( i in 2:length(range1))
# {
#   tmp<-data %>% 
#     filter(C108_C <60) %>% 
#     filter( D182_C>=range1[[i-1]] & D182_C<range1[[i]]) 
#   if(nrow(tmp)>30){
#     tmp<-tmp %>% sample_n(replace = FALSE,size	=30)
#   }
#   tbl_of_cells<-bind_rows(tbl_of_cells ,tmp)
# }
# tbl_of_cells<-tbl_of_cells %>% arrange(D182_C) 

dir_of_calmorph<-"/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/DATA_of_microscope/03-calmorph/"



pdf(file = paste0(out_dir,"outlier_cells_random_collection.pdf"),width = 16, height = 9)


RN<-2
CN<-3

k=0
while(k < nrow(tbl_of_cells)) {
  my_draw<-ggdraw()
  for(i_r in 1:RN)
  {
    for(i_c in 1:CN){
      
      if(k < nrow(tbl_of_cells) ) {
        k<-k+1
        
        row1<-  tbl_of_cells[k,]
        row1<-row1 %>% mutate(Dgroup2=ifelse(Dgroup=="A1"|Dgroup=="B" , "A1B",Dgroup))
        
        genotype_label<-plate_map_tbl %>%  semi_join(row1 , by = c("plateID", "well", "genotypeID")) %>% .$genotype_label
        outlier_flag_tbl<-outlier_filter_which_traits_are_failed %>%  semi_join(row1 , c("plateID", "well",  "cell_id"))
        outlier_flags<-outlier_flag_tbl %>% select(-plateID,-well,-genotypeID,-Dgroup,-cell_id) %>% as.matrix() 
        outlier_flags<-outlier_flags[1,]
        outlier_flags<-outlier_flags[outlier_flags & !is.na(outlier_flags)]
        outlier_flags_string<-paste0(names(outlier_flags), collapse = ",")
        
        flag_tbl<-tibble(trait0=names(outlier_flags), value=unlist(row1[1,names(outlier_flags)]))
        flag_tbl<-flag_tbl %>%
          mutate(trait=paste0(trait0,"_",row1$Dgroup2)) %>%
          left_join(data0$trait_description_tbl, by = "trait") %>% 
          mutate(label=paste0(trait0,"[",signif(value,4),"] ", description ))
        #flag_tbl$label
        
        res1<-get_calmorph_cell_images(cell_data_row = row1,  dir_of_calmorph, plates_tbl=plates_tbl, R=50  )
        img1<- 
          cowplot::ggdraw() + 
          cowplot::draw_image(res1$raster_image_conA,x = 0,width = 0.48, y=0.4, height = 0.6,scale = 1)+
          cowplot::draw_image(res1$raster_image_dapi,x = 0.5,width = 0.48, y=0.4,height = 0.6,scale = 1)+
          cowplot::draw_label(label = paste0(genotype_label," (",row1$Dgroup,")\n"),x=0.05,y=0.39,hjust = 0,vjust = 1, size=11)+
          cowplot::draw_label(label = paste(flag_tbl$label, collapse = "\n"),x=0.05,y=0.35,hjust = 0,vjust = 1, size=9)
        
        my_draw<-my_draw+draw_plot(img1,x=(i_c-1)/CN, width = 0.98/CN, y=(i_r-1)/RN, height = 0.9/RN)
      }
    }
  }
  print(my_draw)
}
dev.off()


######################x######################x######################x######################x
######################x######################x######################x######################x




