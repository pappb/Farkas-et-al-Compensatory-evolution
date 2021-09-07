
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

plate_map_tbl<-read_rds("data/preprocess_calmorph_output/plate_map_tbl.rds")


outlier_filter_which_traits_are_failed <- read_rds("data/preprocess_calmorph_output/outlier_filter_which_traits_are_failed.rds")

big_tbl <- readRDS("data/preprocess_calmorph_output/smaller_big_tbl.rds")

big_tbl <- big_tbl %>%
  #semi_join(outlier_filter_which_traits_are_failed,by=c("plateID", "well", "genotypeID",  "cell_id")) %>% 
  semi_join(plate_map_tbl %>% filter(use_for_paper  ),by=c("plateID", "well") )
#rm(big_tbl)
#####################

data0<-read_rds("data/data_actual/data0.rds")

out_dir="out/2020_01_24-outlier_filter-heatmap/"
dir.create(out_dir)



#########################################################################################

trait_list<-grep("^[CD]\\d+(-\\d+)?$",names(outlier_filter_which_traits_are_failed), value = TRUE) 

lft1<-big_tbl %>% select( plateID,well, genotypeID,  cell_id, trait_list) %>% 
  gather(key="trait", value="value",-plateID,-well, -genotypeID,  -cell_id)

lft2<-outlier_filter_which_traits_are_failed  %>% 
  select( plateID,well, genotypeID,  cell_id, trait_list) %>% 
  gather(key="trait", value="is_outlier",-plateID,-well, -genotypeID,  -cell_id)

lft<-left_join(lft1, lft2, by = c("plateID", "well", "genotypeID", "cell_id", "trait"))
rm(lft1, lft2)

sum_tbl<-lft %>%  group_by(plateID,genotypeID, trait) %>%  summarise(outlier_cnt=sum(is_outlier, na.rm=TRUE), n=n(),outlier_ratio=outlier_cnt/n )
sum_tbl<-sum_tbl %>%  left_join(plate_map_tbl%>% select(genotypeID, genotype_label) %>%  distinct())
rm(lft)
gc(reset = TRUE,full = TRUE)

sum_tbl %>% ggplot(aes(y=trait, x=genotype_label, fill=outlier_ratio))+
  geom_raster()+
  theme(axis.title = element_blank(), axis.text.x = element_text(angle=90, size=6, hjust = 1, vjust=0.5), axis.text.y = element_text(size=6))

ggsave(filename = paste0(out_dir,"heatmap.pdf"), width = 1.3*16, height = 1.3*9)

trait_list<-sort(unique(sum_tbl$trait))
for(i in seq(trait_list))
{
  trait1<-trait_list[[i]]

  plot1<-
    sum_tbl %>% filter(trait==trait1)  %>%
    ggplot(aes(y=outlier_ratio,x=genotype_label, label=outlier_cnt))+
    geom_bar(stat = "identity")+
    geom_text(hjust=0.5, vjust=1, color="gray90")+
    scale_y_continuous(breaks = seq(0,1,by=0.002) , labels = sprintf("%0.1f%%", 100*seq(0,1,by=0.002)))+
    facet_wrap(~plateID, scales = "free_x",ncol = 5)+
    theme(axis.text.x = element_text(angle=90, vjust=0.5,hjust=1, size = 7))+
    theme(axis.title.x = element_blank())+
    labs(caption = "Az egyes genotipusokban mennyire gyulnek fel az outlierek. Az a jó, ha kb azonos mennyisegben")

  parameter_fig1<- data0$get_parameter_figure(paste0(trait1,"_C"))

  combi_plot<-ggdraw()+draw_plot(parameter_fig1, y=0.9,height = 0.1)+
    draw_plot(plot1, y=0, height = 0.9)
  ggsave(filename = paste0(out_dir,"barplot-",trait1,"-outlier_cnt_aggregated_by_genotype.pdf"),plot = combi_plot,width = 1.3*16, height = 4*1.3*9)



}



#####################################
#####################################
#####################################
#####################################



outlyer_tresholds_tbl <- readRDS("data/preprocess_calmorph_output/outlyer_tresholds_tbl.rds") %>% ungroup()

outlier_filter_which_traits_are_failed <- read_rds("data/preprocess_calmorph_output/outlier_filter_which_traits_are_failed.rds")

#big_tbl <- readRDS("data/preprocess_calmorph_output/smaller_big_tbl.rds")
big_tbl<-big_tbl %>% mutate(Dgroup2=ifelse(Dgroup=="A1"|Dgroup=="B" , "A1B",Dgroup))


big_tbl2 <- big_tbl %>%
  semi_join(outlier_filter_which_traits_are_failed,by=c("plateID", "well", "genotypeID",  "cell_id")) %>% 
  semi_join(plate_map_tbl %>% filter(use_for_paper  ),by=c("plateID", "well") )
#rm(big_tbl)
#####################

#data0<-read_rds("/home/feketeg/eclipse-workspaces/ws3-forEclipseOxigen/Morphology-Karcsi-2018_01/data/data_actual/data0.rds")

out_dir_cell_photos <- paste0(out_dir,"cell_photos/")
dir.create(out_dir_cell_photos)



#########################################################################################

tbl1<-outlyer_tresholds_tbl %>%  select(trait, Dgroup) %>% distinct()

for(i in 1:nrow(tbl1))
{
  r1<-tbl1[i,]
  
  filter_tbl1<-outlier_filter_which_traits_are_failed %>%
    filter(Dgroup==r1$Dgroup & !!(as.name(r1$trait))) %>% 
    select(plateID,well,cell_id)
    
  tbl_of_cells<- big_tbl2 %>% 
    semi_join(filter_tbl1, by = c("plateID","well","cell_id"))
  
  
  if(nrow(tbl_of_cells)>100)
  {
    tbl_of_cells<-tbl_of_cells %>%   sample_n(size=100, replace = FALSE) 
  }
  
  #tbl_of_cells<-tbl_of_cells %>%   arrange(plateID, genotypeID, well,image_number)
  tbl_of_cells<-tbl_of_cells %>%   arrange(!!(as.name(r1$trait)))
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
  
  
  
  pdf(file = paste0(out_dir_cell_photos,"cell_images-",r1$trait,"_", r1$Dgroup,"-random_collection_of_300_outliers-detealed.pdf"),width = 16, height = 9)
  
  
  RN<-2
  CN<-2
  
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
          outlier_flags<-names(outlier_flags)
          
          flag_tbl<-tibble(trait0=outlier_flags, value=unlist(row1[1,outlier_flags]))
          flag_tbl<-flag_tbl %>%
            mutate(trait=paste0(trait0,"_",row1$Dgroup2)) %>%
            left_join(data0$trait_description_tbl, by = "trait") %>% 
            mutate(label=paste0(trait0,"[",signif(value,4),"] ", description ))
          #flag_tbl$label
          
          draw_of_density_plots<-ggdraw()
          tbl_of_current_genotype<-big_tbl %>% semi_join(row1,  by = c("plateID",  "genotypeID","Dgroup2"))
          NT<-nrow(flag_tbl);
          for( i_t in 1:NT)
          {
            # i_t<-1
            trait1<-flag_tbl$trait0[[i_t]]
            value1<-unlist(row1[ trait1 ]) 
            
            outlyer_tresholds1<-outlyer_tresholds_tbl %>%
              semi_join(row1, by = c("plateID", "genotypeID", "Dgroup"="Dgroup2")) %>% 
              filter(trait==trait1)
            
            
            plot_density_1<-
              tbl_of_current_genotype %>%
              ggplot(aes( x=!!(as.name(trait1)) ) ) +
              geom_vline(xintercept = c(outlyer_tresholds1$median1,outlyer_tresholds1$q_ca15, outlyer_tresholds1$q_ca85) , color="gray")+
              geom_vline(xintercept = c(outlyer_tresholds1$treshold_low, outlyer_tresholds1$treshold_high) , color="gray20")+
              geom_density()+
              geom_vline(xintercept =value1 , color="red")+
              theme(axis.title = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(0,0,0,0),units = "mm"))
            
            if(NT>10){
              plot_density_1<-plot_density_1+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
            }
            
            draw_of_density_plots<-draw_of_density_plots+
              draw_plot(plot_density_1,x=0, width = 1, y=(i_t-1)/NT, height = 0.95/NT)+
              draw_label(label = flag_tbl$label[[i_t]],x=.99,  y=(i_t-0.01)/NT, hjust = 1, vjust = 1, size=9, color="red2")
            
          }
          
          
          res1<-get_calmorph_cell_images(cell_data_row = row1,  dir_of_calmorph, plates_tbl=plates_tbl, R=50  )
          img1<- 
            cowplot::ggdraw() + 
            cowplot::draw_line(x=c(0,1,1,0,0), y=c(0,0,1,1,0))+
            cowplot::draw_image(res1$raster_image_conA,x = 0,width = 0.33, y=0.46, height = 0.45,scale = 1)+
            cowplot::draw_image(res1$raster_image_dapi,x = 0,width = 0.33, y=0.0, height = 0.45,scale = 1)+
            cowplot::draw_plot(draw_of_density_plots,x = 0.33,width = 0.66, y=0.0, height = 1)+
            cowplot::draw_label(label = paste0(genotype_label," (",row1$Dgroup,")\n"),x=0.05,y=0.98,hjust = 0,vjust = 1, size=12, color="red2",fontface="bold")
          
          my_draw<-my_draw+draw_plot(img1,x=(i_c-1)/CN, width = 0.98/CN, y=(i_r-1)/RN, height = 0.9/RN)
        }
      }
    }
    print(my_draw)
  }
  dev.off()
  
}# for over trait+Dgroup

######################x######################x######################x######################x
######################x######################x######################x######################x




