# Kijelöltunk mar egy csomo outlier kepet. ez a sript keszit egy bazi nagy pdf-et, amiben at lehet nezni az outliernek jelolt dolgokat, 


graphics.off()

rm(list=ls())

library(tidyverse)
library(cowplot)

##########
source("R-scripts/preprocess_calmorph_output/my_parameters.R") # load the dir locations and parameters



####################################x####################################x####################################x



screen_metadata_tbl<-read_csv(file = "data/preprocess_calmorph_output/YMP_tables/YMP_screen_metadata.csv",col_types = cols(
  .default = col_character(),
  use_screen = col_logical(),
  screen_repeat = col_logical(),
  fixation = col_date(format = "%Y%m%d"),
  Alexa44_staining_date = col_date(format = "%Y%m%d"),
  Alexa44_staining_volume_ul = col_double(),
  DAPI_staining_screen = col_date(format = "%Y%m%d"),
  Alexa488_stock_date = col_date(format = "%Y%m%d"),
  Alexa488_exp_time_ms = col_double(),
  DAPI_stock_date = col_date(format = "%Y%m%d"),
  DAPI_exp_time_ms = col_double(),
  layout = col_logical()
))

screen_metadata_tbl<-screen_metadata_tbl %>% filter(use_screen)
stopifnot(!any(duplicated( screen_metadata_tbl$YMP_folder_name             )))
stopifnot(!any(duplicated( screen_metadata_tbl$YMP_exp_name)))

####################################x####################################x####################################x

#dir1<-"/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/Operetta_images/c_cmax_4000/170623_morphology_plate5_m13__2017-06-23T11_18_49-Measurement1/D03/"

#dir0<-"/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/DATA_of_microscope/02-Operetta_images/c_cmax_4000/"
dir0<-params$direcory_of_jpg_images_from_microscoope
out_data_dir0<-"data/preprocess_calmorph_output/"

tbl_of_outlier_images<- read_rds(  paste0(out_data_dir0,"tbl_of_outlier_images.rds"))
cat("A kepek ekkora resze van outliernek jelolve: ",mean(!tbl_of_outlier_images$ok),"\n",sep="")
tbl_of_outlier_images<-tbl_of_outlier_images %>% filter(!ok)

out_dir1<-"out/2020_08_12-outlier_filter-images-newAssay/";
dir.create(out_dir1)

# 
# 
# file1<-paste0(dir1,"B11-C16.jpg")
# raster_image_dapi<-jpeg::readJPEG(source = file1)
# 
# tmp<-as.vector(raster_image_dapi)
# tmp<-tmp[tmp!=0]
# 
# tbl1<-tibble(value=tmp)
# tbl1 %>% ggplot(aes(x=value))+geom_histogram(breaks=seq(0,1,by=1/256)) + coord_cartesian(ylim=c(0,6000))
# 
# 
list_of_plates<-tbl_of_outlier_images %>%
  select(plateID) %>% 
  distinct() %>%
  semi_join(screen_metadata_tbl, by=c("plateID"="YMP_plate_ID")) %>% 
  .$plateID

for(plate1 in list_of_plates)
{
  pdf(file = paste0(out_dir1, plate1,".pdf"),width = 16, height = 3)
  
  
  tbl_outlier_of_a_plate<- tbl_of_outlier_images %>%  filter(plateID==plate1) %>%  arrange(well,  image_number)
  
  for (i1 in 1:nrow(tbl_outlier_of_a_plate))
  {
    r<-tbl_outlier_of_a_plate[i1,]
    r2<-screen_metadata_tbl %>% filter(YMP_plate_ID==r$plateID)
    fileC<-paste0(dir0, r2$YMP_folder_name,"/", r$well,"/",r$well,"-C",r$image_number,".jpg")
    fileD<-paste0(dir0, r2$YMP_folder_name,"/", r$well,"/",r$well,"-D",r$image_number,".jpg")
    
    cat(i1 ," - ", r$plateID, "-", r$well,"-", r$image_number,"\n")
    raster_image_conA<-jpeg::readJPEG(source = fileC)
    raster_image_dapi<-jpeg::readJPEG(source = fileD)
    
    
    
    
    tbl1<-tibble(value=as.vector(raster_image_conA))
    plotC<-tbl1 %>% ggplot(aes(x=value))+
      geom_histogram(breaks=seq(0,1,by=1/256), fill="darkgreen")   + 
      coord_cartesian(ylim=c(0,6000), xlim = c(0,1))+
      theme(axis.title = element_blank())
    
    tbl2<-tibble(value=as.vector(raster_image_dapi))
    plotD<-tbl2 %>% ggplot(aes(x=value))+
      geom_histogram(breaks=seq(0,1,by=1/256), fill="darkblue")   + 
      coord_cartesian(ylim=c(0,6000), xlim = c(0,1))+
      theme(axis.title = element_blank())
    
    msg1<-paste0(r$plateID, " - ", r$well," - image:", r$image_number)
    tmp<-r %>% select_at(.vars = grep("too", names(r), value=TRUE)) %>% as_vector()
    msg2<-paste(names(tmp)[tmp], collapse = "\n")
    
    fig<-ggdraw()+
      draw_plot(plotC, x=0, width = 0.59, height = 0.5)+
      draw_text("conA",x = 0.30,y = 0.47, hjust = 1, vjust = 1,size=10, color="darkgreen")+
      draw_plot(plotD, x=0, width = 0.59, height = 0.5, y=0.5)+
      draw_text("dapi",x = 0.30,y = 0.97, hjust = 1, vjust = 1,size=10, color="darkblue")+
      draw_image(image = fileC,x = 0.8, width = 0.2)+
      draw_text("conA",x = 0.99,y = 0.99, hjust = 1, vjust = 1,size=10, color="darkgreen")+
      draw_image(image = fileD,x = 0.59, width = 0.2)+
      draw_text("dapi",x = 0.79,y = 0.99, hjust = 1, vjust = 1,size=10, color="darkblue")+
      draw_text(msg1,x = 0.5,y = 0.99, hjust = 0.5, vjust = 1,size=14, color="black")+
      draw_text(paste0("\n",msg2),x = 0.5,y = 0.99, hjust = 0.5, vjust = 1,size=12, color="red")
    
    
    
    
    print(fig)
    
    
  }
  dev.off()
}

# 
# pdf(file = paste0(out_dir1, "hist.pdf"),width = 16, height = 3)
# file_tbl1<-tibble(filename=list.files(dir1))
# file_tbl1<-file_tbl1 %>%  filter(grepl("...-C\\d+\\.jpg", filename) );
# file_tbl1<-file_tbl1 %>%  mutate(filanameD=gsub("(...-)C(\\d+\\.jpg)","\\1D\\2", filename) );
# file_tbl1<-file_tbl1 %>%  mutate(file_index= as.integer(gsub("...-C(\\d+)\\.jpg","\\1", filename)) ) %>%  arrange(file_index);
# for(i  in 1:nrow(file_tbl1))
# {
#   cat(i ," - ", file_tbl1$filename[[i]],"\n")
#   fileC<-paste0(dir1,file_tbl1$filename[[i]])
#   fileD<-paste0(dir1,file_tbl1$filanameD[[i]])
#   raster_image_conA<-jpeg::readJPEG(source = fileC)
#   raster_image_dapi<-jpeg::readJPEG(source = fileD)
#   
#   
#   
#   
#   tbl1<-tibble(value=as.vector(raster_image_conA))
#   plotC<-tbl1 %>% ggplot(aes(x=value))+
#     geom_histogram(breaks=seq(0,1,by=1/256), fill="darkgreen")   + 
#     coord_cartesian(ylim=c(0,6000), xlim = c(0,1))+
#     theme(axis.title = element_blank())
#   
#   tbl2<-tibble(value=as.vector(raster_image_dapi))
#   plotD<-tbl2 %>% ggplot(aes(x=value))+
#     geom_histogram(breaks=seq(0,1,by=1/256), fill="darkblue")   + 
#     coord_cartesian(ylim=c(0,6000), xlim = c(0,1))+
#     theme(axis.title = element_blank())
#   
#   
#   fig<-ggdraw()+
#     draw_plot(plotC, x=0, width = 0.59, height = 0.5)+
#     draw_text("conA",x = 0.58,y = 0.5, hjust = 1, vjust = 1,size=10, color="darkgreen")+
#     draw_plot(plotD, x=0, width = 0.59, height = 0.5, y=0.5)+
#     draw_text("dapi",x = 0.58,y = 0.99, hjust = 1, vjust = 1,size=10, color="darkblue")+
#     draw_image(image = fileC,x = 0.8, width = 0.2)+
#     draw_text("conA",x = 0.99,y = 0.99, hjust = 1, vjust = 1,size=10, color="darkgreen")+
#     draw_image(image = fileD,x = 0.59, width = 0.2)+
#     draw_text("dapi",x = 0.79,y = 0.99, hjust = 1, vjust = 1,size=10, color="darkblue")+
#     draw_text(gsub("(.*)\\.jpg","\\1",file_tbl1$filename[[i]]),x = 0.5,y = 0.99, hjust = 0.5, vjust = 1,size=14, color="darkgreen")
#   
#   
#   
#   print(fig)
#   
#   
# }
# dev.off()
# 
# #############
