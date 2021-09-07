# Ez nem része a fő dolgoknak, csak az adatokra lehet vetni a scriptel egy pillantást.
# Egy konkrét well 2×52 mikroszkopos fotojat és azok hisztogramjait teszi ki egy osszefogalo pdf-be.


graphics.off()

rm(list=ls())

library(tidyverse)
library(cowplot)




####################################x####################################x####################################x

plates_tbl<-xlsx::read.xlsx(file = "/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/DATA_of_microscope/plates_tbl.xlsx",sheetIndex = 1,stringsAsFactors=FALSE) %>%
  as_tibble()  %>% 
  mutate_at(.vars=grep("use_",names(. ), ignore.case = TRUE,value =TRUE), .funs = as.logical) %>% 
  filter(!is.na(filename))

stopifnot(!any(duplicated( plates_tbl$plateID)))
stopifnot(!any(duplicated( plates_tbl$filename)))

####################################x####################################x####################################x

dir1<-"/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/DATA_of_microscope/02-Operetta_images/c_cmax_4000/170623_morphology_plate5_m13__2017-06-23T11_18_49-Measurement1/D03/"

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

out_dir1<-"out/2019_12_04-images/";
dir.create(out_dir1)

pdf(file = paste0(out_dir1, "hist.pdf"),width = 16, height = 3)
file_tbl1<-tibble(filename=list.files(dir1))
file_tbl1<-file_tbl1 %>%  filter(grepl("...-C\\d+\\.jpg", filename) );
file_tbl1<-file_tbl1 %>%  mutate(filanameD=gsub("(...-)C(\\d+\\.jpg)","\\1D\\2", filename) );
file_tbl1<-file_tbl1 %>%  mutate(file_index= as.integer(gsub("...-C(\\d+)\\.jpg","\\1", filename)) ) %>%  arrange(file_index);
for(i  in 1:nrow(file_tbl1))
{
  cat(i ," - ", file_tbl1$filename[[i]],"\n")
  fileC<-paste0(dir1,file_tbl1$filename[[i]])
  fileD<-paste0(dir1,file_tbl1$filanameD[[i]])
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
    
  
  fig<-ggdraw()+
    draw_plot(plotC, x=0, width = 0.59, height = 0.5)+
    draw_text("conA",x = 0.58,y = 0.5, hjust = 1, vjust = 1,size=10, color="darkgreen")+
    draw_plot(plotD, x=0, width = 0.59, height = 0.5, y=0.5)+
    draw_text("dapi",x = 0.58,y = 0.99, hjust = 1, vjust = 1,size=10, color="darkblue")+
    draw_image(image = fileC,x = 0.8, width = 0.2)+
    draw_text("conA",x = 0.99,y = 0.99, hjust = 1, vjust = 1,size=10, color="darkgreen")+
    draw_image(image = fileD,x = 0.59, width = 0.2)+
    draw_text("dapi",x = 0.79,y = 0.99, hjust = 1, vjust = 1,size=10, color="darkblue")+
    draw_text(gsub("(.*)\\.jpg","\\1",file_tbl1$filename[[i]]),x = 0.5,y = 0.99, hjust = 0.5, vjust = 1,size=14, color="darkgreen")
    
  
  
  print(fig)
  
  
}
dev.off()

#############
