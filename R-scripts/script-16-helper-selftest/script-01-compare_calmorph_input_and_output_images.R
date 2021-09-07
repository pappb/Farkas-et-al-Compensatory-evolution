# Ez a script arra valo, hogy ellenorizze, hogy a calmorph futtataskor
# nem keveredtek-e ossze a platek.
#
# Minden plateről vesz egy mikroszkópos kép pozíciót (well+kep_sorszam), 
# és a calmorph ki és bemeneti képet egymás mellé teszi,
# és csinál egy diff-ezt is róluk. 
# Ha nem passzolnak a képek, akkor a diff-en ez nagyon meglátszik,
# könnyű szemmel kiszúrni.
# Az egészet kimenti egy pdf-be, és át kell nézni, hogy passzolnak-e a képek.


options( encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)
library(cowplot)

out_dir<-"out/2021_07_06-compare_calmorph_in_and_out_images/"
dir.create(out_dir, recursive = TRUE)

source("script/script-08-preprocess_calmorph_output/my_parameters.R") # load the dir locations and parameters


dir2<-params$direcory_of_jpg_images_from_microscoope
dir3<-params$direcory_of_calmorph_output

subdir2<-list.dirs(dir2,recursive = FALSE,full.names = FALSE)
subdir3<-list.dirs(dir3,recursive = FALSE,full.names = FALSE)

setdiff(subdir3, subdir2)
setdiff(subdir2, subdir3)

subdir_common<-intersect(subdir2,subdir3)

pdf(file = paste0(out_dir,"compare.pdf"),width = 2*16,height = 2*9)
for(i   in seq(subdir_common)){
  currend_subdir<-subdir_common[[i]]
  cat(currend_subdir,"\n")
  
  path2<-paste0(dir2,currend_subdir,"/C07/C07-C5.jpg")
  path3<-paste0(dir3,currend_subdir,"/C07/C07-conA5.jpg")
  if(!file.exists(path2) || !file.exists(path3)){
    path2<-paste0(dir2,currend_subdir,"/C07/C07-C7.jpg")
    path3<-paste0(dir3,currend_subdir,"/C07/C07-conA7.jpg")
  }
  if(!file.exists(path2) || !file.exists(path3)){
    path2<-paste0(dir2,currend_subdir,"/C07/C07-C2.jpg")
    path3<-paste0(dir3,currend_subdir,"/C07/C07-conA2.jpg")
  }
  if(!file.exists(path2) || !file.exists(path3)){
    path2<-paste0(dir2,currend_subdir,"/C09/C09-C2.jpg")
    path3<-paste0(dir3,currend_subdir,"/C09/C09-conA2.jpg")
  }
  if(!file.exists(path2) || !file.exists(path3)){
    path2<-paste0(dir2,currend_subdir,"/C09/D08-C10.jpg")
    path3<-paste0(dir3,currend_subdir,"/C09/D08-conA10.jpg")
  }
  
  
  raster_image_2<-jpeg::readJPEG(source = path2)
  raster_image_3<-jpeg::readJPEG(source = path3)
  
  diff_image<-raster_image_3
  diff_image[,,1]<-abs(diff_image[,,1]-raster_image_2)
  diff_image[,,2]<-abs(diff_image[,,2]-raster_image_2)
  diff_image[,,3]<-abs(diff_image[,,3]-raster_image_2)
  
  
  fig1<-ggdraw()+
    # draw_plot(plotC, x=0, width = 0.59, height = 0.5)+
    # draw_text("conA",x = 0.30,y = 0.47, hjust = 1, vjust = 1,size=10, color="darkgreen")+
    #draw_plot(plotD, x=0, width = 0.59, height = 0.5, y=0.5)+
    draw_text(currend_subdir,x = 0.5,y = 0.97, hjust = 0.5, vjust = 1,size=35, color="darkblue")+
    draw_image(image = path2,x = 0.02, width = 0.30)+
    draw_image(image = path3,x = 0.65, width = 0.30)+
    draw_image(image = diff_image,x = 0.34, width = 0.30)
  
  print(fig1)
}

dev.off()
