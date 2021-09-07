
# graphics.off()
# 
# rm(list=ls())

library(tidyverse)
#library(cowplot)
library(jpeg)



# A rasterImg egy H×W×3 méretu array az RGB ertekekkel
# A position egy lista, amiben keres x1,x2,y1,y2 értékeket, amik egy doboz-t határoznak meg. Nem ezt a dobozt vágja ki,
# hanem egy olat, akinek a közepe a megadott doboz közepen van, es kocka alaku, es 2×R oldalhosszusagu
crop_from_raster_image<-function(rasterImg, position,R=55)
{
  row1<-position
  y_center<-round((row1$y1+row1$y2)/2)
  x_center<-round((row1$x1+row1$x2)/2)
  x1<-x_center-R
  
  r2<-rasterImg
  #dim(r1)
  if(x1>0)
  {r2<-r2[,x1:(dim(r2)[2]),]
  }else{
    gap1<- 1-x1
    tmp<-array(data = 0.5, dim = c(dim(r2)[1],gap1,3))
    r2<-abind::abind(tmp,r2,along = 2)
  }
  
  if((dim(r2)[2])>=2*R )
  {
    r2<-r2[,1:(2*R),]
  }else{
    gap1<-2*R-(dim(r2)[2])
    tmp<-array(data = 0.5, dim = c(dim(r2)[1],gap1,3))
    r2<-abind::abind(r2,tmp,along = 2)
    
  }
  
  y1<-y_center-R
  dim(r2)
  if(y1>0)
  {r2<-r2[y1:(dim(r2)[1]),,]
  }else{
    gap1<- 1-y1
    tmp<-array(data = 0.5, dim = c(gap1,dim(r2)[2],3))
    r2<-abind::abind(tmp,r2,along = 1)
  }
  
  
  if((dim(r2)[1])>=2*R )
  {
    r2<-r2[1:(2*R),,]
  }else{
    gap1<-2*R-(dim(r2)[1])
    tmp<-array(data = 0.5, dim = c(gap1,dim(r2)[2],3))
    r2<-abind::abind(r2,tmp,along = 1)
  }
  return(r2)
}

############################################x


# ugyanaz mint az elozo fuggven, de nem 3 dimenzios array-on mukodik, hanem 2 dimenzioson.
# nem RGB kep hanem grayscale
crop_from_gray_raster_image<-function(rasterImg, position,R=55)
{
  row1<-position
  y_center<-round((row1$y1+row1$y2)/2)
  x_center<-round((row1$x1+row1$x2)/2)
  x1<-x_center-R
  
  r2<-rasterImg
  #dim(r1)
  if(x1>0)
  {r2<-r2[,x1:(dim(r2)[2])]
  }else{
    gap1<- 1-x1
    tmp<-array(data = 0.5, dim = c(dim(r2)[1],gap1))
    r2<-abind::abind(tmp,r2,along = 2)
  }
  
  if((dim(r2)[2])>=2*R )
  {
    r2<-r2[,1:(2*R)]
  }else{
    gap1<-2*R-(dim(r2)[2])
    tmp<-array(data = 0.5, dim = c(dim(r2)[1],gap1))
    r2<-abind::abind(r2,tmp,along = 2)
    
  }
  
  y1<-y_center-R
  dim(r2)
  if(y1>0)
  {r2<-r2[y1:(dim(r2)[1]),]
  }else{
    gap1<- 1-y1
    tmp<-array(data = 0.5, dim = c(gap1,dim(r2)[2]))
    r2<-abind::abind(tmp,r2,along = 1)
  }
  
  
  if((dim(r2)[1])>=2*R )
  {
    r2<-r2[1:(2*R),]
  }else{
    gap1<-2*R-(dim(r2)[1])
    tmp<-array(data = 0.5, dim = c(gap1,dim(r2)[2]))
    r2<-abind::abind(r2,tmp,along = 1)
  }
  return(r2)
}

############################################x





# plateId<-tbl1$plateID[[1]]
# well<-tbl1$well[[1]]
# cellID<-tbl1$cellID[[1]]
# dir_of_calmorph<-"/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/calmorph/"
# 

# A megadott paremeterek alapjan a calmorph outputjabol kikeresi a megfelelo kepet, es abbol kivagja a megadott sejtet
# 2 kepet ad vissza, a dapi és a conA festesut kulon
# emellett visszadja a vagatlan kepek file path-jat, és a sejt poziciojat tartalmazo adattabla sort.
#
# R parameter azt szabalyozza, hogy mekkora kepet adjon vissza. A visszaadott kepke merete: 2R×2R
get_calmorph_cell_images<-function(
#cell_data_row=r1
  cell_data_row,
  dir_of_calmorph,
  dir_of_images,
  screen_metadata_tbl,
  R=55
){
  r<-cell_data_row
  
  
  dirname_of_plate<-screen_metadata_tbl %>%  filter(YMP_plate_ID==r$plateID) %>% .$YMP_folder_name
  
  # log_row1<-r %>%  select(plateID,well,image_number,x1,x2,y1,y2,genotypeID )
  # cat(paste0(paste0(names(log_row1),"=",log_row1), collapse = "; "),"\n")
  # cat("dirname_of_plate=",dirname_of_plate,"\n")
  # 
  
  
  if(!is.null(dir_of_calmorph))
  {
    if(!grepl("/$",dir_of_calmorph)){
      dir_of_calmorph<-paste0(dir_of_calmorph,"/")
    }
    
    
    file_calmorph_conA_jpg <- paste0(dir_of_calmorph,dirname_of_plate,"/",r$well,"/",r$well,"-conA", r$image_number,".jpg")
    file_calmorph_dapi_jpg <- paste0(dir_of_calmorph,dirname_of_plate,"/",r$well,"/",r$well,"-dapi", r$image_number,".jpg")
    
    
    #file.exists(file_calmorph_conA_jpg)
    
    
    raster_image_conA<-jpeg::readJPEG(source = file_calmorph_conA_jpg)
    raster_image_conA<-crop_from_raster_image(rasterImg = raster_image_conA,position = r,R=R)
    
    raster_image_dapi<-jpeg::readJPEG(source = file_calmorph_dapi_jpg)
    raster_image_dapi<-crop_from_raster_image(rasterImg = raster_image_dapi,position = r,R=R)
    
    
    
  } else
  {
    raster_image_conA<- NULL;
    raster_image_dapi<- NULL;
    file_calmorph_conA_jpg<-NULL;
    file_calmorph_dapi_jpg<-NULL;
  }
  
  
  
  
  
  if(!is.null(dir_of_images))
  {
    
    
    
    if(!grepl("/$",dir_of_images)){
      dir_of_images<-paste0(dir_of_images,"/")
    }
    
    
    file_conA_jpg2 <- paste0(dir_of_images,dirname_of_plate,"/",r$well,"/",r$well,"-C", r$image_number,".jpg")
    file_dapi_jpg2 <- paste0(dir_of_images,dirname_of_plate,"/",r$well,"/",r$well,"-D", r$image_number,".jpg")
    
    
    
    raster_image_conA2<-jpeg::readJPEG(source = file_conA_jpg2)
    raster_image_conA2<-crop_from_gray_raster_image(rasterImg = raster_image_conA2,position = r,R=R)
    
    raster_image_dapi2<-jpeg::readJPEG(source = file_dapi_jpg2)
    raster_image_dapi2<-crop_from_gray_raster_image(rasterImg = raster_image_dapi2,position = r,R=R)
    
    raster_image_mix<-array(data=0.0,dim=c(dim(raster_image_conA2),3))
    raster_image_mix[,,1]<-raster_image_dapi2
    raster_image_mix[,,2]<-raster_image_conA2
    raster_image_mix[,,3]<-raster_image_conA2
    
  }  else
  {
    raster_image_mix<- NULL;
    file_conA_jpg<-NULL;
    file_dapi_jpg<-NULL;
  }
  
  
  return(list(raster_image_dapi=raster_image_dapi,
              raster_image_conA=raster_image_conA,
              raster_image_mix=raster_image_mix,
              # file_conA_jpg=file_conA_jpg,
              # file_dapi_jpg=file_dapi_jpg,
              # file_calmorph_conA_jpg=file_calmorph_conA_jpg,
              # file_calmorph_dapi_jpg=file_calmorph_dapi_jpg,
              dirname_of_plate=dirname_of_plate,
              plateID=r$plateID,
              cell_data_row=r
  ))
}

# 
# cowplot::ggdraw() +
#   #cowplot::draw_image(raster_image_conA,x = 0,width = 0.5,scale = 1)+
#   cowplot::draw_image(raster_image_mix,x = 0.5,width = 0.5,scale = 1)
