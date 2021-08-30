# Beolvassa a millionyi mikroszkopos kepet, es mindegyikrol elmenti a histogram adatait a conA és a dapi csatornan.
# ezekre az adatokra  szuksege lesz az outlier szuresnek.
#
# A futtatási idő kb egy ejjszaka

graphics.off()

rm(list=ls())

library(tidyverse)
# library(cowplot)

source("script/script-08-preprocess_calmorph_output/my_parameters.R") # load the dir locations and parameters

out_data_dir1<-"data/preprocess_calmorph_output/histogra_data_of_images/";
dir.create(out_data_dir1)



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
dir1<-params$direcory_of_jpg_images_from_microscoope



subdir_name_list<-list.dirs(dir1,recursive = FALSE, full.names = FALSE)


if(! all(screen_metadata_tbl$YMP_folder_name %in% subdir_name_list ))
{
  cat("WARNING:\nHainyoznak a kovetkezo alkonyvtarak a " ,dir1,"-bol:\n  ",
      paste0(screen_metadata_tbl$YMP_folder_name[!(screen_metadata_tbl$YMP_folder_name %in% subdir_name_list )],
             collapse = "\n  "),
      "\n-----------------------------------------\n",
      sep=""
  )
  stop()
}


if(! all(subdir_name_list  %in% screen_metadata_tbl$YMP_folder_name ))
{
  cat("WARNING:\nA " ,dir1," konyvtar kobvetkezo alkonyvtarait kihagyom, mert  nem szerepelnek a plate_metadata_tbl-ben:\n  ",
      paste0(subdir_name_list[!(subdir_name_list  %in% screen_metadata_tbl$YMP_folder_name )],
             collapse = "\n  "),
      "\n-----------------------------------------\n",
      sep=""
  )
  #stop()
}





subdir_name_list<- intersect(subdir_name_list, screen_metadata_tbl$YMP_folder_name)





list_of_tbls<-list()
for(dir_of_plate in subdir_name_list)
{
  #dir_of_plate<-subdir_name_list[[1]]
  
  cat(dir_of_plate,"\n")
  
  plateID<-screen_metadata_tbl %>%  filter(use_screen) %>% 
    filter(YMP_folder_name==dir_of_plate) %>% 
    .$YMP_plate_ID
  stopifnot(length(plateID)==1 || length(plateID)==0)
  
  out_tbl_file <- paste0(out_data_dir1,plateID,".rds")
  
  
  # ha a plates_tbl tablazat szerint letezik ez a plate es hasznalni kell a normalizalashoz
  if(length(plateID)==1 & ! file.exists(out_tbl_file)){
    
    path_of_dir_of_plate<-paste0(dir1,dir_of_plate,"/")
    
    
    my_list1<-list() # ebbe gyujtom az output-ot
    
    
    
    dir_of_well_list<-list.files(path_of_dir_of_plate)
    stopifnot(all(grepl("^\\w\\d\\d$",dir_of_well_list))) # vaamelyik konytar nem ilyesmi nevu: "B01"
    for(dir_of_well in dir_of_well_list)
    {
      cat(dir_of_plate,"/",dir_of_well,"\n")
      well<-gsub("^(\\w\\d\\d).rds$","\\1",dir_of_well)
      
      f1<-paste0(path_of_dir_of_plate,dir_of_well,"/")
      
      file_tbl1<-tibble(filename=list.files(f1))
      file_tbl1<-file_tbl1 %>%  filter(grepl("...-C\\d+\\.jpg", filename) );
      file_tbl1<-file_tbl1 %>%  mutate(filanameD=gsub("(...-)C(\\d+\\.jpg)","\\1D\\2", filename) );
      file_tbl1<-file_tbl1 %>%  mutate(file_index= as.integer(gsub("...-C(\\d+)\\.jpg","\\1", filename)) ) %>%  arrange(file_index);
      for(i  in 1:nrow(file_tbl1))
      {
        well<-gsub("(...)-C(\\d+\\.jpg)","\\1", file_tbl1$filename[[i]])
        
        
        cat(i ," - ", file_tbl1$filename[[i]],"\n")
        fileC<-paste0(f1,file_tbl1$filename[[i]])
        fileD<-paste0(f1,file_tbl1$filanameD[[i]])
        raster_image_conA<-jpeg::readJPEG(source = fileC)
        raster_image_dapi<-jpeg::readJPEG(source = fileD)
        
        
        tblConA<-tibble(chanel="conA",value=0:255)    
        my_int_vector<-as.integer(as.vector(raster_image_conA)*255)
        tblConA$cnt<-sapply(tblConA$value,function(x) sum(x==my_int_vector))    
        
        tblDapi<-tibble(chanel="dapi",value=0:255)    
        my_int_vector<-as.integer(as.vector(raster_image_dapi)*255)
        tblDapi$cnt<-sapply(tblDapi$value,function(x) sum(x==my_int_vector))    
        
        tbl1<-bind_rows(tblDapi, tblConA) %>%  add_column(well=well,image_number=file_tbl1$file_index[[i]],.before = 1)
        my_list1[[length(my_list1)+1]]<-tbl1
        
      } # for ower jpg images in a well
      
    }# for ower wells
    
    tbl1<-do.call(bind_rows, my_list1)
    object.size(tbl1)
    rm(my_list1)
    
    write_rds(tbl1, out_tbl_file,compress = "gz")
    rm(tbl1)
    print(  gc())
    
  }
  
}

################




