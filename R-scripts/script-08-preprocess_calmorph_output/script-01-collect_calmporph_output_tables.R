# Eza script azt csinálja, hogy a CALMORPH output-jan vegigmegy, 
# osszegyujti az osszes xls tablazatban talalhato sejtszitu adatokat (ezek valojaban tsv-k, csak a filename xls)
# Az eredményt nagyobb fileokba gyujti. Eredetileg well-enkent 5 file volt egy csomo jpg file koze keverve. 
# A kimenetpen welle-nkent egy tablazat rds-ben mentve.



options( encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)

source("script/script-08-preprocess_calmorph_output/my_parameters.R") # load the dir locations and parameters

dir_data_out<-"data/preprocess_calmorph_output/data-collected_all_cell_level_data_from_calmorph_output/"
dir.create(dir_data_out,recursive = TRUE);


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


# plates_tbl<-xlsx::read.xlsx(file = "/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/DATA_of_microscope/plates_tbl.xlsx",sheetIndex = 1,stringsAsFactors=FALSE) %>%
#   as_tibble()  %>% 
#   mutate_at(.vars=grep("use_",names(. ), ignore.case = TRUE,value =TRUE), .funs = as.logical) %>% 
#   filter(!is.na(filename))
# 
# stopifnot(!any(duplicated( plates_tbl$plateID)))
# stopifnot(!any(duplicated( plates_tbl$filename)))

# directory of the  output of the calmorph
dir0<-params$direcory_of_calmorph_output
subdir_name_list<-list.dirs(dir0,recursive = FALSE, full.names = FALSE)


if(! all(screen_metadata_tbl$YMP_folder_name %in% subdir_name_list ))
{
  cat("WARNING:\nHainyoznak a kovetkezo alkonyvtarak a " ,dir0,"-bol:\n  ",
      paste0(screen_metadata_tbl$YMP_folder_name[!(screen_metadata_tbl$YMP_folder_name %in% subdir_name_list )],
            collapse = "\n  "),
      sep=""
      )
  #stop()
}


subdir_name_list<- intersect(subdir_name_list, screen_metadata_tbl$YMP_folder_name)

for(dir_of_plate in subdir_name_list)
{
  # dir_of_plate <- subdir_name_list[[1]]
  path_of_dir_of_plate<-paste0(dir0,dir_of_plate,"/")
  cat(dir_of_plate,"\n")
  
  subdir_well_list<-list.dirs(path_of_dir_of_plate,recursive = FALSE, full.names = FALSE)
  stopifnot(all(grepl("\\w\\d\\d",subdir_well_list))) # vaamelyik konytar nem ilyesmi nevu: "B01"
  for(dir_of_well in subdir_well_list)
  {
    #dir_of_well<-"B02"
    cat(dir_of_plate,"/",dir_of_well,"\n")
    output_file_name<-paste0(dir_data_out,dir_of_plate,"/",dir_of_well,".rds")
    
    if(file.exists(output_file_name))
    {
      cat("  SKIPPED\n")
    }else{
      
      dir1<-paste0(path_of_dir_of_plate,dir_of_well,"/")
      #dir1<-"/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/calmorph/151022_morph-coated-noActin_morph_WT-anc_ev11-22__2015-10-22T20_39_31-Measurement1/c11/"
      
      
      xls_file_list<-list.files(dir1,pattern = "*.xls")
      
      
      file_conA_basic<-grep("\\w\\d\\d_conA_basic.xls",xls_file_list, value = TRUE)
      file_conA_biological<-grep("\\w\\d\\d_conA_biological.xls",xls_file_list, value = TRUE)
      file_dapi_basic<-grep("\\w\\d\\d_dapi_basic.xls",xls_file_list, value = TRUE)
      file_dapi_biological<-grep("\\w\\d\\d_dapi_biological.xls",xls_file_list, value = TRUE)
      file_versatile<-grep("\\w\\d\\d.xls",xls_file_list, value = TRUE)
      
      stopifnot(length(file_conA_basic)==1)
      stopifnot(length(file_conA_biological)==1)
      stopifnot(length(file_dapi_basic)==1)
      stopifnot(length(file_dapi_biological)==1)
      stopifnot(length(file_versatile)==1)
      
      
      tbl1<-read_tsv(paste0(dir1,file_versatile), col_types = cols(
        .default = col_double(),
        image_number = col_double(),
        cell_id = col_double(),
        Cgroup = col_character(),
        Agroup = col_character(),
        Dgroup = col_character()
      ))
      # stopifnot(all(is.na(tbl1$X103))) # delete empty last column
      # tbl1 <- tbl1 %>% select(-X103)
      
      tbl_conA_basic<-read_tsv(paste0(dir1,file_conA_basic), col_types = cols(
        .default = col_character(),
        image_number = col_double(),
        cell_id = col_double(),
        Cgroup = col_character(),
        `C11-1` = col_double(),
        `C11-2` = col_double(),
        `C12-1` = col_double(),
        `C12-2` = col_double(),
        C13 = col_double(),
        X24 = col_logical()
      ))
      stopifnot(all(is.na(tbl_conA_basic$X24))) # delete empty last column
      tbl_conA_basic <- tbl_conA_basic %>% select(-X24)
      
      
      
      tbl_conA_biological<-read_tsv(paste0(dir1,file_conA_biological), col_types = cols(
        .default = col_double(),
        Cgroup = col_character(),
        image_number = col_integer(),
        cell_id=col_integer(),
        X25 = col_logical()
      ))
      stopifnot(all(is.na(tbl_conA_biological$X25))) # delete empty last column
      tbl_conA_biological <- tbl_conA_biological %>% select(-X25)
      
      tbl_dapi_basic<-read_tsv(paste0(dir1,file_dapi_basic), col_types =  cols(
        .default = col_character(),
        image_number = col_integer(),
        cell_id=col_integer(),
        Cgroup = col_character(),
        Dgroup = col_character(),
        `D14-1` = col_double(),
        `D14-2` = col_double(),
        `D14-3` = col_double(),
        `D15-1` = col_double(),
        `D15-2` = col_double(),
        `D15-3` = col_double(),
        `D16-1` = col_double(),
        `D16-2` = col_double(),
        `D16-3` = col_double(),
        `D17-1` = col_double(),
        `D17-2` = col_double(),
        `D17-3` = col_double(),
        X48 = col_logical() ))
      stopifnot(all(is.na(tbl_dapi_basic$X48))) # delete empty last column
      tbl_dapi_basic <- tbl_dapi_basic %>% select(-X48)
      
      
      
      tbl_dapi_biological<-read_tsv(paste0(dir1,file_dapi_biological), col_types = cols(
        .default = col_double(),
        Cgroup = col_character(),
        Dgroup = col_character(),
        image_number = col_integer(),
        cell_id=col_integer(),
        X103 = col_logical()
      ))
      stopifnot(all(is.na(tbl_dapi_biological$X103))) # delete empty last column
      tbl_dapi_biological <- tbl_dapi_biological %>% select(-X103)
      
      ########################
      
      stopifnot(all(
        intersect(names(tbl_conA_biological), names(tbl_conA_basic)) ==
          c("image_number", "cell_id", "Cgroup")))
      
      stopifnot(nrow(tbl_conA_biological)==nrow(tbl_conA_basic))
      stopifnot(all(tbl_conA_biological$image_number==tbl_conA_basic$image_number))
      stopifnot(all(tbl_conA_biological$cell_id==tbl_conA_basic$cell_id))
      stopifnot(all(tbl_conA_biological$Cgroup==tbl_conA_basic$Cgroup))
      
      tbl_conA<-bind_cols(tbl_conA_basic, tbl_conA_biological %>% select(-image_number, -cell_id, -Cgroup))
      rm(tbl_conA_basic, tbl_conA_biological)
      
      #############
      
      stopifnot(all(
        intersect(names(tbl_dapi_biological), names(tbl_dapi_basic)) ==
          c("image_number", "cell_id", "Cgroup", "Dgroup")))
      
      stopifnot(nrow(tbl_dapi_biological)==nrow(tbl_dapi_basic))
      stopifnot(all(tbl_dapi_biological$image_number==tbl_dapi_basic$image_number))
      stopifnot(all(tbl_dapi_biological$cell_id==tbl_dapi_basic$cell_id))
      stopifnot(all(tbl_dapi_biological$Cgroup==tbl_dapi_basic$Cgroup))
      stopifnot(all(tbl_dapi_biological$Dgroup==tbl_dapi_basic$Dgroup))
      
      tbl_dapi<-bind_cols(tbl_dapi_basic, tbl_dapi_biological %>% select(-image_number, -cell_id, -Cgroup, -Dgroup))
      rm(tbl_dapi_basic, tbl_dapi_biological)
      
      ###############
      
      stopifnot(all(
        intersect(names(tbl_dapi), names(tbl_conA)) ==
          c("image_number", "cell_id", "Cgroup")))
      
      stopifnot(nrow(tbl_conA)==nrow(tbl_dapi))
      stopifnot(all(tbl_conA$image_number==tbl_dapi$image_number))
      stopifnot(all(tbl_conA$cell_id==tbl_dapi$cell_id))
      stopifnot(all(tbl_conA$Cgroup==tbl_dapi$Cgroup))
      
      
      tbl_combi1<-bind_cols(tbl_dapi %>% select(image_number, cell_id, Cgroup, Dgroup),
                            tbl_conA %>% select(-image_number, -cell_id, -Cgroup, ),
                            tbl_dapi %>% select(-image_number, -cell_id, -Cgroup, -Dgroup))
      rm(tbl_conA, tbl_dapi)
      #############x
      
      #grep("actin",names(tbl1), value = TRUE)
      
      # I drop all the actin related columns, but first I chneck if it is really empty
      stopifnot(all(tbl1$Agroup=="-"))
      stopifnot(nrow(tbl1)==0 || all(tbl1 %>% select(actin_region_ratio,bud_actin_ratio,mother_actin_gravity_point, bud_actin_gravity_point   )==-1))
      tbl1<- tbl1 %>% select(-Agroup,-actin_region_ratio,-bud_actin_ratio,-mother_actin_gravity_point, -bud_actin_gravity_point   )
      
      stopifnot(all(
        intersect(names(tbl1), names(tbl_combi1)) ==
          c("image_number", "cell_id", "Cgroup", "Dgroup")))
      
      stopifnot(nrow(tbl1)==nrow(tbl_combi1))
      stopifnot(all(tbl1$image_number==tbl_combi1$image_number))
      stopifnot(all(tbl1$cell_id==tbl_combi1$cell_id))
      stopifnot(all(tbl1$Cgroup==tbl_combi1$Cgroup))
      stopifnot(all(tbl1$Dgroup==tbl_combi1$Dgroup))
      
      tbl_combi2<-bind_cols(tbl1 %>% select(image_number, cell_id, Cgroup, Dgroup),
                            tbl1 %>% select(-image_number, -cell_id, -Cgroup, -Dgroup),
                            tbl_combi1 %>% select(-image_number, -cell_id, -Cgroup, -Dgroup))
      
      rm(tbl1, tbl_combi1)
      ################
      
      sapply(tbl_combi2,FUN = typeof)
      tbl_combi2$`D11-1`
      
      tmp<-sapply(tbl_combi2,FUN = typeof)
      tbl_combi2<-tbl_combi2 %>%
        mutate_at(.vars= names(tmp)[tmp=="double"], .funs = function(x) {x[x==-1]<-as.double(NA); return(x)}) %>% 
        mutate_at(.vars= names(tmp)[tmp=="character"], .funs = function(x) {x[x=="[-1,-1]"]<-as.character(NA); return(x)})
      
      
      stopifnot(length(setdiff(unique(tbl_combi2$Cgroup),c("complex", "large" ,"medium","no", "small")))==0)
      stopifnot(length(setdiff(unique(tbl_combi2$Dgroup),c("-","A","A1","B","C","D","E","F" )))==0)
      
      
      tbl_combi2 %>%  group_by(Dgroup) %>%  summarise(n())
      tbl_combi2 %>%  group_by(Cgroup) %>%  summarise(n())
      
      
      cat("  size of table of well ")
      
      print(object.size(tbl_combi2), unit="MB")
      dir.create(paste0(dir_data_out,dir_of_plate,"/"))
      write_rds(x = tbl_combi2, output_file_name ,compress = "gz")
      #write_tsv(x = tbl_combi2,path = paste0(dir_data_out,"x.tsv"))
    } # end of 'if output_file not exist yet'
  } 
  
}


################



