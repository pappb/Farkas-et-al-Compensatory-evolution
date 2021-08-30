# Az elozo script kimenete meg mindig rengeteg kis tablazat( well-enkent egy)
# Mindegyiket betolti es egyetlen nagyon nagy tablazatba rakja ossze (big_tbl.rds ~ 2.6 GB)
# 
# Mellektermekkent csinal egy pici tablazatot arrol, hogy mely sejttipuspbol hanyat talalt az egyes well-ekben(summarise_tbl.rds ~ 120 KB)


options( encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)

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

subdir_name_list<-list.dirs(dir_data_out,recursive = FALSE, full.names = FALSE)


if(! all(screen_metadata_tbl$YMP_folder_name %in% subdir_name_list ))
{
  cat("WARNING:\nHainyoznak a kovetkezo alkonyvtarak a " ,dir_data_out,"-bol:\n  ",
      paste0(screen_metadata_tbl$YMP_folder_name[!(screen_metadata_tbl$YMP_folder_name %in% subdir_name_list )],
             collapse = "\n  "),
      sep=""
  )
  #stop() TODO itt meg kene allni hibaval
}


subdir_name_list<- intersect(subdir_name_list, screen_metadata_tbl$YMP_folder_name)

list_of_tbls<-list()
for(dir_of_plate in subdir_name_list)
{
  #dir_of_plate <- subdir_name_list[[1]]
  
  plateID<-screen_metadata_tbl %>%  filter(YMP_folder_name== dir_of_plate) %>% .$YMP_plate_ID
  stopifnot(length(plateID)==1)
  
  
  path_of_dir_of_plate<-paste0(dir_data_out,dir_of_plate,"/")
  
  file_well_list<-list.files(path_of_dir_of_plate)
  stopifnot(all(grepl("^\\w\\d\\d.rds$",file_well_list))) # vaamelyik konytar nem ilyesmi nevu: "B01"
  for(file_of_well in file_well_list)
  {
    cat(dir_of_plate,"/",file_of_well,"\n")
    well<-gsub("^(\\w\\d\\d).rds$","\\1",file_of_well)
    
    f1<-paste0(path_of_dir_of_plate,file_of_well)

    tbl1<-read_rds(f1)
    tbl1<-tbl1 %>%  add_column(plateID=plateID, well=well,.before = 1)
    list_of_tbls[[length(list_of_tbls)+1]]<-tbl1
  }
  gc()
}

list_of_tbls<-do.call(what = bind_rows, args =  list_of_tbls)
big_tbl<-list_of_tbls;
rm(list_of_tbls)
################
 
# out_dir1<-"out/2019_12_04/";
# dir.create(out_dir1)

cat("Writeing big_tbl.rds. it takes some time\n" )
write_rds(big_tbl,  paste0(dir_data_out,"../big_tbl.rds"),compress = "gz")
cat("Done.\n" )

gc(reset = TRUE, full = TRUE)
#big_tbl<-read_rds( path = paste0(dir_data_out,"big_tbl.rds"))
  
# filter_tbl<-plates_tbl %>% filter(use_for_outlier_filter.well_level) %>%  select(plateID)
# big_tbl<-semi_join(big_tbl,filter_tbl, by = "plateID")
# 
# 
# summarise_tbl1<-big_tbl %>% group_by( plateID,well,Cgroup,Dgroup) %>%  summarise(count=n()) %>%  ungroup()
 summarise_tbl1<-big_tbl %>% group_by( plateID,well,Dgroup) %>%  summarise(count=n()) %>%  ungroup()
# print(object.size(summarise_tbl1), unit="MB")
 write_rds(summarise_tbl1, paste0(dir_data_out,"../summarise_of_raw_big_tbl.rds"),compress = "gz")
 
 source("script/script-03-tryGLMM/function-my_xlsx_save.R")
 summarise_tbl2<-summarise_tbl1 %>% spread(key="Dgroup", value="count")
 summarise_tbl2 %>% rename(complex=`-`) 
 my_xlsx_save(summarise_tbl2,file = "data/preprocess_calmorph_output/morphology_tbls.xlsx",sheet = "summarise_of_raw_big_tbl-Dgroup")
 
 summarise_tbl3<-big_tbl %>%
   group_by( plateID) %>%
   summarise(well_count=length(unique(well)) , cell_count=n(), cell_per_well=cell_count/well_count) %>% 
   arrange(plateID)
 my_xlsx_save(tbl=summarise_tbl3,file = "data/preprocess_calmorph_output/morphology_tbls.xlsx",sheet = "summarise_of_raw_big_tbl-plates")
 
 
 #
 
# 
# 
# 
# 
# big_tbl %>% select(Cgroup) %>%  distinct()
# big_tbl<-big_tbl %>% filter(Cgroup!="complex")
# 
# big_tbl %>% select(Dgroup) %>%  distinct()
# summarise_tbl1  %>% filter(Cgroup!="complex")%>% group_by(Dgroup) %>% summarise(count=sum(count))
# 
# big_tbl %>%head(20) %>% View()
# 
# 
# 
# tbl2<-tbl1 %>% group_by( plateID,well,image_number) %>%  summarise(count=sum(count)) %>%  ungroup()
# tbl2 %>% ggplot(aes(x=count))+geom_histogram()
# tbl2 %>% ggplot(aes(x=count))+stat_ecdf()
# 
# tbl2 %>% ggplot(aes(x=count))+geom_histogram() +facet_grid(plateID~.)
# ggsave(filename = paste0(out_dir1,"x.pdf"),width = 16, height = 3*9)
# 
# 
# tbl2 %>% ggplot(aes(x=count, color=plateID))+geom_density()
