

options( encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)



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

 plate_layout_tbl<-read_csv(file = "data/preprocess_calmorph_output/YMP_tables/YMP_plate_layout.csv")
 
 ##################
 
 tmp<-plate_layout_tbl %>% filter(use_strain & is_control)

 tmp<-inner_join(tmp,screen_metadata_tbl,by = "YMP_plate_ID")
 
 tmp<-tmp %>%  select(YMP_plate_ID,YMP_folder_name,YMP_well, genotype_ID) %>% distinct()
 
 dir.create("TMP_BIG_DATA")
 write_csv(tmp,"TMP_BIG_DATA/control_well_list.csv")
 
