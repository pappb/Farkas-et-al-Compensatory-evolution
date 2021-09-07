

options( encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)

###########

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

#######x


tbl1<-read_csv("/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/DATA_of_microscope/02-Operetta_Images_newMatlab_Script-2021_01_14/treshold_tables_based_on_control_well_images.csv")

tbl1<-tbl1 %>%  mutate(short_folder_name=gsub("^.*(EXP\\d\\d\\d).*$","\\1",YMP_folder_name))


cowplot::plot_grid(
tbl1 %>%  ggplot(mapping = aes(y=treshold_high_conA,x=short_folder_name, color=wellName)) +
  geom_point(position=position_dodge(width = 0.5))+
  scale_y_continuous(limits = c(NA,10000))+
  theme(legend.position = "none"),
tbl1 %>%  ggplot(mapping = aes(y=treshold_low_conA,x=short_folder_name, color=wellName)) +
  geom_point(position=position_dodge(width = 0.5))+
  scale_y_continuous(limits = c(NA,1500))+
  theme(legend.position = "none"),
tbl1 %>%  ggplot(mapping = aes(y=treshold_high_dapi,x=short_folder_name, color=wellName)) +
  geom_point(position=position_dodge(width = 0.5))+
  scale_y_continuous(limits = c(NA,4000))+
  theme(legend.position = "none"),
tbl1 %>%  ggplot(mapping = aes(y=treshold_low_dapi,x=short_folder_name, color=wellName)) +
  geom_point(position=position_dodge(width = 0.5))+
  scale_y_continuous(limits = c(NA,1000))+
  theme(legend.position = "none"),

ncol=1)


tbl1 %>%  ggplot(mapping = aes(y=treshold_high_conA, ymaxend=treshold_low_conA,x=YMP_folder_name, xend=YMP_folder_name, color=wellName)) +
  geom_linerange(position=position_dodge(width = 0.5))


tbl2<- tbl1 %>%
  select(-wvp,-wellName) %>%
  group_by(YMP_folder_name,short_folder_name) %>% summarise_all(median)


cowplot::plot_grid(
  tbl2 %>%  ggplot(mapping = aes(y=treshold_high_conA,x=short_folder_name)) +
    geom_point()+
    theme(legend.position = "none"),
  tbl2 %>%  ggplot(mapping = aes(y=treshold_low_conA,x=short_folder_name)) +
    geom_point()+
    theme(legend.position = "none"),
  tbl2 %>%  ggplot(mapping = aes(y=treshold_high_dapi,x=short_folder_name)) +
    geom_point()+
    theme(legend.position = "none"),
  tbl2 %>%  ggplot(mapping = aes(y=treshold_low_dapi,x=short_folder_name)) +
    geom_point()+
    theme(legend.position = "none"),
  
  ncol=1)

cowplot::plot_grid(
tbl2 %>%
  ggplot(mapping = aes(y=treshold_high_conA, yend=treshold_low_conA,x=short_folder_name, xend=short_folder_name)) +
  geom_segment()+
  scale_y_continuous(limits = c(0,NA))+
  labs(y= "conA intervalls"),
tbl2 %>%
  ggplot(mapping = aes(y=treshold_high_dapi, yend=treshold_low_dapi,x=short_folder_name, xend=short_folder_name)) +
  geom_segment()+
  scale_y_continuous(limits = c(0,NA))+
  labs(y= "dapi intervalls"),
ncol=1)

cowplot::plot_grid(
  tbl2 %>%
    ggplot(mapping = aes(y=treshold_high_conA/treshold_low_conA,x=short_folder_name)) +
    geom_bar(stat = "identity")+
    scale_y_continuous(limits = c(0,NA))+
    labs(y= "conA intervalls"),
  tbl2 %>%
    ggplot(mapping = aes(y=treshold_high_dapi/treshold_low_dapi,x=short_folder_name)) +
    geom_bar(stat = "identity")+
    scale_y_continuous(limits = c(0,NA))+
    labs(y= "dapi intervalls"),
  ncol=1)


write_csv(tbl2,"/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/DATA_of_microscope/02-Operetta_Images_newMatlab_Script-2021_01_14/aggregated-plate-treshold.csv")
write_tsv(tbl2,"/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/DATA_of_microscope/02-Operetta_Images_newMatlab_Script-2021_01_14/aggregated-plate-treshold.tsv")

  
