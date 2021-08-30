


options( encoding="utf8")
graphics.off()

rm(list=ls())



library(tidyverse)
source("script/script-03-tryGLMM/function-my_xlsx_save.R")

#YMP_data_dir<-"link_to_raid_microsccope_data/YMP_tables/"
in_YMP_data_dir<-"/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/DATA_of_microscope/YMP_tables/"
out_YMP_data_dir<-"data/preprocess_calmorph_output/YMP_tables/"
dir.create(out_YMP_data_dir, recursive = TRUE)

for( f1 in c("YMP_plate_layout.csv","YMP_screen_metadata.csv","YMP_strain_list.csv","YMP_strain_blacklist.csv"))
{
  cat("copying ", f1,"\n", sep="")
  
  tbl1<-read_csv(file = paste0( in_YMP_data_dir,f1), col_types = cols(  .default = col_character()))
  write_csv(tbl1, paste0(  out_YMP_data_dir,f1) )
  my_xlsx_save(tbl1, paste0(  out_YMP_data_dir,"YMP_tables.xlsx"), sheet=gsub("\\.csv$","",f1))
}

