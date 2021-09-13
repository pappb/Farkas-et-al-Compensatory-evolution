# Minden genotipushoz csinalok egy konyvtarba 100 sejtrol egy-egy kepet
# szamozva vannak a kepek, es a sorrend a sejtfazisokat igyekszik mutatni:
# A, A1, B, C sejtfazisok jonnek egymas utan, es egy sejtfazison belul sejtmeret szerint jonnek sorba a sejtek.
#
# Az egyes sejtfazisokbol szarmazo kepek aranya aranyos a genotipusra jellemzovel

options( encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)
#source("script/script-03-tryGLMM/function-my_xlsx_save.R")
source("R-scripts/preprocess_calmorph_output/function-find_a_cell_image-v2.R")

source("R-scripts/preprocess_calmorph_output/my_parameters.R")

screen_metadata_tbl<-read_csv("data/preprocess_calmorph_output/YMP_tables/YMP_screen_metadata.csv")

strain_tbl<-read_csv("data/preprocess_calmorph_output/YMP_tables/YMP_strain_list.csv")
strain_tbl1 <- strain_tbl %>%
  select(deletion_gene, evolutionary_line, genotype_ID) %>% 
  distinct() %>% 
  rename(genotypeID=genotype_ID)

big_tbl<-read_rds("data/preprocess_calmorph_output/smaller_big_tbl.rds")
big_tbl<-big_tbl %>%  filter(is_ok) %>% 
  mutate(Dgroup2= case_when( (Dgroup=="A1"  | Dgroup=="B" ) ~ "A1B", TRUE~ Dgroup)) %>% 
  select(Dgroup2, everything())

trait_names<-grep("^[CD]\\d\\d.*$",names(big_tbl), value = TRUE)
# 
# trait_description_tbl<-read_rds("data/preprocess_calmorph_output/trait_description_tbl.rds")

out_dir1<-"out/2020_12_15-cell_order/"
dir.create(out_dir1,recursive = TRUE)

#tbl1<-big_tbl %>%  filter(plateID=="YMP_plate_07" & well=="G08")
genotype_list<-unique(big_tbl$genotypeID)
genotype_list<-setdiff(genotype_list,"SIGMA")
for( i in seq(genotype_list))
{
  # DEBUG gt<-  big_tbl$genotypeID[10000]
  # DEBUG gt<-  "SZ2014_YMP_012"
  # DEBUG gt<- "SZ2014_YMP_044"
  gt<-genotype_list[[i]]
  cat("[",i,"/",length(genotype_list),"]",gt,"\n")
  
  row_of_strain<-strain_tbl1 %>% filter(genotypeID==gt)
  stopifnot(nrow(row_of_strain)==1)
  
  out_dir2<-sprintf("%s%s-%s-%s/",
                    out_dir1,
                    row_of_strain$deletion_gene,
                    row_of_strain$evolutionary_line,
                    row_of_strain$genotypeID )
  if(dir.exists(out_dir2))
  {
    cat("SKIPPING: ", out_dir2, "\n")
  }  else{
    dir.create(out_dir2,recursive = TRUE)
    
    
    
    tbl1<-big_tbl %>%  filter(genotypeID==gt)
    
    # sorba rakom a sejteket nagysag szerint , ugy hogy eloszor az A, aztán az A1, aztan B aztan a C allapotuak jonnek
    # es allapotokon belul a lehgkisebb van elol, a legnagyobb utoljara
    # A (C101 a sejtmeretet jelennti)
    tbl1<-tbl1 %>% arrange(Dgroup, C101) 
    # egyenletesen elosztva kiveszek 100 sejtet
    idx<-round( seq(0,1,length.out = 100)*(nrow(tbl1)-1) +1)
    tbl1<-tbl1[idx,]
    
    
    # a tbl1 minden sorához generalok egy png kepet arrol a sejtrol
    
    for(ii in 1:nrow(tbl1))
    {
      
      
      r1<-tbl1[ii,]
      
      # ez a fuggveny egy adatsorhoz kikeresi a kepeket es egy listaba szervezve visszaad 3 kep ojketumot
      res1<-get_calmorph_cell_images(cell_data_row = r1,
                                     screen_metadata_tbl = screen_metadata_tbl,
                                     
                                     dir_of_images=params$direcory_of_jpg_images_from_microscoope, #"/home/feketeg/eclipse-workspaces/ws3-forEclipseOxigen/MORPHOLOGY-2018/mater-BRANCH/Morphology-2018-Karcsi/link_to_raid_microsccope_data/02-Operetta_Images_newMatlab_Script-2021_02_10/",
                                     dir_of_calmorph=params$direcory_of_calmorph_output,#"/home/feketeg/eclipse-workspaces/ws3-forEclipseOxigen/MORPHOLOGY-2018/mater-BRANCH/Morphology-2018-Karcsi/link_to_raid_microsccope_data/03-calmorph_newMatlab_Script/",
                                     R=30)
      
      cowplot::ggdraw() +
        cowplot::draw_image(res1$raster_image_mix,x = 0,width = 1,scale = 1)
      ggsave(filename = sprintf("%s/%04i-%s.png", out_dir2, ii, r1$Dgroup2), width = 1, height = 1)
      
    }
    Sys.sleep(3) # 3 secound várakozás, hogy a RAID-et hagyjuk pihenni.
  }
  
}
