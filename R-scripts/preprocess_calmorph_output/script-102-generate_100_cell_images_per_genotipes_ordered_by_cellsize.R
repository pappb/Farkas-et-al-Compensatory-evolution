# For each genotype I generate a collection of 100 cell images
# The image file numbers are ordered by the cell  seize.
# the 100 cell contains each A, A1, B and C  of DGroup cells. The number of images of a DGroup is proportional to the genotype carateristic.

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

out_dir1<-"out/2020_12_15-cell_order/"
dir.create(out_dir1,recursive = TRUE)

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
    
    # I order tche cells by  Dgroup-fase and cell size. The smallest cell is the first.
    # C101=cell size
    tbl1<-tbl1 %>% arrange(Dgroup, C101) 
    
    # I select the 100 images uniformly distributed
    idx<-round( seq(0,1,length.out = 100)*(nrow(tbl1)-1) +1)
    tbl1<-tbl1[idx,]
    
    
    # for each row of tbl a png image will be generated
    for(ii in 1:nrow(tbl1))
    {
      
      
      r1<-tbl1[ii,]
      
      # this function looks for the images. It returns 3 image objects ( dapi chanel/ conA chanel / mixed) 
      res1<-get_calmorph_cell_images(cell_data_row = r1,
                                     screen_metadata_tbl = screen_metadata_tbl,
                                     dir_of_images=params$direcory_of_jpg_images_from_microscoope, 
                                     dir_of_calmorph=params$direcory_of_calmorph_output,
                                     R=30)
      
      cowplot::ggdraw() +
        cowplot::draw_image(res1$raster_image_mix,x = 0,width = 1,scale = 1)
      ggsave(filename = sprintf("%s/%04i-%s.png", out_dir2, ii, r1$Dgroup2), width = 1, height = 1)
      
    }
    Sys.sleep(3) # wait 3 secounds to prevent I/O errors
  }
  
}
