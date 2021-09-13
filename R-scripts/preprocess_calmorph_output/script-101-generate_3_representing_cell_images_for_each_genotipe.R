## minden genotipusrol csinalok egy reprezentans kepet ugy, hogy
# valasztok egy-egy A, A1B és C tipusu sejtet
# Ezeket egymsa melle rajzolom 3 kockaba

options( encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)
#source("script/script-03-tryGLMM/function-my_xlsx_save.R")
source("R-scripts/preprocess_calmorph_output/function-find_a_cell_image-v3.R")

screen_metadata_tbl<-read_csv("data/preprocess_calmorph_output/YMP_tables/YMP_screen_metadata.csv")

# strain_tbl<-read_csv("data/preprocess_calmorph_output/YMP_tables/YMP_strain_list.csv")
# strain_tbl2<-strain_tbl %>% select(genotype_ID, deletion_gene, ANCvsEVO) %>% distinct() %>% rename(genotypeID=genotype_ID)

big_tbl<-read_rds("data/preprocess_calmorph_output/smaller_big_tbl.rds")
big_tbl<-big_tbl %>%  filter(is_ok) %>% 
  mutate(Dgroup2= case_when( (Dgroup=="A1"  | Dgroup=="B" ) ~ "A1B", TRUE~ Dgroup)) %>% 
  select(Dgroup2, everything())

trait_names<-grep("^[CD]\\d\\d.*$",names(big_tbl), value = TRUE)
# 
# trait_description_tbl<-read_rds("data/preprocess_calmorph_output/trait_description_tbl.rds")


#################x

res1<-get_calmorph_cell_images(cell_data_row = big_tbl[150,],
                               screen_metadata_tbl = screen_metadata_tbl,
                               dir_of_images="/home/feketeg/eclipse-workspaces/ws3-forEclipseOxigen/MORPHOLOGY-2018/mater-BRANCH/Morphology-2018-Karcsi/link_to_raid_microsccope_data/02-Operetta_Images_newMatlab_Script/",
                               dir_of_calmorph="/home/feketeg/eclipse-workspaces/ws3-forEclipseOxigen/MORPHOLOGY-2018/mater-BRANCH/Morphology-2018-Karcsi/link_to_raid_microsccope_data/03-calmorph_newMatlab_Script/",
                               R=100)
cowplot::ggdraw() +
  cowplot::draw_image(res1$raster_image_mix,x = 0,width = 0.32,scale = 1)+
  cowplot::draw_image(res1$raster_image_conA,x = 0.33,width = 0.32,scale = 1)+
  cowplot::draw_image(res1$raster_image_dapi,x = 0.66,width = 0.32,scale = 1)


cell_data_row<-big_tbl[100,]
dir_of_images<-"/home/feketeg/eclipse-workspaces/ws3-forEclipseOxigen/MORPHOLOGY-2018/mater-BRANCH/Morphology-2018-Karcsi/link_to_raid_microsccope_data/02-Operetta_Images_newMatlab_Script/"
dir_of_calmorph<-"/home/feketeg/eclipse-workspaces/ws3-forEclipseOxigen/MORPHOLOGY-2018/mater-BRANCH/Morphology-2018-Karcsi/link_to_raid_microsccope_data/03-calmorph_newMatlab_Script/"
plates_tbl=screen_metadata_tbl
R=55


# minden genotipus - Dgroup párhoz választok egy reprezentáns sejt sort a big_tbl-bol.
res<-list()  
genotype_list<-unique(big_tbl$genotypeID)
for( i in seq(genotype_list))
{
  # DEBUG gt<-  big_tbl$genotypeID[10000]
  gt<-genotype_list[[i]]
  cat("[",i,"/",length(genotype_list),"]",gt,"\n")
  
  for( Dgroup2 in c("A","A1B","C") ) {
    
    tmp<-big_tbl %>% filter(genotypeID==gt & Dgroup2==!!Dgroup2) 
    q<-tmp %>% 
      select(all_of(trait_names)) %>% 
      select_if(~mean(!is.na(.))>0.99 ) %>% 
      mutate_all(.funs = function(x)   (rank(x) -1)/(n()-1)  -0.5) %>% 
      rowwise() %>% 
      mutate(q = max(abs(across())) ) %>% as_tibble() %>% 
      .$q
    idx_of_center <- min(which.min(q))
    
    res[[length(res)+1]]<-tmp[idx_of_center,]
  }
}
  tbl_of_representinng_cells<-do.call(bind_rows, res)
#  write_rds(tbl_of_representinng_cells,path = "tbl_of_representinng_cells.rds")
  tbl_of_representinng_cells<-read_rds(path = "tbl_of_representinng_cells.rds")
  rm(res, tmp, q)
  

# 
 strain_tbl<-read_csv("data/preprocess_calmorph_output/YMP_tables/YMP_strain_list.csv")
 strain_tbl1 <- strain_tbl %>%  select(deletion_gene, evolutionary_line, genotype_ID) %>%  rename(genotypeID=genotype_ID)
# 
# strain_tbl1 %>%  group_by(deletion_gene) %>%  summarise(n=n()) %>%  arrange(desc(n))%>%  View()
# strain_tbl1 %>%  group_by(deletion_gene) %>%  mutate(n=n()) %>% ungroup() %>%  arrange(desc(n)) %>%  View()

 tbl_of_representinng_cells<-tbl_of_representinng_cells %>% left_join(strain_tbl1)
 tbl_of_representinng_cells<-tbl_of_representinng_cells %>%  arrange(deletion_gene, evolutionary_line) %>% select(deletion_gene, evolutionary_line, Dgroup2, everything())

 tbl1<-tbl_of_representinng_cells %>% select(deletion_gene, evolutionary_line, genotypeID) %>% distinct() %>%  arrange(deletion_gene, evolutionary_line)
 
 out_dir1<-"out/2020_12_15-genotype_representing_cell_images/"
 out_dir2<-"out/2020_12_15-genotype_representing_cell_images-named_by_genes/"
 
dir.create(out_dir1,recursive = TRUE)
dir.create(out_dir2,recursive = TRUE)

for(i in seq(genotype_list))
{
  
  img_list<-list()
  for( Dgroup2 in c("A","A1B","C") ) {
    
    
  r1<-tbl_of_representinng_cells %>% filter(genotypeID==genotype_list[[i]] & Dgroup2==!!Dgroup2) 
  res1<-get_calmorph_cell_images(cell_data_row = r1,
                                 screen_metadata_tbl = screen_metadata_tbl,
                                 dir_of_images="/home/feketeg/eclipse-workspaces/ws3-forEclipseOxigen/MORPHOLOGY-2018/mater-BRANCH/Morphology-2018-Karcsi/link_to_raid_microsccope_data/02-Operetta_Images_newMatlab_Script/",
                                 dir_of_calmorph="/home/feketeg/eclipse-workspaces/ws3-forEclipseOxigen/MORPHOLOGY-2018/mater-BRANCH/Morphology-2018-Karcsi/link_to_raid_microsccope_data/03-calmorph_newMatlab_Script/",
                                 R=30)
  img_list[[Dgroup2]]<-res1
  }

  cowplot::ggdraw() +
    cowplot::draw_image(img_list$A$raster_image_mix,x = 0.01,width = 0.32,scale = 1)+
    cowplot::draw_image(img_list$A1B$raster_image_mix,x = 0.34,width = 0.32,scale = 1)+
    cowplot::draw_image(img_list$C$raster_image_mix,x = 0.67,width = 0.32,scale = 1)
  ggsave(filename = paste0(out_dir1,r1$genotypeID,".jpg"), width = 16, height = 16/3)
  ggsave(filename = paste0(out_dir2,r1$deletion_gene,"-", r1$evolutionary_line,".jpg"), width = 16, height = 16/3)
}
