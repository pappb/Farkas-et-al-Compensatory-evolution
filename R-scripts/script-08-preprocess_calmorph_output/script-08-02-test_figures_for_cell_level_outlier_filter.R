# Az outlier szures ellenorzesere keszit egy csomo hisztogramot (density fuggvenyt)
# Minden trait-re elkesziti az osszes torzshoz tartozo abrat, bejelöli rajta az outlierszuro altal meghatarozott treshold-ot, 
# illetve a mediant es a 15% és 85%-os kvantiilist.
# piros szonyeggel megjeloli az outlierek poziciojat, de őpirosak nem csak a hatarokon tul vannak, mert masik trait alapjan is lehetnek outliernek jelolve
#
# @author: Fekete Gergeo


graphics.off()

rm(list=ls())

library(tidyverse)
library(cowplot)
source("script/script-03-tryGLMM/function-my_xlsx_save.R")


out_dir="out/2020_08_30-outlier_filter-test_figures-only_genotypes_for_paper-newAssay/"
dir.create(out_dir)
####################################x####################################x####################################x


data0 <- read_rds("data-source/data0.rds")

plate_map_tbl<-read_rds("data/preprocess_calmorph_output/plate_map_tbl.rds")
genotype_label_tbl<-plate_map_tbl %>% select(plateID, genotypeID, genotype_label) %>%  distinct()



big_tbl<- read_rds("data/preprocess_calmorph_output/smaller_big_tbl.rds") %>% 
  mutate(Dgroup=ifelse( (Dgroup=="A1" | Dgroup=="B") , "A1B", Dgroup)) 

big_tbl<-big_tbl %>% semi_join(plate_map_tbl %>% filter(use_for_paper), by=c("plateID","genotypeID"))

print(object.size(big_tbl), units = "GB")

aggregated_tbl2<- read_rds( "data/preprocess_calmorph_output/outlyer_tresholds_tbl.rds")

#write_rds(outlier_filter_which_traits_are_failed,"data/preprocess_calmorph_output/outlier_filter_which_traits_are_failed.rds", compress = "gz")

tbl1<-aggregated_tbl2 %>% ungroup() %>%  dplyr::select(trait, Dgroup) %>%  distinct()
for( i in 1:nrow(tbl1))
{
  r1<- tbl1[i,]
  cat(r1$trait,"_",r1$Dgroup,"\n")
  
  pdf(file = paste0(out_dir,r1$trait,"_",r1$Dgroup,".pdf"), width = 16, height = 9);
  print(data0$get_parameter_figure(paste0(r1$trait,"_",r1$Dgroup)))
  
  NR<-6;
  y_pos=1-1/NR;
  my_draw=cowplot::ggdraw();
  tbl2<- aggregated_tbl2 %>% semi_join(r1, by = c("trait", "Dgroup"))
  
  range1<-big_tbl %>% filter(is_ok) %>% semi_join(r1, by = c( "Dgroup")) %>% summarise(max=max( !!(as.name(r1$trait)) , na.rm = TRUE) ,min=min( !!(as.name(r1$trait)) , na.rm = TRUE) )
  
  for( j in 1:nrow(tbl2))
  {
    r2<- tbl2[j,]
    if(j%%20==0) {cat("  j=",j,"/",nrow(tbl2),"\n")}
    
    genotype_label<-genotype_label_tbl %>% semi_join(r2, by = c("plateID", "genotypeID")) %>% .$genotype_label
    
    tbl3<-big_tbl %>% semi_join(r2, by=c("plateID", "genotypeID", "Dgroup"))
    
    if(nrow(tbl3)>0){
      plot1<-
        ggplot(mapping = aes( x=!!(as.name(r2$trait)) ))+
        geom_vline(xintercept = c(r2$median1,r2$q_ca15, r2$q_ca85) , color="gray")+
        geom_vline(xintercept = c(r2$treshold_low, r2$treshold_high) , color="gray20")+
        geom_density(data=tbl3 , color="red")+
        geom_density(data=tbl3 %>% filter(is_ok) , color="blue")+
        #   geom_rug()+
        geom_rug(data=tbl3 %>% filter(is_marked_for_deletion_by_outlier_filter) , color="red")+
        coord_cartesian(xlim = c(range1$min, range1$max))+
        theme(axis.title =element_blank())
      
      if(is.na(r2$treshold_high)){
        plot1<-  plot1+theme(plot.background = element_rect(fill="pink"))
      }
      
      my_draw<-my_draw+
        draw_plot(plot1, x = 0.2,width = 0.8, y= y_pos, height = 1/NR)+
        draw_text(text = paste0("[",j,"]  ",genotype_label,"\n", r2$plateID,"\n",r2$trait,"_",r2$Dgroup), x = 0.01, y= y_pos+0.02 ,hjust = 0, vjust = 0, size=11)+
        draw_text(text = sprintf("N=%i\nM=%0.2f\nOutl=%i",r2$n_not_NA, r2$M, r2$cnt_too_high+r2$cnt_too_low), x = 0.96, y= y_pos+(1/NR)-0.01 ,hjust = 0, vjust = 1, size = 8)
      #print(my_draw)
      y_pos<-y_pos-1/NR
      
      if(y_pos<=0){
        print(my_draw);
        my_draw=cowplot::ggdraw();
        y_pos<-1-1/NR
        #      stop()
      }
    }
  }
  
  dev.off() # close pfd device
}






