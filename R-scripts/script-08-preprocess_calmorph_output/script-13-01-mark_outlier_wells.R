

options( encoding="utf8")
graphics.off()

rm(list=ls())

library(tidyverse)
source("script/script-03-tryGLMM/function-my_xlsx_save.R")


strain_tbl<-read_csv("data/preprocess_calmorph_output/YMP_tables/YMP_strain_list.csv")
strain_tbl2<-strain_tbl %>% select(genotype_ID, deletion_gene, ANCvsEVO) %>% distinct() %>% rename(genotypeID=genotype_ID)

big_tbl<-read_rds("data/preprocess_calmorph_output/smaller_big_tbl.rds")
big_tbl<-big_tbl %>%  filter(is_ok) %>% 
  mutate(Dgroup2= case_when( (Dgroup=="A1"  | Dgroup=="B" ) ~ "A1B", TRUE~ Dgroup)) %>% 
  select(Dgroup2, everything())

trait_names<-grep("^[CD]\\d\\d.*$",names(big_tbl), value = TRUE)

trait_description_tbl<-read_rds("data/preprocess_calmorph_output/trait_description_tbl.rds")

## ezen level alapjan kiszurok par doolgot
#Majd küldd el úgy is a histogramot és csináld úgy a normalizálást,
#hogy kiszeded az összes brightness traitet + a 3 rossz Kroskal-Wallis trait-et:
#  D165_A1B D172_A1B D161_A1B
# Köszönöm:   Karcsi

trait_filter_tbl <- trait_description_tbl %>%
  filter(is_used) %>%
  #filter(trait!="D165_A1B" & trait!="D172_A1B" & trait!="D161_A1B" ) %>%
  select(trait) %>% 
  mutate(trait1= gsub("^([^_]+)_([^_]+)","\\1",trait)) %>% 
  mutate(Dgroup2= gsub("^([^_]+)_([^_]+)","\\2",trait)) %>% 
  select(-trait) %>% rename(trait=trait1)



#TODO ezt a foloslegs fuggvenyt torolni
my_mean<-function(x, sample_size_limit=50)
{
  x <-  x[is.finite(x)]
  if(length(x) <= sample_size_limit) {return (NA)}  
  else{ return(mean(x) ) } 
}
# 
# 
# my_sd<-function(x , sample_size_limit=50)
# {
#   x <-  x[is.finite(x)]
#   if(length(x) <= sample_size_limit) {return (NA)}  
#   else{ return(sd(x) ) } 
# }


mean_tbl1<-big_tbl %>%
  group_by(Dgroup2,plateID, well,  genotypeID) %>%
  #summarise_at(.vars = trait_names, .funs =function(x) my_mean(x, sample_size_limit = 50) ) %>%
  summarise_at(.vars = trait_names, .funs =function(x) mean(x, na.rm=TRUE) ) %>% 
  ungroup()
# sd_tbl1<-big_tbl %>%
#   group_by(Dgroup2,genotypeID) %>%  summarise_at(.vars = trait_names, .funs = sd)


#names(mean_tbl1)
mean_lft1 <- # long format table
  mean_tbl1 %>%
  gather(key = "trait", value="mean", - Dgroup2, -plateID, -well, -genotypeID) %>%
  semi_join(trait_filter_tbl) %>% 
  rename(Dgroup=Dgroup2)


list1<-list()
genotypeID_list<- unique(mean_lft1$genotypeID)
for(genotypeID1 in genotypeID_list)
{
  #DEBUG genotypeID1<-"SZ2014_YMP_120"
  cat(genotypeID1,"\n")
  
  tmp1<-mean_lft1 %>%  filter(genotypeID==genotypeID1)
  
  tmp_sd_tbl1 <- tmp1 %>%
    group_by(trait,Dgroup) %>% 
    summarise(sd_all_well_means=sd(mean, na.rm = TRUE),sample_size=n(),  .groups ="drop" )
  
  # positions of the replicates (plateID,well)
  rep_tbl<-tmp1 %>% select(plateID, well) %>%  distinct()
  for( i in 1:nrow(rep_tbl)){
    tmp2<-  tmp1 %>% anti_join(rep_tbl[i,] , by = c("plateID", "well")) # ebbol kihagytam az egyik well-t
    # tmp2 %>% select(plateID, well) %>%  distinct()
    
    tmp_sd_tbl2 <- tmp2 %>%
      group_by(trait,Dgroup) %>% summarise(sd_excluded_one_well=sd(mean,na.rm=TRUE) , .groups ="drop")
    
    tmp_sd_tbl3<- full_join(tmp_sd_tbl1,tmp_sd_tbl2 , by = c("trait", "Dgroup")) %>% 
      bind_cols(rep_tbl[i,]) %>% 
      add_column(genotypeID=genotypeID1)
    
    list1[[length(list1)+1]] =tmp_sd_tbl3
  }
  
}
rel_sd_tbl1<-do.call(bind_rows, list1)
rel_sd_tbl1<-rel_sd_tbl1 %>% mutate(rel_sd=  sd_excluded_one_well/sd_all_well_means)

# takaritas
rm(list=grep("^tmp", ls(), value = TRUE) )
rm(list1, i, genotypeID1, rep_tbl)

############################################################################################
############################################################################################
############################################################################################

my_min<-function(arr,v)
{
  mapply(arr,FUN =  function(x) min(x,v))
}

treshold_score1<-30
treshold_score2<-0.4


well_score_tbl <- 
  rel_sd_tbl1 %>%
  group_by(plateID, well ,genotypeID  ) %>%
  summarise(sample_size=sample_size[[1]],
            score1=sum(rel_sd < 0.50, na.rm = TRUE),                                                                  
            score2=mean(my_min(log2(1/rel_sd),1004), na.rm = TRUE), .groups="drop") %>% 
  mutate(label=paste(genotypeID,plateID,well)) %>%
  left_join(strain_tbl2,by=  "genotypeID") %>% 
  mutate(label2=sprintf("%s-%s %s  [%i, %0.2f]", deletion_gene, gsub("^(...).*$","\\1",ANCvsEVO),label, score1, score2)) %>% 
  mutate(color=case_when(sample_size<4 ~ "blue", score1>treshold_score1 & score2>treshold_score2 ~"red", TRUE~"black")) %>% 
  arrange(label)  



#######################


hline_positions<-which(
  well_score_tbl %>% 
    mutate(x=genotypeID != lag(genotypeID)) %>% 
    .$x
)
#hline_positions<-which( genotype1 != lag(genotype1))



plot_heatmap_1<-
  rel_sd_tbl1 %>%
  #mutate(lab1=sprintf("%i",round(log2(1/rel_sd))))
  
  ggplot(mapping = aes(y=paste(genotypeID,plateID,well), x=paste(trait,Dgroup),fill=log2(1/rel_sd)))+
  geom_raster()+
  geom_hline(yintercept = hline_positions-0.5)+
  theme(legend.position = "top")+
  #scale_fill_gradient2(midpoint = 0,limits=c(-3,2), trans="log")
  scale_fill_gradient2(midpoint = 0, low = "blue", high = "red2")+ #,limits=c(0,2), )+
  scale_y_discrete(limits=well_score_tbl$label, labels=well_score_tbl$label2)+
  theme(axis.text.y = element_text(colour = well_score_tbl$color))+
  #scale_y_discrete(limits=trait_order1)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 1))+
  labs(caption=paste(
    "Az abra azt vizsgalja, hogy az egyes well-ek kilognak-e az egyes genotipusok replikatumai közül.\n",
    "A kiszamolom az SD-t a 4 (vagy több) replikatum között, és kiszamolom ugy is, hogy az egyik replikatumot kihagyom.\n",
    "(A wellek atlagai között számolok szórást)\n",
    "A rel_SD ennek a kettonek a hanyadosa, tehat azt mutatja, hogy hanyadára csökken a replikatumok szorasa, ha az adott well-t kihagyom.\n",
    "Ez log2 skálán van ábrázolva, így a leglátványosabb az eredmény.\n",
    "Van kék regió is, ahol a szórás növekedett egy replikátum kihagyásávaaal, de ez minig enyhe növekedés, így csak halvány kék."
    
  ))

out_dir0<-"out/2020_11_18-outlier_wells/"
dir.create(out_dir0)
#ggsave(filename = paste0(out_dir0,"outlier_wells-heatmap-replicate_wells_vs_trait_rel_SD.pdf"), width = 16*1.5,height = 160,limitsize = FALSE)

plot_histogram_1<-
  well_score_tbl %>% ggplot(mapping = aes( x=score1)) +
  geom_histogram(breaks = seq(from = -0.5,to = 60.5,by = 1))+
  geom_rug()+
  geom_vline(xintercept = treshold_score1)+
  ggrepel::geom_label_repel(max.iter = 5000,
                            data=well_score_tbl %>%
                              filter(score1>treshold_score1) %>%
                              group_by(score1) %>%
                              summarise(label1=paste(paste( genotypeID, plateID, well), collapse = "\n") , .groups="drop")  ,
                            mapping = aes(y=80, label=label1),
                            alpha=0.4,
                            size=2,
                            direction="y")+
  labs(caption=paste(
    "Az x tengelyen mért score azt mutatja, hogy egy sorban a heatmapen hány piros mezo van.\n",
    "Azt szamolom piros mezonek, ahol rel_sd < 0.5, vagyis ahol legalább 2-szeresére no a szórás az adott well használatától."
  ))



plot_histogram_2<-
  well_score_tbl %>% ggplot(mapping = aes( x=score2)) +
  geom_histogram(binwidth = 0.025)+
  geom_rug()+
  geom_vline(xintercept = treshold_score2)+
  ggrepel::geom_label_repel(
    data =
      well_score_tbl %>%
      filter(score2>treshold_score2) %>%
      group_by(score2) %>%
      summarise(label1=paste(label, collapse = "\n"), .groups="drop"),
    mapping = aes(y=80, label=label1),
    max.iter = 5000,
    size=2,
    alpha=0.5,
    direction="y")+
  labs(caption=paste(
    "Kb ugyanaz, de itt az x tengelyen mért score az =mean(1/log2(rel_sd)) keplettel van kiszamitva\n",
    "Ez azt jelenti, hogy a heatmapban egy sorban lévő színek által reprezentált értékek átlaga"
  ))

plot_scatter_1<-
  well_score_tbl %>% ggplot(mapping = aes( x=score2, y=score1, color=color)) +
  geom_vline(xintercept = treshold_score2, color="gray")+
  geom_hline(yintercept = treshold_score1, color="gray")+
  ggrepel::geom_label_repel(
    data =well_score_tbl %>%  filter(color!="blue" & (score2>treshold_score2 | score1>treshold_score1)) ,
    mapping = aes(label=label2),
    max.iter = 5000,
    alpha=0.35,
    direction="both")+
  geom_point()+
  scale_color_identity()+
  labs(caption="Osszehasonlitom, hogy a ketfele score hogy korrelál")



ggsave(
  plot = cowplot::plot_grid(plot_heatmap_1, plot_histogram_1, plot_histogram_2,plot_scatter_1, ncol = 1, rel_heights = c(20,1,1,2)),
  filename = paste0(out_dir0,"outlier_wells-heatmap-replicate_wells_vs_trait_rel_SD.pdf"),
  width = 16*1.5,height = 180,limitsize = FALSE)

write_rds(well_score_tbl, paste0(out_dir0,"outlier_wells-score-tbl.rds"))

write_rds(rel_sd_tbl1, paste0(out_dir0,"relative_sd_tbl.rds"))

well_score_tbl2<- well_score_tbl %>% mutate(is_ok= (color!="red")) %>% select(-label, -label2, -color)
write_rds(well_score_tbl2, "data/preprocess_calmorph_output/outlier_wells-score_tbl.rds", compress = "gz")
my_xlsx_save(well_score_tbl2,file = "data/preprocess_calmorph_output/morphology_tbls.xlsx",sheet = "outlier_well_tbl")


tbl_grouped_by_plates<- well_score_tbl2 %>% filter(!is_ok) %>% 
  group_by(plateID) %>%  summarise(cnt_of_outlier_wells=n(),
                                   genotypeID_list=paste(genotypeID, collapse = ";"),
                                   gene_list=paste(paste0(deletion_gene,"_", gsub("^(...).*$","\\1",ANCvsEVO) ), collapse = ";"),
                                   .groups="drop")

tbl_grouped_by_genotypes<- well_score_tbl2 %>% 
  group_by(genotypeID) %>%  summarise(
                                     gene=paste0(deletion_gene[[1]],"_", ANCvsEVO[[1]] ),
                                    cnt_of_outlier_wells=sum(!is_ok),
                                    cnt_of_ok_wells=sum(is_ok),
                                    outlier_positions=paste(paste0(plateID[!is_ok],"/", well[!is_ok]), collapse = ";"),
                                   .groups="drop") %>% 
  arrange(desc(cnt_of_outlier_wells))


write_rds(tbl_grouped_by_plates,"data/preprocess_calmorph_output/outlier_well-gr_by_plates.rds")
write_rds(tbl_grouped_by_genotypes,"data/preprocess_calmorph_output/outlier_well-gr_by_genotypes.rds")
# my_xlsx_save(tbl_grouped_by_plates,file = "data/preprocess_calmorph_output/morphology_tbls.xlsx",sheet = "outlier_well-gr_by_plates")
# my_xlsx_save(tbl_grouped_by_genotypes,file = "data/preprocess_calmorph_output/morphology_tbls.xlsx",sheet = "outlier_well-gr_by_genotypes")

#######################################################
# 
# WT<-"SZ2014_YMP_255"
# 
# well_score_tbl_WT<-well_score_tbl %>%  filter(genotypeID==WT)
# 
# hline_positions<-which(
#   well_score_tbl_WT %>% 
#     mutate(x=plateID != lag(plateID)) %>% 
#     .$x
# )
# 
# 
# plot_heatmap_1<-
#   rel_sd_tbl1 %>%  filter(genotypeID==WT) %>%
#   #mutate(lab1=sprintf("%i",round(log2(1/rel_sd))))
#   
#   ggplot(mapping = aes(y=paste(genotypeID,plateID,well), x=paste(trait,Dgroup),fill=log2(1/rel_sd)))+
#   geom_raster()+
#   geom_hline(yintercept = hline_positions-0.5)+
#   theme(legend.position = "top")+
#   #scale_fill_gradient2(midpoint = 0,limits=c(-3,2), trans="log")
#   scale_fill_gradient2(midpoint = 0, low = "blue", high = "red2")+ #,limits=c(0,2), )+
#   scale_y_discrete(limits=well_score_tbl_WT$label, labels=well_score_tbl_WT$label2)+
#   theme(axis.text.y = element_text(colour = well_score_tbl_WT$color))+
#   #scale_y_discrete(limits=trait_order1)+
#   theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 1))+
#   labs(caption=paste(
#     "Az abra azt vizsgalja, hogy az egyes well-ek kilognak-e az egyes genotipusok replikatumai közül.\n",
#     "A kiszamolom az SD-t a 4 (vagy több) replikatum között, és kiszamolom ugy is, hogy az egyik replikatumot kihagyom.\n",
#     "(A wellek atlagai között számolok szórást)\n",
#     "A rel_SD ennek a kettonek a hanyadosa, tehat azt mutatja, hogy hanyadára csökken a replikatumok szorasa, ha az adott well-t kihagyom.\n",
#     "Ez log2 skálán van ábrázolva, így a leglátványosabb az eredmény.\n",
#     "Van kék regió is, ahol a szórás növekedett egy replikátum kihagyásávaaal, de ez minig enyhe növekedés, így csak halvány kék."
#     
#   ))
# 
# 
# plot_histogram_1<-
#   well_score_tbl_WT %>%
#   ggplot(mapping = aes( x=score1)) +
#   #geom_histogram(breaks = seq(from = -0.5,to = 60.5,by = 1))+
#   geom_histogram()+
#   geom_rug()+
#   geom_vline(xintercept = treshold_score1)+
#   ggrepel::geom_label_repel(max.iter = 5000,
#                             data=well_score_tbl %>%
#                               filter(score1>treshold_score1) %>%
#                               group_by(score1) %>%
#                               summarise(label1=paste(paste( genotypeID, plateID, well), collapse = "\n") , .groups="drop")  ,
#                             mapping = aes(y=80, label=label1),
#                             alpha=0.4,
#                             size=2,
#                             direction="y")+
#   labs(caption=paste(
#     "Az x tengelyen mért score azt mutatja, hogy egy sorban a heatmapen hány piros mezo van.\n",
#     "Azt szamolom piros mezonek, ahol rel_sd < 0.5, vagyis ahol legalább 2-szeresére no a szórás az adott well használatától."
#   ))
# 
# 
# 
# plot_histogram_2<-
#   well_score_tbl_WT %>% ggplot(mapping = aes( x=score2)) +
#   geom_histogram(binwidth = 0.001)+
#   geom_rug()+
#  # geom_vline(xintercept = treshold_score2)+
#   # ggrepel::geom_label_repel(
#   #   data =
#   #     well_score_tbl_WT  %>%
#   #     filter(score2>treshold_score2) %>%
#   #     group_by(score2) %>%
#   #     summarise(label1=paste(label, collapse = "\n"), .groups="drop"),
#   #   mapping = aes(y=80, label=label1),
#   #   max.iter = 5000,
#   #   size=2,
#   #   alpha=0.5,
#   #   direction="y")+
#   labs(caption=paste(
#     "Kb ugyanaz, de itt az x tengelyen mért score az =mean(1/log2(rel_sd)) keplettel van kiszamitva\n",
#     "Ez azt jelenti, hogy a heatmapban egy sorban lévő színek által reprezentált értékek átlaga"
#   ))
# 
# plot_scatter_1<-
#   well_score_tbl %>% ggplot(mapping = aes( x=score2, y=score1, color=color)) +
#   geom_vline(xintercept = treshold_score2, color="gray")+
#   geom_hline(yintercept = treshold_score1, color="gray")+
#   ggrepel::geom_label_repel(
#     data =well_score_tbl %>%  filter(color!="blue" & (score2>treshold_score2 | score1>treshold_score1)) ,
#     mapping = aes(label=label2),
#     max.iter = 5000,
#     alpha=0.35,
#     direction="both")+
#   geom_point()+
#   scale_color_identity()+
#   labs(caption="Osszehasonlitom, hogy a ketfele score hogy korrelál")
# 
# 
# 
