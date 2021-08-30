# Ebben a scriptben ketfele outlier szurest csinalok. Az egyiket Quality controlnak hivom.
# 
# (1) quality control - Van néhány fix feltétel, amit ha nem teljesit egy sejt (adatsor) akkor torlesre jelolom.
# Ilyen feltetel pl a bud rovid tengelyenek a hossza. Az 50 feletti hossz irrealis. A calmorph egy tipushibaja okozza.
# A feltételek tresholdjait kezzel allapitottuk meg ugy hogy rengeteg sejt kepet kigyujtöttük különbözo tengelyhosszakkal, es
# kezzel megneztuk, hogy honnanstol hibas kb mind.
# 
# (2) Outlier filter - annak egy valtozata hogy az atlagtol 10SD elteres utan kidobjuk a sejteket.
# Ez az alapgondolat finomitva van ugy, hogy nem minden trait eseten alkalmazzuk ezt a szurest,
# illetve nem az SD alapjan hatarozzuk meg az elfogadhato intervallumot 
# (amikor ezt irom, meg nem talaltam ki a vegso modszert)

graphics.off()

rm(list=ls())

library(tidyverse)
source("script/script-03-tryGLMM/function-my_xlsx_save.R")


out_dir="out/2020_08_12-outlier_filter-newAssay/"
dir.create(out_dir)
####################################x####################################x####################################x

# plate_map_tbl<-read_rds("data/preprocess_calmorph_output/plate_map_tbl.rds")
# genotype_label_tbl<-plate_map_tbl %>% select(plateID, genotypeID, genotype_label) %>%  distinct()


big_tbl<- read_rds("data/preprocess_calmorph_output/smaller_big_tbl.rds")
print(object.size(big_tbl), units = "GB")

#big_tbl<-big_tbl[,!grepl("^is_",names(big_tbl))]

big_tbl<-big_tbl %>%  mutate(
  is_marked_for_deletion_by_quality_control= 
      (!is.na(C108) & C108>50) |  # ha tul hosszu a bud keresztiranyu tengelye, akkor  rosszul ismerte fel a calmorph
      (!is.na(C114) & Dgroup=="C" & C114<0.75) |  # ha a bud rossz iranyba tojas alaku, akkor 2 sejt van egymas melle fenykepezve
      (!is.na(C114) & (Dgroup=="A1" | Dgroup=="B") & Cgroup!="small" & C114<0.4) | # ugyanaz mint az elozo, de ha nem latszik a masodik sejtmag, akkor extremebb a treshold
      FALSE
  )


cat( sprintf("A sejtek (adatsorok)  %0.2f %%-a torlodik a Quality Control által, ami %i db sejt.\n",
             mean(big_tbl$is_marked_for_deletion_by_quality_control) *100,
             sum(big_tbl$is_marked_for_deletion_by_quality_control) ))
#A sejtek (adatsorok)  1.10 %-a torlodik a Quality Control által, ami 47185 db sejt.


sum_tbl1<-big_tbl %>% group_by(genotypeID) %>% summarise(ratio_of_deleted_cells_by_qualiti_control=mean(is_marked_for_deletion_by_quality_control),
                                               cnt_of_deleted_cells_by_qualiti_control=sum(is_marked_for_deletion_by_quality_control) ,
                                               cell_cnt=n())

cowplot::plot_grid(

sum_tbl1 %>%
  select(genotypeID, cnt_of_deleted_cells_by_qualiti_control,cell_cnt) %>% 
  gather(key = "key", value = "cnt",-genotypeID) %>% 
  ggplot(mapping = aes(x=genotypeID, y=cnt, fill=key)) +
  geom_bar(stat = "identity", alpha=0.5)+
#  coord_cartesian(ylim=c(1,35000))+
  scale_y_log10()+
  theme(legend.position = "top", axis.text.x = element_text(angle = 90,size=4, vjust = 0.5)),

sum_tbl1 %>%
  ggplot(mapping = aes(x=genotypeID, y=ratio_of_deleted_cells_by_qualiti_control)) +
  geom_bar(stat = "identity")+
  theme(legend.position = "top", axis.text.x = element_text(angle = 90,size=4, vjust = 0.5)),

ncol=1,axis = "lr",align = "hv"

)

ggsave(filename = paste0(out_dir,"barplot-removed_by_qualty_control.pdf"),width = 16*1.5, height = 9*1.5)


# csak atrendezem az oszlopok sorrendjet
big_tbl<-big_tbl %>%
  mutate(is_ok=! is_marked_for_deletion_by_quality_control) %>% 
  select(plateID, well,  genotypeID, image_number,is_ok,is_marked_for_deletion_by_quality_control, everything())

# write_rds(big_tbl,"data/preprocess_calmorph_output/smaller_big_tbl.rds", compress = "gz")
# 
# cat("Done.\n")

#############################################
## betoltom mely trait-eket kell hasznalni az outlier szuresben
##


tbl_of_traits_1<-xlsx::read.xlsx(file = "data-source/trait_description_tbl-manually_edited_G.xlsx",
                       sheetIndex = 1,
                 colIndex = 1:2,
                 colClasses = c("character", "logical"))
 

#trait_list0
tbl_of_traits_2<-tbl_of_traits_1 %>% filter(use_in_outlier_filter) %>% 
  rename(trait_long=trait) %>% 
  mutate(trait=gsub("(^[^_]+)_(.+)$","\\1",trait_long) , Dgroup=gsub("(^[^_]+)_(.+)$","\\2",trait_long))
  
####################################################

main_colnames<-c("plateID",
  "well",                                
  "genotypeID",  
  #"image_number"                             
  # "is_ok" 
  #"is_marked_for_deletion_by_quality_control"
  "Dgroup",
   "cell_id")


#names(big_tbl)
#big_tbl %>%  group_by(plateID, well, cell_id) %>% summarise(n=n()) %>% ungroup() %>% summarise(max(n))


# Ez egy nagy meretu tabla, azokata trait-Dgroup párokat amik nem kellenek kiveszem, hogy kisebb menorian elferjek
long_format_tbl<-big_tbl %>%
  filter(!is_marked_for_deletion_by_quality_control) %>%  
  mutate(Dgroup=ifelse( (Dgroup=="A1" | Dgroup=="B") , "A1B", Dgroup)) %>% 
  select(c(main_colnames, unique(tbl_of_traits_2$trait))) %>%
  gather(key ="trait" , value = "value", - plateID, -well,-genotypeID,-Dgroup, -cell_id)

long_format_tbl<-long_format_tbl %>% semi_join(tbl_of_traits_2, by = c("Dgroup", "trait"))

###########################

p_of_sigma<-pnorm(1) #  ~0.8413  vagyis a 85% os quantilisnél lesz


aggregated_tbl1<-long_format_tbl %>% 
  group_by( plateID, genotypeID, trait,Dgroup) %>%
  summarise(
      n=n(),
      n_not_NA= sum(!is.na(value)),
      median1=median(value, na.rm = TRUE),
      mean1=mean(value, na.rm = TRUE),
      sd1=sd(value, na.rm = TRUE),
      mad1=median(abs(median1-value), na.rm = TRUE),
      quantiles=list(Q=quantile(value , probs = c(0.01,0.05,0.10, 1-p_of_sigma, 0.25, 0.75, p_of_sigma,0.90,0.95, 0.99) , na.rm=TRUE))
      # q01= quantile(value , probs = 0.01 , na.rm=TRUE),
      # q25= quantile(value , probs = 0.25 , na.rm=TRUE),
      # q75= quantile(value , probs = 0.75 , na.rm=TRUE),
      # q99= quantile(value , probs = 0.99 , na.rm=TRUE)
      ) %>%
  ungroup()

strain_tbl<-read_csv("data/preprocess_calmorph_output/YMP_tables/YMP_strain_list.csv",
                     col_types = cols(
                       genotype_ID = col_character(),
                       unique_ID = col_character(),
                       deletion_gene = col_character(),
                       deletion_ORF = col_character(),
                       ANCvsEVO = col_character(),
                       timepoint = col_character(),
                       evolutionary_line = col_character(),
                       ploidy = col_character(),
                       is_compensated_at_end_line = col_double(),
                       genome_sequence_line = col_double(),
                       microarray_profile_line = col_double(),
                       rank_by_fitness = col_double(),
                       evo_pop_plate = col_character(),
                       evo_pop_well = col_character(),
                       evo_pop_well2 = col_character()
                     ))

genotype_label_tbl <- strain_tbl %>%
  select(genotype_ID,unique_ID) %>%
  rename(genotypeID=genotype_ID,genotype_label=unique_ID) %>%
  distinct()

aggregated_tbl1<-aggregated_tbl1 %>%
  left_join(genotype_label_tbl, by=c( "genotypeID")) %>% 
  select(plateID, genotypeID, genotype_label, everything())

aggregated_tbl1<-aggregated_tbl1 %>% 
  mutate(
  q01=sapply(quantiles, FUN = function(x) {x[1]}),
  q05=sapply(quantiles, FUN = function(x) {x[2]}),
  q10=sapply(quantiles, FUN = function(x) {x[3]}),
  q_ca15=sapply(quantiles, FUN = function(x) {x[4]}),
  q25=sapply(quantiles, FUN = function(x) {x[5]}),
  q75=sapply(quantiles, FUN = function(x) {x[6]}),
  q_ca85=sapply(quantiles, FUN = function(x) {x[7]}),
  q90=sapply(quantiles, FUN = function(x) {x[8]}),
  q95=sapply(quantiles, FUN = function(x) {x[9]}),
  q99=sapply(quantiles, FUN = function(x) {x[10]}),
  ) %>% 
  select(-quantiles)


q95_per_sigma <- qnorm(0.95)

aggregated_tbl1<-aggregated_tbl1 %>%
  mutate(M_old             = qnorm(0.99^(1/n_not_NA)),
         sigma_low_old     = median1-q_ca15,
         sigma_high_old     = q_ca85-median1,
         treshold_low_old  = median1-M_old*sigma_low_old,
         treshold_high_old = median1+M_old*sigma_high_old,
         
         M             = qnorm(0.99^(1/n_not_NA)),
         sigma_low     = (median1-q05)/q95_per_sigma,
         sigma_high     = (q95-median1)/q95_per_sigma,
         treshold_low  = median1-M*sigma_low,
         treshold_high = median1+M*sigma_high
         )%>% 
  mutate(treshold_low=ifelse(n_not_NA>100,treshold_low,NA),
         treshold_high=ifelse(n_not_NA>100,treshold_high,NA)
         )


# aggregated_tbl1 %>% ggplot(aes(x=M_old, y=M))+
#   geom_smooth(method = "lm")+
#   geom_point() +
#   geom_abline(color="red")+
#   coord_equal()
# aggregated_tbl1 %>% ggplot(aes(x=M_old/M))+
#   geom_density()+
#   geom_vline(xintercept = 1)
# 
# aggregated_tbl1 %>%
#   filter(!is.na(treshold_high)) %>%
#   mutate(label=ifelse(sigma_high/sigma_high_old>3 & sigma_high>0000 , paste0(plateID,"\n", genotype_label) , NA)) %>% 
#   summarise(sum(!is.na(label)))
# 
# aggregated_tbl1 %>%
#   filter(!is.na(treshold_high)) %>%
#   #semi_join(plate_map_tbl %>% filter(use_for_paper), by=c("plateID", "genotypeID")) %>% 
#   mutate(label=ifelse(sigma_high/sigma_high_old>5 & sigma_high>10000 , paste0(plateID,"\n", genotype_label) , NA)) %>% 
#   ggplot(aes(x=sigma_high_old, y=sigma_high, label=label))+
#   geom_smooth(method = "lm")+
#   ggrepel::geom_label_repel()+
#   geom_abline(slope = 2:5,color="cyan")+
#   geom_abline(color="red")+
#   geom_point() +
#   coord_equal()
# aggregated_tbl1 %>% ggplot(aes(x=sigma_high_old/sigma_high))+
#   geom_density()+
#   geom_vline(xintercept = 1)


# aggregated_tbl1<-aggregated_tbl1 %>%
#   mutate(treshold_low=q25-4*(median1-q25),  treshold_high=q75+4*(q75-median1)) %>% 
#   mutate(treshold_low=ifelse(n_not_NA>50,treshold_low,NA),treshold_high=ifelse(n_not_NA>50,treshold_high,NA))

# aggregated_tbl1<-aggregated_tbl1 %>%  mutate(treshold_low=q01-0.5*(median1-q01),  treshold_high=q99+0.5*(q99-median1))
#aggregated_tbl1<-aggregated_tbl1 %>%  mutate(treshold_low=median1-9*mad1, treshold_high=median1+9*mad1)


# x<-rnorm(100000000)
#  median(abs(median(x)-x))
# 1/6.7



treshold_tbl<-aggregated_tbl1 %>% 
  filter(!is.na(treshold_high)) %>% 
  select( plateID, genotypeID,trait, Dgroup ,treshold_low,treshold_high)

tmp<-left_join(long_format_tbl ,treshold_tbl, by = c("plateID", "genotypeID", "Dgroup", "trait"))
stopifnot(nrow(tmp) == nrow(long_format_tbl))
tmp2<-tmp %>% 
        mutate(is_ok=  (value>treshold_low) & (value<treshold_high)) %>%
        mutate(is_ok= ifelse(is.na(is_ok), TRUE, is_ok)) 
tbl_of_outlier_filtered_cells<-tmp2%>% 
        #mutate(label= ifelse(is_ok, "", trait)) %>% 
        group_by( plateID,  well ,  cell_id) %>%  summarise(is_marked_for_deletion_by_outlier_filter=!all(is_ok))


outlier_filter_which_traits_are_failed <- 
  left_join(tmp2,tbl_of_outlier_filtered_cells, by = c("plateID", "well", "cell_id")) %>%
    filter(is_marked_for_deletion_by_outlier_filter)%>%
    select(-treshold_low,-treshold_high ,-value, -is_marked_for_deletion_by_outlier_filter) %>% 
    mutate(is_ok=!is_ok) %>% 
    spread(key=trait, value=is_ok) %>%  arrange(plateID, well, cell_id)
#outlier_filter_which_traits_are_failed
write_rds(outlier_filter_which_traits_are_failed,"data/preprocess_calmorph_output/outlier_filter_which_traits_are_failed.rds", compress = "gz")


tmp_aggr<-tmp2 %>% group_by(plateID, genotypeID, trait,Dgroup) %>% 
  summarise(n=n() , 
            cnt_too_high=sum(value>=treshold_high, na.rm = TRUE),
            cnt_too_low=sum(value<=treshold_low, na.rm = TRUE),
            cnt_NA=sum(is.na(value)),
            cnt_ok=sum(is_ok),
            ratio_too_high= cnt_too_high/n,
            ratio_too_low= cnt_too_low/n,
            ratio_NA=cnt_NA/n,
            ratio_ok=cnt_ok/n)

aggregated_tbl2<-full_join(aggregated_tbl1, tmp_aggr %>%  select(-n), by = c("plateID", "genotypeID", "trait", "Dgroup"))
stopifnot(nrow(aggregated_tbl1)==nrow(aggregated_tbl2) && nrow(aggregated_tbl2)==nrow(tmp_aggr))
rm(tmp, tmp2, tmp_aggr, aggregated_tbl1)

write_rds(aggregated_tbl2, "data/preprocess_calmorph_output/outlyer_tresholds_tbl.rds", compress = "gz")

cat( sprintf("A sejtek (adatsorok)  %0.2f %%-a torlodik az Outlier Filter által, ami %i db sejt.\n",
    mean(tbl_of_outlier_filtered_cells$is_marked_for_deletion_by_outlier_filter)*100,
    sum(tbl_of_outlier_filtered_cells$is_marked_for_deletion_by_outlier_filter)))


#names(big_tbl )

#big_tbl<-big_tbl %>% select(-is_marked_for_deletion_by_outlier_filter.y,-is_marked_for_deletion_by_outlier_filter.x)

##################################################################
## bejelolom a nagy adattablan melyek az outlier sejtek. Ha mar volt erre oslop, akkor eloszor torlom
##


if( "is_marked_for_deletion_by_outlier_filter" %in% names(big_tbl)){
  big_tbl<-big_tbl %>%  select(-is_marked_for_deletion_by_outlier_filter)
}
N<-nrow(big_tbl)
big_tbl<-full_join(big_tbl, tbl_of_outlier_filtered_cells, by = c("plateID", "well", "cell_id"))
stopifnot(N==nrow(big_tbl))

big_tbl<-big_tbl %>%
  mutate(is_ok=!is_marked_for_deletion_by_outlier_filter &! is_marked_for_deletion_by_quality_control) %>% 
  select(plateID, well, genotypeID, image_number,is_ok,is_marked_for_deletion_by_quality_control,is_marked_for_deletion_by_outlier_filter, everything())

##################################################################

#mean(big_tbl$is_ok)

summary_of_outlier_cnts <- big_tbl %>%
  group_by(is_marked_for_deletion_by_outlier_filter, is_marked_for_deletion_by_quality_control) %>% summarise(ratio=n()/nrow(big_tbl) , n=n() )
print(summary_of_outlier_cnts)
my_xlsx_save(summary_of_outlier_cnts,file = "data/preprocess_calmorph_output/morphology_tbls.xlsx",sheet = "outlier cnts")


outlier_cnt_aggregated_by_traits<-
                    aggregated_tbl2 %>%
                      group_by(trait,Dgroup) %>%
                      summarise(  cnt_too_high= sum(cnt_too_high),
                                  cnt_too_low= sum(cnt_too_low),
                                  cnt_NA= sum( cnt_NA),
                                  cnt_ok= sum( cnt_ok),
                                  n=sum(n),
                                  ratio_too_high= cnt_too_high/n,
                                  ratio_too_low= cnt_too_low/n,
                                  ratio_NA=cnt_NA/n,
                                  ratio_ok=cnt_ok/n)

outlier_cnt_aggregated_by_traits %>%
  select(trait,Dgroup,ratio_too_high,   ratio_too_low) %>% 
  gather(key = "key", value="ratio",-trait,-Dgroup) %>%
  ggplot(aes(y=ratio,x=trait, fill=key))+
    geom_bar(stat = "identity",position ="stack" )+
    facet_grid(Dgroup~., scales = "free_x")+
    theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  labs(caption="Egyes trait-ek menni outliert okoznak. Trukkos, mert van atfedes: ugyanaz a sejt lehet outlier egyszerre tobb trait miatt.")
ggsave(filename = paste0(out_dir,"outlier_cnt_aggregated_by_traits.pdf"),width = 16, height = 9)
write_rds(outlier_cnt_aggregated_by_traits, paste0(out_dir,"outlier_cnt_aggregated_by_traits.rds"), compress = "gz")



outlier_cnt_aggregated_by_genotype<-
  big_tbl %>%
  group_by(plateID, genotypeID) %>%
  summarise(  ratio_of_outliers=mean(!is_ok, na.rm = TRUE)) %>% 
  left_join(genotype_label_tbl)

# pdf(file = paste0(out_dir,"outlier_cnt_aggregated_by_genotype.pdf"),width = 1.3*16, height = 1.3*9)
# for(pag_i in 1:3){
plot1<-outlier_cnt_aggregated_by_genotype %>%
  ggplot(aes(y=ratio_of_outliers,x=genotype_label))+
    geom_bar(stat = "identity")+
    scale_y_continuous(breaks = seq(0,1,by=0.05) , labels = sprintf("%i%%", seq(0,100,by=5)))+
    facet_wrap(~plateID, scales = "free_x",ncol = 5)+
    theme(axis.text.x = element_text(angle=90, vjust=0.5,hjust=1, size = 7))+
    theme(axis.title.x = element_blank())+
    labs(caption = "Az egyes genotipusokban mennyire gyulnek fel az outlierek. Az a jó, ha kb azonos mennyisegben")
#print(plot1)
# }

ggsave(filename = paste0(out_dir,"outlier_cnt_aggregated_by_genotype.pdf"),plot = plot1,width = 1.3*16, height = 4*1.3*9)
write_rds(outlier_cnt_aggregated_by_genotype, paste0(out_dir,"outlier_cnt_aggregated_by_genotype.rds"), compress = "gz")




print(object.size(big_tbl), units = "GB")



write_rds(big_tbl,"data/preprocess_calmorph_output/smaller_big_tbl.rds", compress = "gz")

# long_format_tbl %>%
#   filter(trait=="C101" & Dgroup=="A"  )%>%
#   ggplot( mapping =  aes(x=value, color=genotypeID))+
#     geom_density()+
#   theme(legend.position = "none")

cat("Done.\n")
gc()
