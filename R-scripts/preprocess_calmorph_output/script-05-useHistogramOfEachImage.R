graphics.off()

rm(list=ls())

library(tidyverse)
library(cowplot)

source("R-scripts/preprocess_calmorph_output/my_parameters.R") # load the dir locations and parameters

out_data_dir0<-"data/preprocess_calmorph_output/"
out_data_dir1<-"data/preprocess_calmorph_output/histogra_data_of_images/";
dir.create(out_data_dir1)


out_dir1<-"out/2020_08-11-outlier_filter-NewAssay/";
dir.create(out_dir1,recursive = TRUE)


####################################x####################################x####################################x


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
stopifnot(!any(duplicated( screen_metadata_tbl$YMP_folder_name )))
stopifnot(!any(duplicated( screen_metadata_tbl$YMP_exp_name)))
####################################x####################################x####################################x

dir1<-"data/preprocess_calmorph_output/histogra_data_of_images/"


filelist1<-list.files(dir1)
filelist1<-filelist1[filelist1 %in% paste0(screen_metadata_tbl$YMP_plate_ID,".rds")]

my_list<-list()
for(f1 in filelist1)
{
  #f1 <- list.files(dir1)[[1]]
  cat(f1,"\n")
  plateID1<-gsub("^(.+)\\.rds$","\\1",f1)
  
  # is_ok<-plates_tbl %>% filter(plateID==plateID1) %>% .$use_for_outlier_filter_image_level
  # if(length(is_ok)==1 && is_ok){
    tbl1<-  read_rds(paste0(dir1,f1)) %>% add_column(plateID=plateID1, .before = 1)
    my_list[[length(my_list)+1]]<-tbl1
  # }else{
  #   cat("  skipped\n")
  # }
}
tbl1<-do.call(bind_rows, my_list)
rm(my_list)
print(object.size(tbl1),unit="GB")

write_rds(tbl1, "data/preprocess_calmorph_output/histogra_data_of_images-merged.rds", compress = "gz")
#tbl1<-read_rds(path = "data/preprocess_calmorph_output/histogra_data_of_images-merged.rds")

#############################
#############################

set2<-screen_metadata_tbl %>% filter(use_screen) %>% .$YMP_plate_ID
set1<-tbl1 %>% select(plateID) %>%  distinct() %>% .$plateID
cat("missing plates:\n  ", paste(setdiff(set2, set1), collapse = "\n   "))
cat("extra plates:\n  ", paste(setdiff( set1,set2), collapse = "\n   "))
rm(set1, set2)

#############################
#############################


tbl2<-
  full_join(
    tbl1 %>% filter(chanel=="dapi") %>% 
      group_by(plateID, well, image_number) %>%
      summarise(pixelCnt=sum(cnt),
                meanDapi=sum(cnt*value)/pixelCnt ,
                cntHotPixelsDapi=cnt[value==255],
                cntBlackPixelsDapi=cnt[value==255],
                darkAreaRatioDapi=sum(cnt[value<10])/pixelCnt,
      ) %>% ungroup(),
    tbl1 %>% filter(chanel=="conA") %>% 
      group_by(plateID, well, image_number) %>%
      summarise(pixelCnt=sum(cnt),
                darkAreaRatioConA=sum(cnt[value<=10])/pixelCnt,
                cntHotPixelsConA=cnt[value==255],
                cntBlackPixelsConA=cnt[value==0],
                meanConA=sum(cnt*value)/pixelCnt,
      ) %>%  select(-pixelCnt)%>% ungroup(),
    by = c("plateID", "well", "image_number"))

write_rds(tbl2, "data/preprocess_calmorph_output/summary_of_histogram_data_of_images.rds", compress = "gz")
#tbl2<-read_rds(path = "data/preprocess_calmorph_output/summary_of_histogram_data_of_images.rds")

print(object.size(tbl2),unit="MB")


######################################################

tresholdUpper_ConA <- params$image_filter_treshold_ConA_upper
tresholdUpper_Dapi <- params$image_filter_treshold_dapi_upper

pdf(file = paste(out_dir1,"jpg-image-level-outlier-info.pdf"), width = 16, height = 9)

plot_1<-plot_grid(
  tbl2 %>% 
    ggplot(aes(x=meanDapi))+
    geom_histogram(breaks=seq(0,255,1))+
    geom_rug()+
    scale_x_continuous(breaks=seq(0,255,10), limits = c(0,255), expand = c(0,0))+
    geom_vline(xintercept = tresholdUpper_Dapi, color="red", linetype="dotted")
  ,
  tbl2 %>%
    ggplot(aes(x=meanDapi))+
    geom_histogram(breaks=seq(0,255,1))+
    geom_rug()+
    geom_vline(xintercept = tresholdUpper_Dapi, color="red", linetype="dotted") +
    scale_x_continuous(breaks=seq(0,255,10), limits = c(0,255), expand = c(0,0))+
    scale_y_log10()+
    labs(title="log Y scale")
  ,
  tbl2 %>%
    ggplot(aes(x=meanDapi))+
    stat_ecdf()+
    geom_rug()+
    scale_x_continuous(breaks=seq(0,255,10), limits = c(0,255), expand = c(0,0))+
    geom_vline(xintercept = tresholdUpper_Dapi, color="red", linetype="dotted") +
    labs(title="CDF")
  ,
  ncol = 1)
print(plot_1)

tbl2 %>% mutate(ok= (meanDapi<tresholdUpper_Dapi) ) %>% group_by(ok) %>% summarise(cnt=n(), percent=100*n()/nrow(tbl2))


plot_2<-plot_grid(
  tbl2 %>% 
    ggplot(aes(x=meanConA))+
    geom_histogram(breaks=seq(0,255,1))+
    geom_rug()+
    geom_vline(xintercept = tresholdUpper_ConA, color="red", linetype="dotted")+
    scale_x_continuous(breaks=seq(0,255,10), limits = c(0,255), expand = c(0,0))+
    labs(title="histogram"),
  tbl2 %>%
    ggplot(aes(x=meanConA))+
    geom_histogram(breaks=seq(0,255,1))+
    geom_rug()+
    scale_x_continuous(breaks=seq(0,255,10), limits = c(0,255), expand = c(0,0))+
    geom_vline(xintercept = tresholdUpper_ConA, color="red", linetype="dotted") +
    scale_y_log10()+
    labs(title="log Y scale"),
  tbl2 %>%
    ggplot(aes(x=meanConA))+
    stat_ecdf()+
    geom_rug()+
    scale_x_continuous(breaks=seq(0,255,10), limits = c(0,255), expand = c(0,0))+
    geom_vline(xintercept = tresholdUpper_ConA, color="red", linetype="dotted") +
    labs(title="CDF"),
  ncol = 1)
print(plot_2)

plot_3<-tbl2 %>% ggplot(aes(x=meanConA, y=meanDapi)) +geom_point(size=0.3)+
  geom_vline(xintercept = tresholdUpper_ConA, color="red", linetype="dotted") +
  geom_hline(yintercept = tresholdUpper_Dapi, color="red", linetype="dotted") +
  scale_x_continuous(breaks=seq(0,255,10), limits = c(0,255), expand = c(0,0))+
  scale_y_continuous(breaks=seq(0,255,10), limits = c(0,255), expand = c(0,0))+
  labs(caption=paste0("Minden kép-kocka egy pont, vagyis well-enként 52 pont van\n",
                      "A piros vonalak jelolik a trehold ertekeket amiken kivul outliernek jelolom az adott kepet"))
print(plot_3)

# tbl2 %>%
#   filter(meanConA<tresholdUpper_ConA & meanDapi<tresholdUpper_Dapi) %>% 
#   ggplot(aes(x=meanConA, y=meanDapi, color=plateID)) +
#   geom_point(size=0.3)+
#   stat_ellipse(mapping = aes(group=plateID), color="gray20")+
#   #  geom_hex(bins=64)+
#   geom_vline(xintercept = tresholdUpper_ConA, color="red", linetype="dotted") +
#   geom_hline(yintercept = tresholdUpper_Dapi, color="red", linetype="dotted") +
#   # scale_x_continuous(breaks=seq(0,255,10), limits = c(0,255), expand = c(0,0))+
#   # scale_y_continuous(breaks=seq(0,255,10), limits = c(0,255), expand = c(0,0))+
#   #  scale_fill_continuous(trans="log")+
#   labs(caption="Minden kép-kocka egy pont, vagyis well-enként 52 pont van")

plot_4<-tbl2 %>%
  #filter(meanConA<tresholdUpper_ConA & meanDapi<tresholdUpper_Dapi) %>% 
  ggplot(aes(x=meanConA, y=meanDapi, color=well)) +
  geom_abline(intercept=0, slope=c(0.5,1,2),color="gray80" )+
  geom_vline(xintercept = 25, color="gray80" )+
  geom_hline(yintercept = 10, color="gray80" )+
  geom_point(size=0.3)+
  #  geom_hex(bins=64)+
  geom_vline(xintercept = tresholdUpper_ConA, color="red", linetype="dotted") +
  geom_hline(yintercept = tresholdUpper_Dapi, color="red", linetype="dotted") +
   scale_x_continuous( limits = c(0,tresholdUpper_Dapi*1.8), expand = c(0,0))+
   scale_y_continuous( limits = c(0,tresholdUpper_ConA*1.8), expand = c(0,0))+
  #  scale_fill_continuous(trans="log")+
  facet_wrap(~plateID)+
  labs(caption=paste0("Ugyanaz mint az elobb, de csak a tresholdon tuli terulet nagy resz nincs mutatva.\n",
                      "Az egyes plate-ket külön facet-ba tettem."))
print(plot_4)


dev.off()


#########################################################################x

tbl2 %>% 
  ggplot(aes(x=darkAreaRatioDapi))+
  geom_histogram(breaks=seq(0,1,by=0.01))+
  #geom_rug()+
  #scale_x_continuous(breaks=seq(0,255,10), limits = c(0,255), expand = c(0,0))+
  geom_vline(xintercept = tresholdUpper_Dapi, color="red", linetype="dotted")

tbl2 %>% 
  ggplot(aes(x=darkAreaRatioDapi))+
  geom_histogram(breaks=seq(0,1,by=0.01))+
  scale_y_log10()
  coord_cartesian(ylim=c(0,500))

  tbl2 %>% 
    ggplot(aes(x=darkAreaRatioConA))+
    geom_histogram(breaks=seq(0,1,by=0.01))+
    scale_y_log10()
  coord_cartesian(ylim=c(0,500))
  
  tbl2 %>% 
    ggplot(aes(x=darkAreaRatioConA, y=darkAreaRatioDapi))+ 
    geom_abline(color="gray")+
    geom_point(size=0.3    )
    

  tbl2 %>% 
    ggplot(aes(x=darkAreaRatioConA, y=darkAreaRatioDapi))+ 
    geom_abline(color="gray")+
    # geom_hex(bins=100)+
    geom_point(size=0.3)
    # scale_fill_continuous(trans="log")+ scale_alpha_continuous()
  
  
  tbl2 %>% mutate(ok= darkAreaRatioConA<darkAreaRatioDapi) %>%  group_by(ok) %>%  summarise(cnt=n(), percent=100*n()/nrow(tbl2))
  
    #geom_rug()+
  #scale_x_continuous(breaks=seq(0,255,10), limits = c(0,255), expand = c(0,0))+
  #geom_vline(xintercept = tresholdUpper_Dapi, color="red", linetype="dotted")


plot_grid(
  tbl2 %>% filter(cntHotPixelsDapi>10) %>% 
    ggplot(aes(x=cntHotPixelsDapi))+
    geom_histogram()+
    geom_rug()+
    #scale_x_continuous(breaks=seq(0,255,10), limits = c(0,255), expand = c(0,0))+
    geom_vline(xintercept = tresholdUpper_Dapi, color="red", linetype="dotted")
  ,
  tbl2 %>%
    ggplot(aes(x=cntHotPixelsDapi))+
    geom_histogram()+
    geom_rug()+
    geom_vline(xintercept = tresholdUpper_Dapi, color="red", linetype="dotted") +
    #scale_x_continuous(breaks=seq(0,255,10), limits = c(0,255), expand = c(0,0))+
    scale_y_log10()+
    labs(title="log Y scale")
  ,
  tbl2 %>%
    ggplot(aes(x=cntHotPixelsDapi))+
    stat_ecdf()+
    geom_rug()+
    scale_x_continuous(breaks=seq(0,255,10), limits = c(0,255), expand = c(0,0))+
    geom_vline(xintercept = tresholdUpper_Dapi, color="red", linetype="dotted") +
    labs(title="CDF")
  ,
  ncol = 1)

tresholdLower_DarkAreaRatioDapi<-0.5
tresholdLower_DarkAreaRatioConA<-0.5
tresholdUpper_DarkAreaRatioDapi<-0.9999
tresholdUpper_DarkAreaRatioConA<-0.9999



tbl2<- tbl2 %>%  mutate(too_big_mean_dapi= (meanDapi> tresholdUpper_Dapi),
                        too_big_mean_conA= (meanConA> tresholdUpper_ConA),
                        too_small_dark_area_Dapi = (darkAreaRatioDapi<tresholdLower_DarkAreaRatioDapi),
                        too_small_dark_area_ConA = (darkAreaRatioConA<tresholdLower_DarkAreaRatioConA),
                        too_big_dark_area_Dapi = (darkAreaRatioDapi>tresholdUpper_DarkAreaRatioDapi),
                        too_big_dark_area_ConA = (darkAreaRatioConA>=tresholdUpper_DarkAreaRatioConA)
                        )

# ok=TRUE ha egyik too_xxx oszlop sem jeleze elterest
tbl2$ok<-tbl2 %>%  select(.vars=grep("^too_",names(tbl2),value=TRUE)) %>% as.matrix() %>% apply(MARGIN = 1, FUN = any) %>% !.


write_rds(tbl2, paste0(out_data_dir0,"tbl_of_outlier_images.rds"), compress = "gz")


summarise_tbl1<-tbl2 %>%  group_by_at(.vars=grep("^too_",names(tbl2),value=TRUE)) %>%    summarise(cnt=n(), percent=100*n()/nrow(tbl2))
source("script/script-03-tryGLMM/function-my_xlsx_save.R")

tbl3<-tbl2 %>% group_by( plateID, well ) %>%  summarise(cnt_of_images=n(),cnt_of_OK_images=sum(ok), cnt_of_outlier_images=sum(!ok) )

my_xlsx_save(tbl = summarise_tbl1,file = paste0(out_data_dir0,"tbl_of_outlier_images.xlsx"),sheet = "summarise")
my_xlsx_save(tbl = tbl3 %>% arrange(desc(cnt_of_outlier_images)) ,file = paste0(out_data_dir0,"tbl_of_outlier_images.xlsx"),sheet = "sumarise 2")
my_xlsx_save(tbl = tbl2 %>%  filter(!ok),file = paste0(out_data_dir0,"tbl_of_outlier_images.xlsx"),sheet = "tbl of outlier images")

var<-ls(pattern = "treshold")
tmp<-tibble(variable=var)%>%  mutate(value=sapply(variable,get))
my_xlsx_save(tbl = tmp,file = paste0(out_data_dir0,"tbl_of_outlier_images.xlsx"),sheet = "treshold values")


# 
# tbl2 %>% ggplot(aes(x=meanConA, y=meanDapi)) +geom_hex(bins=100)
# 
# 
# tbl2 %>%
#   ggplot(aes(x=meanConA))+geom_histogram(breaks=seq(0,255,1))+geom_rug()+ scale_x_continuous(breaks=seq(0,255,10))+
#   geom_vline(xintercept = 22, color="red", linetype="dotted")
# 
# 
# tbl2 %>%  ggplot(aes(x=cntHotPixelDapi))+geom_histogram(bins = 60)+geom_rug()+scale_x_log10()+
#   geom_vline(xintercept = 400, color="red", linetype="dotted")
# 
# tbl2 %>%  ggplot(aes(x=cntHotPixelConA))+geom_histogram(bins = 60)+geom_rug()+scale_x_log10()+
#   geom_vline(xintercept = 400, color="red", linetype="dotted")
# 
# 
# 
# sum(tbl2$meanDapi>9)
# mean(tbl2$meanDapi>=8)
# 
# tbl3<-tbl2 %>% mutate(too_high_dapi_mean=meanDapi>=8,
#                       too_high_conA_mean=meanDapi>=22,
#                       too_many_hot_dapi_pixel=cntHotPixelDapi>400,
#                       too_many_hot_conA_pixel=cntHotPixelDapi>400,
#                       ok=!too_high_dapi_mean & !too_high_conA_mean & !too_many_hot_dapi_pixel & !too_many_hot_conA_pixel
# ) %>%
#   group_by(plateID, well) %>% summarise(cnt_of_ok=sum(ok), cnt_of_non_ok=sum(!ok))
