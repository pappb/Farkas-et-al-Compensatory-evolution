graphics.off()

rm(list=ls())
gc(reset = TRUE,full = TRUE)

library(tidyverse)
source("script/script-03-tryGLMM/function-my_xlsx_save.R")
####################################x####################################x####################################x
plate_layout_tbl_0<-read_csv("data/preprocess_calmorph_output/YMP_tables/YMP_plate_layout.csv",
                           col_types = cols(
                             YMP_plate_ID = col_character(),
                             YMP_well = col_character(),
                             genotype_ID = col_character(),
                             cultivation_number = col_character(),
                             is_control = col_logical(),
                             use_strain = col_logical()
                           ))

screen_metadata_tbl<-read_csv("data/preprocess_calmorph_output/YMP_tables/YMP_screen_metadata.csv")
                           
plate_layout_tbl <- plate_layout_tbl_0 %>%
  filter(use_strain) %>% 
  semi_join(screen_metadata_tbl %>% filter(use_screen ), by="YMP_plate_ID")


###################
tbl_of_outlier_images <- readRDS("data/preprocess_calmorph_output/tbl_of_outlier_images.rds")
#plate_map_tbl <- readRDS("data/preprocess_calmorph_output/plate_map_tbl.rds")
big_tbl <- readRDS("data/preprocess_calmorph_output/big_tbl.rds")

cat("eredeti big_tbl méret:")
print(object.size(big_tbl), unit="GB")


cnt_of_cells_tbl <-
  full_join(
    big_tbl %>%
      group_by(plateID, well) %>%
      summarise(cnt01_all_calmporh_detected_celles=n(),
                cnt01_A=sum(Dgroup=="A"),
                cnt01_A1B=sum(Dgroup=="A1" | Dgroup=="B" ),
                cnt01_C=sum(Dgroup=="C")
      ) %>% 
      ungroup() ,
    plate_layout_tbl ,
  by = c("plateID"="YMP_plate_ID", "well"="YMP_well")
  )



cat("Kidobom a complex-eket.")

big_tbl<-big_tbl %>% filter(Dgroup =="A" | Dgroup =="A1" |Dgroup =="B" |Dgroup =="C" )

cat("big_tbl méret miutan a complex-ektol megszabadultunk:")
print(object.size(big_tbl), unit="GB")


cnt_of_cells_tbl <-
  full_join(
    big_tbl %>%
      group_by(plateID, well) %>%
      summarise(cnt02_celles_after_deleting_complexes=n(),
                cnt02_A=sum(Dgroup=="A"),
                cnt02_A1B=sum(Dgroup=="A1" | Dgroup=="B" ),
                cnt02_C=sum(Dgroup=="C")
      ) %>% 
      ungroup() ,
    cnt_of_cells_tbl ,
    by = c("plateID", "well")
  )



##############################################

# Kidobom a karakteres oszlopokat, amikben grafikus paht-ok vannak

tmp<-sapply(big_tbl,typeof)
colname_list1<-c("plateID", "well", "Cgroup", "Dgroup", names(tmp)[tmp!="character"])
cat("Ezeket az oszlopokat dobom ki:\n  ",paste0(setdiff(colnames(big_tbl),colname_list1), collapse = "\n  "),"\n")
big_tbl<- big_tbl %>% select(all_of(colname_list1))
cat("big_tbl méret miutan a folosleges karakteres oszlopokat toroltuk:")
print(object.size(big_tbl), unit="GB")
gc()

#####################
#save.image(file="tmp-allasmentes-178687129.Rdata")

# rm(list=ls())
# load(file="tmp-allasmentes-178687129.Rdata")
#####################

tbl_of_outlier_images2 <-
    tbl_of_outlier_images %>% 
      select(plateID, well, image_number, ok) %>%
      rename(is_not_image_level_outlier=ok)

# amik a big_tbl-ben benne vannak, de az outlierszuresbol hianyoznak. Idealis esetben ures
test_tbl1<-anti_join(big_tbl, tbl_of_outlier_images2, by=c("plateID", "well", "image_number")) %>%
  select(plateID, well, image_number) %>%
  distinct()
test_tbl1 %>% select(plateID) %>%  distinct()

# itt vannak azok, amiről van outlier szuresi adat, de a big_tbl-ben nincs hoza passzolo adat
# van sok ilyen
# vannak kepek, amiket a calmorph nem dolgozott fel "blur of cell image" hibauzenettel
# az is lehet, hogy nem volt rajta felismergető sejt,
# szoval  azok, hogy ez a tabla nem ures
#
#TODO megnyugtató lenne a táblázatot osszevetni a clamorph hibauzeneteivel 
test_tbl2<-anti_join( tbl_of_outlier_images2, big_tbl, by=c("plateID", "well", "image_number"))
my_xlsx_save(test_tbl2,file = "data/preprocess_calmorph_output/morphology_tbls.xlsx",sheet = "images_calmoph_can_not_process")

##################################################################



# a töröltem azokat amikre nincs image outlier szuresi adat, ami a sejtek 7%-at tunteti el
N1<-nrow(big_tbl)

stopifnot(nrow(tbl_of_outlier_images2 %>% select(plateID, well, image_number) %>% distinct() )==nrow(tbl_of_outlier_images2 ))
#mean(tbl_of_outlier_images2$is_not_image_level_outlier)
big_tbl<-inner_join( tbl_of_outlier_images2,big_tbl, by=c("plateID", "well", "image_number"))
N2<-nrow(big_tbl)
cat(N1 ," --> ", N2 , "( ratio=", N2/N1," )\n" )   # ha ugyanannyi a ke tertek az OK. Azt jelenti minden sorra van adat a kep szintu outliarszurobol


# a kepek 0.5%-a van outliernek jelolve. Ezek torlese 0.3%-at torli a sejteknek, ami teljesen OK
big_tbl<-big_tbl %>%  filter(is_not_image_level_outlier) %>% select(-is_not_image_level_outlier)
N3<-nrow(big_tbl)
cat(N2 ," --> ", N3 , "Ratio=", N3/N2, "deleted=", 100*(1-(N3/N2)),"%\n" )  
msg<- paste("A mikoszkopos jpg  kepek",  100*(1-mean(tbl_of_outlier_images$ok)), "%-a van törlesre jelolve a kep szintu outlierszuro altal.",
            " Ez az adatsorok (sejtek) ",100*(1-(N3/N2))," %-at torli.")
cat(msg)

cat("big_tbl méret az image szintu outliersures utan:")
print(object.size(big_tbl), unit="GB")



cnt_of_cells_tbl <-
  full_join(
    big_tbl %>%
      group_by(plateID, well) %>%
      summarise(cnt03_celles_after_deleting_bad_jpg_images=n(),
                cnt03_A=sum(Dgroup=="A"),
                cnt03_A1B=sum(Dgroup=="A1" | Dgroup=="B" ),
                cnt03_C=sum(Dgroup=="C")
      ) %>% 
      ungroup() ,
    cnt_of_cells_tbl ,
    by = c("plateID", "well")
  )

cnt_of_cells_tbl <-  cnt_of_cells_tbl %>%
  mutate_at(.vars = grep("^cnt\\d+_.*",names(cnt_of_cells_tbl), value = TRUE),
            .funs = function(x) case_when(is.na(x)~as.integer(0),TRUE~x))

  
#######################################################################

# Itt mar nem kellene eltunnie soroknak. Hova lesznek?


 
#cnt_of_cells_tbl <-  full_join(plate_layout_tbl ,big_tbl %>%  group_by(plateID, well) %>%  summarise(n=n()) , by = c("YMP_plate_ID"="plateID", "YMP_well"="well"))

tbl1<-big_tbl %>% select(plateID, well) %>%  distinct()




test_tbl3 <- anti_join(tbl1, plate_layout_tbl, by = c("plateID"="YMP_plate_ID", "well"="YMP_well"))
test_tbl4 <- anti_join(plate_layout_tbl, tbl1, by = c("YMP_plate_ID"="plateID", "YMP_well"="well"))
test_tbl4 <- test_tbl4 %>% filter(!grepl("^BLANK\\d*$",genotype_ID) & genotype_ID!="SIGMA")
#XXX

my_xlsx_save(test_tbl4,file = "data/preprocess_calmorph_output/morphology_tbls.xlsx", sheet = "wells_witout_any_cells")

# ezt az ellenorzest is kiveszem, mert van pár SIGMA ahol mégis talalt sejteket
# inkabb kiveszem azokat a sejteket
# stopifnot(nrow(test_tbl3)==0) 
big_tbl2<- anti_join(big_tbl, test_tbl3, by = c("plateID", "well"="well"))

#stopifnot(nrow(test_tbl4)<=3) # amik rajta vannaka layout fileon, de nem találtunk sejteket... ezekből nem kéne soknak lenni
#stopifnot(nrow(test_tbl4)==0) TODO 0-nak kene lennie, de szokott 3 maradni


big_tbl2<-right_join(plate_layout_tbl %>%
                      select(YMP_plate_ID, YMP_well, genotype_ID ) %>% 
                      rename(plateID=YMP_plate_ID, well=YMP_well, genotypeID=genotype_ID),
                    big_tbl2 ,
                    by = c("plateID", "well"))

N4<-nrow(big_tbl2)
#stopifnot(N3==N4)

# itt megint azok a képek jottek ki, amiknel a calmorph hibat dobott 1519 ilyen kép van
# hogy kerul ebbe image_number  ?
missing_data_tbl<- big_tbl2 %>% filter(is.na(Dgroup)) %>%  select(plateID, well,  genotypeID, image_number )
missing_data_tbl_aggr<-
  missing_data_tbl %>%
      group_by(plateID,well,genotypeID ) %>%
      summarise(
        cnt_of_missing_images=n(), 
        image_numbers= paste(image_number, collapse="," )   ) %>% 
      ungroup() %>% 
      arrange(plateID,well)
  
my_xlsx_save(missing_data_tbl_aggr,file = "data/preprocess_calmorph_output/morphology_tbls.xlsx", sheet = "missing_images")

big_tbl2<- big_tbl2 %>% filter(!is.na(Dgroup))
N5<-nrow(big_tbl2)

write_rds(big_tbl2,"data/preprocess_calmorph_output/smaller_big_tbl.rds", compress = "gz")


write_rds(cnt_of_cells_tbl,"data/preprocess_calmorph_output/cnt_of_cells_while_data_filtering_tbl.rds", compress = "gz")
my_xlsx_save(cnt_of_cells_tbl,file = "data/preprocess_calmorph_output/morphology_tbls.xlsx", sheet = "cnt_of_cells_while_data_filtering")

