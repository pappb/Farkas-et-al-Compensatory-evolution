# az eredmenyeket felmasolja a RAID-re.
# nehany nagy de masoknak szuksegteklen filet kihagy

target0<-"/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/TMP/res-"

library(lubridate)

# a datom es idpont segitsegevel csinal egy egyedi konytarnevet
dir_target<-paste0(target0,gsub("[^0123456789]","_",now()), "/" )

dir.create(dir_target, recursive = TRUE)
dir.create(paste0(dir_target,"data/"), recursive = TRUE)
dir.create(paste0(dir_target,"out/"), recursive = TRUE)


file.copy("out", dir_target, recursive=TRUE)

list1<-list.files("data/preprocess_calmorph_output/",include.dirs = TRUE)
list1<-setdiff(list1,c("histogra_data_of_images","data-collected_all_cell_level_data_from_calmorph_output"))

for( i in seq(list1))
{
  file.copy( paste0("data/preprocess_calmorph_output/", list1[[i]]), paste0(dir_target,"data/"),recursive=TRUE)
}


