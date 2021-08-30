source("script/script-08-preprocess_calmorph_output/script-01-collect_calmporph_output_tables.R")
 
source("script/script-08-preprocess_calmorph_output/script-02-create_big_tbl.R")
#source("script/script-08-preprocess_calmorph_output/script-04-store_histogram_of_each_image.R")
source("script/script-08-preprocess_calmorph_output/script-05-useHistogramOfEachImage.R")
source("script/script-08-preprocess_calmorph_output/script-06-create_figure_about_images_marked_as_outliers.R")

source("script/script-08-preprocess_calmorph_output/script-07-big_tbl_to_smaller_big_tbl.R")
source("script/script-08-preprocess_calmorph_output/script-08-01-qualty_control_and_cell_level_outlier_filter_on_Big_tbl.R")

source("script/script-08-preprocess_calmorph_output/script-09-apply_genotype_blacklist.R")

source("script/script-08-preprocess_calmorph_output/script-10-smaller_big_tbl_to_data_AA1BC.R")
source("script/script-08-preprocess_calmorph_output/script-11-01-prepare_for_trait_description_tbl.R")
source("script/script-08-preprocess_calmorph_output/script-11-02-create_trait_description_tbl-V2.R")

source("script/script-08-preprocess_calmorph_output/script-12-01-delete_rows_containing_NA.R")
source("script/script-08-preprocess_calmorph_output/script-12-02-delete_wells_with_low_cell_cnt.R")
source("script/script-08-preprocess_calmorph_output/script-12-03B-delete_genotypes_not_enough_replicates.R") # itt ket verzio van, amelyik figyel az ancestorra, és amelyik nem

source("script/script-08-preprocess_calmorph_output/script-13-01-mark_outlier_wells.R")
source("script/script-08-preprocess_calmorph_output/script-13-02-delete_marked_outlier_welles_on_big_tbl.R")

source("script/script-08-preprocess_calmorph_output/script-14-delete_replicates_over_4.R")

source("script/script-08-preprocess_calmorph_output/script-10-smaller_big_tbl_to_data_AA1BC.R")# refresh the tables
source("script/script-08-preprocess_calmorph_output/script-15-create_supplemantary_table_of_cell_data.R")


source("script/script-14-Cell_images/script-02-generate_100_cell_images_per_genotipes_ordered_by_cellsize.R") # generates images about 100 individual cells per genotype

source("script/script-14-Cell_images/script-03B-select_representant_cells_for_each_well-mutidimensionalMedian-simple.R") 

source("script/script-08-preprocess_calmorph_output/script-50-upload_results_to_the_RAID.R")


# 
# GLMM
# 
# source("script/script-10-GLMM-normalisation-new_assay/script_03_01-create_joblist.R")
# source("script/script-10-GLMM-normalisation-new_assay/script_03_02-fit_GLMM_models.R")
# source("script/script-10-GLMM-normalisation-new_assay/script_03_03-check_success_of_model_fitting-2.R")
# source("script/script-10-GLMM-normalisation-new_assay/script_03_04-create_table_of_model_predictions.R")
# source("script/script-10-GLMM-normalisation-new_assay/script_03_05-cell_level_normalisation.R")
# source("script/script-10-GLMM-normalisation-new_assay/script_03_06-genotype_level_normalisation.R")

