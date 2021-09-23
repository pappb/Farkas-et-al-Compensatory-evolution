# The run_All.R script runs all the data preprocess steps.
# It has two versions: run_All.R and  run_All-B.R.
# The only difference is that the first version takes care about the ancestor-evolved relations. 
# the secound verion is used for datasets where there are no ancestor and evolved versions os strains.


# the output of CalMorph are some xls files for each  plate.
# The script collect the data into one single tibble saved to an rds file for each well, and store the files into subdirectories by the plates;
# then merge all the tables into a hough tibble called 'big_tbl'.
# The 'big_tbl' contains all the cell level information povided by the CalMorph.
source("R-scripts/preprocess_calmorph_output/script-01-collect_calmporph_output_tables.R")
source("R-scripts/preprocess_calmorph_output/script-02-create_big_tbl.R")

# There are some microscoope imaging errors. They are easy to recognise based on the histogram of images (mean conA and Dapi values, black area)
# We store the histogram of all the microscoope images;
# then based on the histogram we peek the bad images;
# then we collect all the bad images into summary pdf. These  pdf files help manually checking the procedure.
source("R-scripts/preprocess_calmorph_output/script-04-store_histogram_of_each_image.R")
source("R-scripts/preprocess_calmorph_output/script-05-useHistogramOfEachImage.R")
source("R-scripts/preprocess_calmorph_output/script-06-create_figure_about_images_marked_as_outliers.R")

# The 'big_tbl' contains a lot of information we do not need. (E. G. graphical paths, cell rows out od A, A1,B and  C Dgroups, data from the 'bad images' selected in the prevoius step. )
# I remove the unnneded rows and colums. I only retrive the walues of the morphological traits and the informations for identifiing the cell rows (plate,well, cellID, genotype)
# The resulted tibble is much smaller and easier to computationally managge.
# It is callec 'samller_big_table'. It is our main dtata table. In the next steps we will complement it some additional columns.
source("R-scripts/preprocess_calmorph_output/script-07-big_tbl_to_smaller_big_tbl.R")

# Outlier filtering: we remove the rows of cells which are extreme
source("R-scripts/preprocess_calmorph_output/script-08-01-qualty_control_and_cell_level_outlier_filter_on_Big_tbl.R")


source("R-scripts/preprocess_calmorph_output/script-09-apply_genotype_blacklist.R")

# we store two versions of the cell level data:
# (1) the smaller_big_tbl.rds contains all cells
# (2) the calmorph_data-step1-outlier_images_filtered-data_A.rds, calmorph_data-step1-outlier_images_filtered-data_A1B.rds and
# calmorph_data-step1-outlier_images_filtered-data_C.rds files contain the A, A1B and C phase cells separately.
# This script updates the secound version based on the first.
source("R-scripts/preprocess_calmorph_output/script-10-smaller_big_tbl_to_data_AA1BC.R")# refresh the tables

# I collect a lot of information abot the traits (columns) E.g. range, rate of NA-s...
# I mark which traits will be used by the following analysis
source("R-scripts/preprocess_calmorph_output/script-11-01-prepare_for_trait_description_tbl.R")
source("R-scripts/preprocess_calmorph_output/script-11-02-create_trait_description_tbl-V2.R")

# remove rows which contains NA-s in the traits we use
# remove all cells of wells where are not enough cell for reliable statistics
# remove all cells of genotypes which has not enough replicates for reliable statistics
source("R-scripts/preprocess_calmorph_output/script-12-01-delete_rows_containing_NA.R")
source("R-scripts/preprocess_calmorph_output/script-12-02-delete_wells_with_low_cell_cnt.R")
source("R-scripts/preprocess_calmorph_output/script-12-03B-delete_genotypes_not_enough_replicates.R") # This call DIFFERES from the rum_all.R version

# If 3 of 4 replicates are simmilar and the 4-th differs (increases the variance too much ) then we consider well as outlier and delete it
source("R-scripts/preprocess_calmorph_output/script-13-01-mark_outlier_wells.R")
source("R-scripts/preprocess_calmorph_output/script-13-02-delete_marked_outlier_welles_on_big_tbl.R")

# a few genotypes have more than 4 replicates( wells). The extra replicates will be removed to balance out the experiment.
source("R-scripts/preprocess_calmorph_output/script-14-delete_replicates_over_4.R")

source("R-scripts/preprocess_calmorph_output/script-10-smaller_big_tbl_to_data_AA1BC.R")# refresh the tables again

# Creates 'clean' data tables. The smaller_big_tbl data table contains some rows marked for deletions and some unimportant columns.
# These rows and columns are needed for debugging the workflow but not needed for the later data analysis.
# In the clean data tables these rows and columns will be deleted.
source("R-scripts/preprocess_calmorph_output/script-15-create_supplemantary_table_of_cell_data.R")

# generates 100 images of individual cells for each genotype
# these are used for figures
source("script/script-14-Cell_images/script-02-generate_100_cell_images_per_genotipes_ordered_by_cellsize.R") # generates images about 100 individual cells per genotype


