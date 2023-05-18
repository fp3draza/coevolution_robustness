# clean up
rm(list=ls())

# requires
require(dplyr)

# directories
dir_truncated <- '/home/fernando/coevolution_robustness/output/coevolution/truncated/summarised_output/'
dir_uniform <- '/home/fernando/coevolution_robustness/output/coevolution/uniform/summarised_output/'
network_shape <- read.csv('/home/fernando/coevolution_robustness/input/network_shapes.csv')

# list all subfolders
dir <- c(dir_truncated, dir_uniform)

# list all subfolders
list_of_files <- list.files(dir, full.names =  TRUE, recursive = TRUE)

# initialise empty
metrics <- NULL

# loop through each file
for (file in list_of_files) {
  # read data
  current_df <- read.csv(file)
  # bind to dataframe
  metrics <- rbind(metrics, current_df)
}

# join with network shapes
metrics <- metrics %>% left_join(network_shape)

# store
save(metrics, file = '/home/fernando/coevolution_robustness/results/metrics_with_coevolution.Rda')
