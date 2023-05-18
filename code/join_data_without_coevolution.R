# clean up
rm(list = ls())

# requires
require(dplyr)

# define empty dataframe
df <- NULL

# find all files
file_names_truncated <- list.files("/home/fernando/coevolution_robustness/output/no_coevolution/truncated/summarised_output/no_coevolution/",
                        full.names = TRUE)

file_names_uniform <- list.files("/home/fernando/coevolution_robustness/output/no_coevolution/uniform/summarised_output/no_coevolution/",
                                   full.names = TRUE)

file_names <- c(file_names_truncated, file_names_uniform)

# read each file and append to dataframe
for (file in file_names) {

  # read data
  current_data <- read.csv(file)

  # append to dataframe
  df <- rbind(df, current_data)

}

# rename columns
metrics_no_coevolution <- df %>% select(connectance, num_components, fraction_species_largest_component,
                          average_size_other_components, robustness_lower, robustness_higher, alpha, replica, network_name, distribution)

metrics_no_coevolution <- metrics_no_coevolution %>% dplyr::rename(
  null_connectance = connectance,
  null_components = num_components,
  null_fraction_species_largest_component = fraction_species_largest_component,
  null_average_size_other_components = average_size_other_components,
  null_robustness_lower = robustness_lower,
  null_robustness_higher = robustness_higher
)


# store
save(metrics_no_coevolution, file = '/home/fernando/coevolution_robustness/results/metrics_without_coevolution.Rda')
