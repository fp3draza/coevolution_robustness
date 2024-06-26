# load packages
library(tidyverse)
library(here)

###########################
####### functions #########
###########################

build_directories <- function(distribution, type_evo){
  
  # construct path to the requested output file
  return(here('output', distribution, type_evo))
  
}

list_files_from_path <- function(path_list){
  
  # list all files in the specified path
  return(list.files(path_list, full.names = TRUE, recursive = TRUE))
  
}


merge_output <- function(file_list, coevolution){
  
  if (coevolution) {
    # read each file in the file list
    return(readr::read_csv(file_list))
  }
  else {
    # read each file in the file list but select only relevant columns
    return(readr::read_csv(file_list, col_select = c(null_connectance = connectance, null_num_components = num_components, 
                                                     null_fraction_species_largest_component = fraction_species_largest_component,
                                                     null_average_size_other_components = average_size_other_components, 
                                                     null_robustness_lower = robustness_lower, 
                                                     null_robustness_higher = robustness_higher, 
                                                     alpha, replica, network_name, distribution)))
  }
  
}


###########################
##### read data ##########
###########################


# specify distributions
distributions <- c('truncated', 'uniform')

# build paths to output files for each type of evolution
mutualistic_paths <- mapply(build_directories, distributions, 'mutualistic_coevolution')
antagonistic_paths <- mapply(build_directories, distributions, 'antagonistic_coevolution')
no_coevolution_paths <- mapply(build_directories, distributions, 'no_coevolution')

# put all paths corresponding to coevolution simulations in a single vector
coevolution_paths <- c(mutualistic_paths, antagonistic_paths)

# list all files in each path
coevolution_files <- list_files_from_path(coevolution_paths)
no_coevolution_files <- list_files_from_path(no_coevolution_paths)

# read all files in each path
coevolution_data <- merge_output(coevolution_files, TRUE)
no_coevolution_data <- merge_output(no_coevolution_files, FALSE) # specify that this output corresponds to the no coevolution treatment

###########################
##### merge data ##########
###########################

# merge the output of all simulations in a single data frame
metrics <- coevolution_data %>% dplyr::left_join(no_coevolution_data)

# load network metrics
network_shape <- read_csv(here('input', 'network_shapes.csv'))

# merge network data with simulation output
metrics <- metrics %>% left_join(network_shape)

###########################
##### wrangle data #########
###########################

# standardise connectance
metrics <- metrics %>%
  dplyr::mutate(
    minimum_connectance = if_else(num_resources >= num_consumers, 
                                  num_resources/(num_resources*num_consumers), 
                                  num_consumers/(num_resources*num_consumers)),
    connectance_standard = (connectance - minimum_connectance)/(1 - minimum_connectance),
    null_connectance_standard = (null_connectance - minimum_connectance)/(1 - minimum_connectance)
  )

# rename distributions and evolutionary treatments (for plots)
metrics <- metrics %>% 
  mutate(distribution = if_else(distribution == 'truncated', 
                                'narrow niche', 'broad niche'),
         type_evo = if_else(type_evo == 'no_coevolution',
                            'no\ncoevolution',
                            if_else(type_evo == 'antagonistic_coevolution',
                                    'antagonistic\ncoevolution', 
                                    'mutualistic\ncoevolution'))) %>%
  rename(robustness = robustness_lower,
         null_robustness = null_robustness_lower)


###########################
##### store data #########
###########################
save(metrics, file = here('results', 'metrics.Rda'))

