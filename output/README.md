## output

This folder will store the raw ouput from performing the simulations. Before running the simulations, you need to create folders to store the output. The general structure for storing output is the following:

`coevolution_robustness/output/name_of_simulation_treatment/name_of_trait_distribution/name_of_interaction_type/`.

The available options for simulation treatment are:

- coevolution
- no_coevolution

The available options for distributions are:

- truncated (this uses a truncated distribution to assign species traits values, this corresponds to the narrow niche treatment described in the manuscript)
- uniform (this uses a uniform distribuiton to assign species traits values, this corresponds to the broad niche treatment described in the manuscript

The available options for interaction type are:

- mutualistic
- antagonistic

Once the folders are created, the script `coevolution_robustness/code/run_simulations_with_coevolution.jl` or `coevolution_robustness/code/run_simulations_without_coevolution.jl` will populate the directories.
