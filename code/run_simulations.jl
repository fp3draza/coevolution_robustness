using Distributed
@everywhere using Random, Distributions, LinearAlgebra, DelimitedFiles, DataFrames, CSV, EcologicalNetworks, Distributed, Graphs, Statistics

#########################
###### FUNCTIONS ########
#########################

@everywhere function create_intial_network_fixed_connectance(num_resources, num_consumers, trait_sample, connectance, alpha)

    # build initial matrix
    f = init_adjacency_matrix(num_resources, num_consumers)

    # establish interactions based on trait values
    f = fill_out_interactions_fixed_connectance(f, trait_sample, connectance, alpha, num_resources, num_consumers)

    return f


end

@everywhere function create_intial_network_variable_connectance(num_resources, num_consumers)

    # build initial matrix
    f = init_adjacency_matrix(num_resources, num_consumers)

    return f


end


@everywhere function rewire_network_from_traits(num_resources, num_consumers, connectance, alpha, trait_values, type_connectance, replicate)

    if type_connectance == "fixed"
        # build initial matrix
        f = create_intial_network_fixed_connectance(num_resources, num_consumers, trait_values, connectance, alpha)
        # establish interactions based on trait values
        f = fill_out_interactions_fixed_connectance(f, trait_values, connectance, alpha, num_resources, num_consumers)
    end

    if type_connectance == "variable"
        # build initial matrix
        f = create_intial_network_variable_connectance(num_resources, num_consumers)
        # establish interactions based on trait values
        f = fill_out_interactions_variable_connectance(f, trait_values, alpha, num_resources, num_consumers, replicate)
    end

    return f

end

@everywhere function draw_initial_trait_values(num_resources, num_consumers, replicate, distribution)

    if distribution == "truncated"
      # truncated normal mean 5, sd 1 from 0 to 10
      distribution_to_draw_from = Truncated(Normal(5,1), 0, 10)
      # seed
      Random.seed!(replicate)
      # draw traits from distribution
      trait_sample = rand(distribution_to_draw_from, num_resources + num_consumers)
    end


    if distribution == "uniform"
      # uniform distribution on interaval 0 to 10
      distribution_to_draw_from = Uniform(0, 10)
      # seed
      Random.seed!(replicate)
      # draw traits from distribution
      trait_sample = rand(distribution_to_draw_from, num_resources + num_consumers)
    end

    return trait_sample

end


@everywhere function init_adjacency_matrix(num_resources, num_consumers)

    # create matrix of ones
    mat = ones(Int, num_resources, num_consumers)

    # tranform to adjacency matrix
    f = transform_to_adjacency(mat)

    return f

end


@everywhere function transform_to_adjacency(matrix_to_transform)

  return vcat(hcat(zeros(Int, size(matrix_to_transform,1),size(matrix_to_transform,1)), matrix_to_transform),hcat(transpose(matrix_to_transform), zeros(Int,size(matrix_to_transform,2),size(matrix_to_transform,2))))

end

@everywhere function draw_baseline_interactions(f, num_resources, num_consumers, trait_values, alpha)

    # calculate trait differences
    z_dif = transpose(f.*trait_values) - f.*trait_values

    # calculate interaction probability
    p_int = f.*exp.(-alpha.*(z_dif.^2))

    # keep only incidence matrix
    inc_matrix = p_int[1:num_resources, num_resources + 1: num_resources+num_consumers]

    # find pairs of species with highest probs by rows
    postion_highest_p_row  = findmax(inc_matrix, dims = 1)

    # find pairs of species with highest probs by columns
    postion_highest_p_column  = findmax(inc_matrix, dims = 2)

    # establish interaction betwen pairs of species with highest prob
    inc_matrix[postion_highest_p_row[2]] .= 1
    inc_matrix[postion_highest_p_column[2]] .= 1

    # return to adjacency
    p_int_after_baseline = transform_to_adjacency(inc_matrix)

    return p_int_after_baseline

end

@everywhere function fill_out_interactions_fixed_connectance(f, trait_values, connectance, alpha, num_resources, num_consumers)

    # draw baseline network
    p_int = draw_baseline_interactions(f, num_resources, num_consumers, trait_values, alpha)

    # keep only incidence matrix
    inc_matrix = p_int[1:num_resources, num_resources + 1: num_resources+num_consumers]

    # while target connectance has not been reached
    while sum(inc_matrix .== 1)/(num_resources * num_consumers) != connectance

        # convert ones to 0
        temp_matrix = inc_matrix.*(inc_matrix .!= 1)

        # find next largest prob
        next_highest = findmax(temp_matrix)

        # establish interaction
        inc_matrix[next_highest[2]] = 1

    end

    # reformat to adjacency matrix
    inc_matrix = inc_matrix.==1
    f = transform_to_adjacency(inc_matrix)

    return f

end

@everywhere function fill_out_interactions_variable_connectance(f, trait_values, alpha, num_resources, num_consumers, replicate)

    # draw baseline network
    p_int = draw_baseline_interactions(f, num_resources, num_consumers, trait_values, alpha)

    # keep only incidence matrix
    inc_matrix = p_int[1:num_resources, num_resources + 1: num_resources+num_consumers]

    # seed
    Random.seed!(replicate)

    # flip coins
    flipped_coins = rand(num_resources, num_consumers)

    # establish interactions
    inc_matrix = inc_matrix .> flipped_coins

    # reformat to adjacency
    f = transform_to_adjacency(inc_matrix)

    return f

end


@everywhere function initialise_parameters_for_simulation(num_resources, num_consumers, connectance,  m_mean, alpha_val, replicate, distribution)

        # draw initial trait values
        init = draw_initial_trait_values(num_resources, num_consumers, replicate, distribution)

        # set initial values to be same as environmental optimum
        theta = copy(init)

        # draw phi values
        phi_distribution = Normal(0.5, 0.01)

        # seed
        Random.seed!(replicate)
        phi = rand(phi_distribution, num_resources + num_consumers)

        # set m values
        m = fill(m_mean, num_resources + num_consumers)

        # set alpha
        alpha = alpha_val

        # set epsilon
        epsilon = 10

        # define max number of generations
        t_max = 10000

        # build the initial network from init traits
        f = rewire_network_from_traits(num_resources, num_consumers, connectance, alpha, init, "variable", replicate)

        return init, theta, alpha, phi, m, t_max, f, epsilon

end

@everywhere function simulate_coevolution(f, z, theta, alpha, m, phi, num_resources, num_consumers, epsilon, type_evo)

	# trait change due to coevolution
	z_dif = transpose(f.*z) - f.*z
	q = f.*(exp.(-alpha.*(z_dif.^2)))
	q_n = q./sum(q,dims = 2)
	q_m = q_n.* m

    # if antagonistic coevolution
    if type_evo == "antagonistic_coevolution"
        z_subset_og = z_dif[1:num_resources,(num_resources+1):num_resources + num_consumers]
        u = 1 .* (broadcast(abs, z_subset_og) .<= epsilon)
        z_subset_mod = copy(z_subset_og)
        z_subset_mod[findall(z_subset_og .>= 0)] = z_subset_og[findall(z_subset_og .>= 0)] .- epsilon
        z_subset_mod[findall(z_subset_og .< 0)] = z_subset_og[findall(z_subset_og .< 0)] .+ epsilon
        z_dif[1:num_resources,(num_resources+1):num_resources + num_consumers] = u .* z_subset_mod
    end

	sel_dif = q_m .* z_dif
	r_mut = phi .* sum(sel_dif,dims=2)
	r_env = phi .* (1 .- m) .* (theta .- z)
	z_new = z .+ r_mut .+ r_env

	return z_new

end

@everywhere function flip_interactions_and_simulate_coevolution(f, z, theta, alpha, m, phi, num_resources, num_consumers, epsilon, type_evo, fraction, strategy)

    # make a copy of the ajdacency matrix
    f_work = copy(f)

    # trait change due to coevolution
    z_dif = transpose(f_work.*z) - f_work.*z
    q_prior = exp.(-alpha.*(z_dif.^2))
    q_prior[q_prior .== 0] .= 1.0e-10
    q = f.*q_prior
    q_n = q./sum(q,dims = 2)
    q_m = q_n.* m


    # flip interactions
    f_work = flip_interactions(f_work, fraction, strategy, num_resources, num_consumers, type_evo)

    # turn f_work adjacency to incidence matrix
    bi_work = f_work[1:num_resources,(num_resources+1):num_resources + num_consumers]

    # find position of antagonistic interactions
    pos_antagonistic = findall(bi_work .== -1)

    # antogonism section
    z_dif_cp = copy(z_dif)
    z_subset_tmp = z_dif_cp[1:num_resources,(num_resources+1):num_resources + num_consumers]
    z_subset_og = z_subset_tmp[pos_antagonistic]
    u = 1 .* (broadcast(abs, z_subset_og) .<= epsilon)
    z_subset_mod = copy(z_subset_og)
    z_subset_mod[findall(z_subset_og .>= 0)] = z_subset_og[findall(z_subset_og .>= 0)] .- epsilon
    z_subset_mod[findall(z_subset_og .< 0)] = z_subset_og[findall(z_subset_og .< 0)] .+ epsilon
    z_subset_tmp[pos_antagonistic] = u .* z_subset_mod
    z_dif_cp[1:num_resources,(num_resources+1):num_resources + num_consumers] = z_subset_tmp


    # general coevolution
	sel_dif = q_m .* z_dif_cp
	r_mut = phi .* sum(sel_dif,dims=2)
	r_env = phi .* (1 .- m) .* (theta .- z)
	z_new = z .+ r_mut .+ r_env

	return z_new

end


@everywhere function compute_rewiring_events(network_past, network_current)

    # compute the number of links that are rewired
    num_rewiring_events = sum(network_past .!= network_current)

    return num_rewiring_events

end

@everywhere function compute_average_trait_change(traits_past, traits_current)

    # compute average trait change
    average_trait_change = mean(abs.(traits_current .- traits_past))

    return average_trait_change

end

@everywhere function compute_average_trait_matching(traits_current, network_current, alpha)

    # convert zeros to NA
    network_current = convert(Array{Union{Missing,Int64}}, network_current)
    network_current[network_current.===0] .= missing

    # compute trait differences
    matching_diff = [norm(traits_current[i]-traits_current[j]) for i in eachindex(traits_current), j in eachindex(traits_current)]
    matching = exp.(-alpha.*(matching_diff).^2)

    # calculate average trait matching of interacting species
    network_matching = mean(skipmissing(matching .* network_current))

    return network_matching

end

@everywhere function compute_average_theta_matching(traits_current, theta, alpha)

    # compute trait differences
    matching = exp.(-alpha.*(theta - traits_current).^2)

    # calculate average theta matching
    network_theta_matching = mean(skipmissing(matching))

    return network_theta_matching

end

@everywhere function update_dataframe(df, t, network_past, network_current, traits_past, traits_current, type_evo, alpha, type_connectance, num_resources, num_consumers, m_val, replica, id, theta, distribution)

    # compute metrics
    modularity, nestedness, connectance, num_components, fraction_species_largest_component, average_size_other_components, robustness_lower, robustness_higher = measure_network_structure(network_current, num_resources, num_consumers, replica, true)

    # update dataframe
    df[1, "modularity"] = modularity
    df[1, "nestedness"] = nestedness
    df[1, "connectance"] = connectance
    df[1, "num_components"] = num_components
    df[1, "fraction_species_largest_component"] = fraction_species_largest_component
    df[1, "average_size_other_components"] = average_size_other_components
    df[1, "robustness_lower"] = robustness_lower
    df[1, "robustness_higher"] = robustness_higher
    df[1, "m"] = m_val
    df[1, "alpha"] = alpha
    df[1, "replica"] = replica
    df[1, "network_name"] = id
	
    return df

end


@everywhere function update_traits_and_matrix(traits, matrix)

    # copy variables
    init = copy(traits)
    f = copy(matrix)

    return init, f


end

@everywhere function initialise_dataframe_for_results(t_max, type_evo, type_connectance, distribution, fraction, strategy)

    # initialise dataframe for results
    df = DataFrame(generation=1:1,
                      type_evo=type_evo,
                      distribution=distribution,
                      type_connectance=type_connectance,
                      fraction=fraction,
                      strategy=strategy,
                      rewiring_events=Vector{Union{Missing, Int64}}(missing, 1),
                      average_trait_change = Vector{Union{Missing, Float64}}(missing, 1),
                      average_trait_matching = Vector{Union{Missing, Float64}}(missing, 1),
                      average_theta_matching = Vector{Union{Missing, Float64}}(missing, 1),
                      modularity = Vector{Union{Missing, Float64}}(missing, 1),
                      nestedness = Vector{Union{Missing, Float64}}(missing, 1),
                      connectance = Vector{Union{Missing, Float64}}(missing, 1),
                      num_components = Vector{Union{Missing, Int64}}(missing, 1),
                      fraction_species_largest_component = Vector{Union{Missing, Float32}}(missing, 1),
                      average_size_other_components = Vector{Union{Missing, Float32}}(missing, 1),
                      robustness_lower = Vector{Union{Missing, Float64}}(missing, 1),
                      robustness_higher = Vector{Union{Missing, Float64}}(missing, 1),
                      m = Vector{Union{Missing, Float32}}(missing, 1),
                      alpha = Vector{Union{Missing, Float32}}(missing, 1),
                      replica = Vector{Union{Missing, Int64}}(missing, 1),
                      network_name = Vector{Union{Missing, String}}(missing, 1))

    return df

end

@everywhere function initialise_dataframe_for_components(t_max, type_evo, type_connectance, distribution)

    # initialise dataframe for results
    df = DataFrame(generation=1:t_max,
                      type_evo=type_evo,
                      distribution=distribution,
                      type_connectance=type_connectance,
                      num_components = Vector{Union{Missing, Int64}}(missing, t_max),
                      fraction_species_largest_component = Vector{Union{Missing, Float32}}(missing, t_max),
                      average_size_other_components = Vector{Union{Missing, Float32}}(missing, t_max),
                      number_of_rewiring_events = Vector{Union{Missing, Int64}}(missing, t_max),
                      average_trait_matching = Vector{Union{Missing, Float32}}(missing, t_max))

    return df

end

@everywhere function measure_network_structure(f, num_resources, num_consumers, replicate, convert)

    modularity = measure_modularity(f, replicate, num_resources, num_consumers, convert)

    nestedness = nestedness_Fortuna(f, num_resources, num_consumers, convert)

    connectance = measure_connectance(f, num_resources, num_consumers)

    num_components, fraction_species_largest_component, average_size_other_components = measure_components(f, num_resources, num_consumers)

    robustness_lower = robustness_to_extinctions(f, num_resources, num_consumers, "lower", 100)

    robustness_higher = robustness_to_extinctions(f, num_resources, num_consumers, "higher", 100)

    return modularity, nestedness, connectance, num_components, fraction_species_largest_component, average_size_other_components, robustness_lower, robustness_higher

end

@everywhere function measure_modularity(f, replicate, num_resources, num_consumers, convert)

    if convert
        # convert adjacency matrix to incidence matrix
        f = f[1:num_resources, num_resources + 1: num_resources+num_consumers]
    end

    # convert matrix to network
    net = BipartiteNetwork(f .== 1)

    # random communities
    n = repeat(3:12, outer=20)
    m = Array{Dict}(undef, length(n))

    # Each run returns the network and its modules
    # We discard the network, and assign the modules to our object
    for i in eachindex(n)
        _, m[i] = n_random_modules(n[i])(net) |> x -> brim(x...)
    end

    # calculate modularity for each random community
    q = map(x -> Q(net,x), m)
    # find commnuty with largest modularity
    # seed
    Random.seed!(replicate)
    optimal = rand(findall(q.== maximum(q)))
    # store largest modularity value
    modularity = q[optimal]

    return modularity

end

@everywhere function nestedness_Fortuna(B, num_resources, num_consumers, convert)

    if convert
        # convert adjacency matrix to incidence matrix
        B = B[1:num_resources, num_resources + 1: num_resources+num_consumers]
    end

    # nestedness of rows
    nested_rows = 0
    for i=1:size(B,1)
        for j=1:size(B,1)
            if j>i
                shared=sum(B[i,:].*B[j,:]) # sum of common interactions
                min_shared = min(sum(B[i,:]),sum(B[j,:])) # min of the degrees
                nested_rows = nested_rows+(shared/min_shared)
            end
        end
    end

    # nestedness of columns
    nested_columns = 0
    for i=1:size(B,2)
        for j=1:size(B,2)
            if j>i
                shared=sum(B[:,i].*B[:,j]) # sum of common interactions
                min_shared = min(sum(B[:,i]),sum(B[:,j])) # min of the degrees
                nested_columns = nested_columns+(shared/min_shared)
            end
        end
    end

    # nestedness of the network
    nestedness_network = (nested_rows+nested_columns)/((size(B,1)*(size(B,1)-1)/2)+(size(B,2)*(size(B,2)-1)/2))

    return nestedness_network
end

@everywhere function measure_connectance(f, num_resources, num_consumers)

    # convert to incidence matrix
    inc_matrix = f[1:num_resources, num_resources + 1: num_resources+num_consumers]

    # calculate connectance
    connectance = sum(inc_matrix .== 1)/(num_resources * num_consumers)

    return connectance

end

@everywhere function measure_components(f, num_resources, num_consumers)

  # convert adjacency to graph
  g = SimpleGraph(f)

  # compute components
  comp = connected_components(g)

  # number of components
  num_components = length(comp)

  if num_components == 1
      # fraction of species in largest
      frac_species_largest_component = (length(comp[1]))/(num_resources + num_consumers)
      average_size_other_components = 0
  end

  if num_components > 1
      # find component with largest number of species
      num_species_largest_comp = length(comp[argmax(length.(comp))])
      # get fraction
      frac_species_largest_component = (num_species_largest_comp)/(num_resources + num_consumers)
      # get the average size of the other components
      average_size_other_components = mean(length.(comp[1:end .!= argmax(length.(comp))]))

  end

  return num_components, frac_species_largest_component, average_size_other_components

end

@everywhere function extinctions(N::T, participant) where {T <: AbstractBipartiteNetwork}

 # Make a copy of the network to extinguish
 Y = [copy(N)]

 # While there is at least one species remaining...
 while richness(last(Y)) > 1

   if participant === "lower" # remove resource
     # Remove one species randomly
     remain = sample(species(last(Y); dims=1), richness(last(Y); dims=1)-1, replace=false)
     # Network of remaining species
     R = last(Y)[remain,:]

   elseif participant === "higher" # remove consumer
     # Remove one species randomly
     remain = sample(species(last(Y); dims=2), richness(last(Y); dims=2)-1, replace=false)
     # Network of remaining species
     R = last(Y)[:,remain] # when removing consumers

   end

   # Remove species without interactions
   simplify!(R)

   # Add the simplified network (without the extinct species) to collection
   push!(Y, copy(R))

 end

 return Y
end

@everywhere function robustness_to_extinctions(f, num_resources, num_consumers, participant, nrep)

  # convert to incidence matrix
  M_inc = f[1:num_resources, num_resources+ 1: num_resources+num_consumers]

  # Convert incidence matrix to bipartite object
  N = BipartiteNetwork(M_inc .== 1)

  # Initialise vectors for storing fraction of species removed (X) and remaining (Y)
  X = Float64[]
  Y = Float64[]

  # Simulate extionctions nrep times
  for i in 1:nrep

    # Simulate extinctions
    timeseries = extinctions(N, participant)

    if participant === "lower" # removing resource
      # Vectors of fraction of species removed and remaining
      fract_removed = 1.0.-richness.(timeseries; dims=1)./richness(N; dims=1) # when removing resources
      fract_remaining = richness.(timeseries; dims=2)./richness(N; dims=2)    # when removing resources

    elseif participant === "higher" # removing consumer
      # Vectors of fraction of species removed and remaining
      fract_removed = 1.0.-richness.(timeseries; dims=2)./richness(N; dims=2) # when removing consumers
      fract_remaining = richness.(timeseries; dims=1)./richness(N; dims=1)    # when removing consumers
    end

    # Store current replica
    append!(X, fract_removed)
    append!(Y, fract_remaining)

  end

  # Vector of species removed
  x = sort(unique(X))

  # Initialise vector of mean of species remaning
  y = zeros(Float64, length(x))

  # Get mean and standard deviation of species remaining across replicas
  for (i, tx) in enumerate(x)
    y[i] = mean(Y[X.==tx])
  end

  # Initialise area below curve = robustness to extionctions
  R = 0

  # Compute area below curve
  for i=2:length(y)
    R = R + 0.5*(y[i-1]+y[i])*(x[i]-x[i-1])
  end

  return R
end

@everywhere function flip_interactions(f, fraction, strategy, num_resources, num_consumers, steady_state_interaction_type)

    # turn adjacency to incidence
    mat = f[1:num_resources, (num_resources + 1): (num_resources + num_consumers)]

    # calculate number of interactions
    total_number_of_interactions = sum(mat)

    # calculate number of interactions to flip
    number_of_interaction_to_flip = round(total_number_of_interactions * fraction)

    # if random strategy, flip interactions at random
    if strategy == "random"
        flipped_matrix = flip_interactions_at_random(mat, number_of_interaction_to_flip, steady_state_interaction_type)
    end

    # if specialist strategy, flip the interactions of the most specialist species first
    if strategy == "specialist"
        flipped_matrix = flip_interactions_based_on_degree(mat, false, num_consumers, number_of_interaction_to_flip, steady_state_interaction_type)
    end

    # if generalist strategy, flip the interactions of the most generalist species first
    if strategy == "generalist"
        flipped_matrix = flip_interactions_based_on_degree(mat, true, num_consumers, number_of_interaction_to_flip, steady_state_interaction_type)
    end

    # rebuild ajdacency matrix
    f_flipped = vcat(hcat(zeros(Int, num_resources,num_resources), flipped_matrix),hcat(transpose(flipped_matrix), zeros(Int,num_consumers,num_consumers)))

    return f_flipped

end


@everywhere function flip_interactions_at_random(mat, number_of_interaction_to_flip, steady_state_interaction_type)

    # make copy of matrix
    matrix_to_flip = copy(mat)
    # find location of all interactions in matrix
    interaction_location = findall(x->x==1, matrix_to_flip)
    # sample however many interactions are needed
    interactions_to_flip = sample(interaction_location, Int64(number_of_interaction_to_flip), replace = false)

    # if dealing with an antagonistic network turn all the interactions to -1
    if steady_state_interaction_type == "antagonistic_coevolution"
        matrix_to_flip = matrix_to_flip .* -1
    end

    # change interaction type based on sample
    matrix_to_flip[interactions_to_flip] .= matrix_to_flip[interactions_to_flip] .* -1

    return matrix_to_flip

end

@everywhere function run_simulation(num_resources, num_consumers, connectance, type_evo, type_connectance, m_val, alpha_val, replica, id, distribution, fraction, strategy)

    # initalise parameters
    init, theta, alpha, phi, m, t_max, f, epsilon = initialise_parameters_for_simulation(num_resources, num_consumers, connectance, m_val, alpha_val, replica, distribution)

    # initialise dataframe
    df = initialise_dataframe_for_results(t_max, type_evo, type_connectance, distribution, fraction, strategy)

    if  type_evo == "no_coevolution"

      t = 1

      # write final values to file
      df = update_dataframe(df, t, f, f, init, init, type_evo, alpha_val, type_connectance, num_resources, num_consumers, m_val, replica, id, theta, distribution)


    else

      for t in 1:t_max

        # run the coevolution model for one generation
        traits_after_coevolution = flip_interactions_and_simulate_coevolution(f, init, theta, alpha, m, phi, num_resources, num_consumers, epsilon, type_evo, fraction, strategy)

        # calculate trait differences between timesteps
        delta_traits = mean(abs.(init .- traits_after_coevolution))

        # rewire network after coevolution
        rewired_network_after_coevolution = rewire_network_from_traits(num_resources, num_consumers, connectance, alpha, traits_after_coevolution, type_connectance, replica)

        # update traits and matrix
        init, f = update_traits_and_matrix(traits_after_coevolution, rewired_network_after_coevolution)

        if t == t_max || delta_traits < 0.000001

            # write final values to file
            df = update_dataframe(df, t, f, rewired_network_after_coevolution, init, traits_after_coevolution, type_evo, alpha, type_connectance, num_resources, num_consumers, m_val, replica, id, theta, distribution)

	    break

        end

      end

    end

    return df

end



@everywhere function extract_network_size_and_connectance(network)

    # extract matrix
    mat = network.edges

    # extract number of resources and consumers
    num_consumers, num_resources = size(mat)

    # extract connectance
    connectance = sum(mat .== 1)/(num_resources * num_consumers)

    return num_resources, num_consumers, connectance

end


@everywhere function run_routine_artifical(type_of_interaction, type_of_connectance, m, alpha, replicate, distribution, fraction, strategy)

    # define network shapes
    network_shapes = define_artifical_networks()

    # create empty data frame
    store = DataFrame()

    # loop through each network
    for row in 1:nrow(network_shapes)

        # extract shape and name
        name, size, num_resources, num_consumers = network_shapes[row, :]

        # run the simulation
        worker = run_simulation(num_resources, num_consumers, 0, type_of_interaction, type_of_connectance, m, alpha, replicate, name, distribution, fraction, strategy)
        append!(store, worker)

    end

    return store

end

@everywhere function define_artifical_networks()

    # define network names
    string_network = repeat(["network_"], outer = 30)
    network_names = map(string, string_network, 1:30)

    # define sizes
    sizes = vcat(repeat([5], 2), repeat([10], 3), repeat([15], 2),
                repeat([20], 3), repeat([25], 2), repeat([30], 3),
                repeat([35], 2), repeat([40], 3), repeat([45], 2),
                repeat([50], 3), repeat([55], 2), repeat([60], 3))

    # num resources
    resources = [2,3,5,6,4,10,5,10,15,5,15,10,15,20,10,20,15,20,30,10,25,20,25,30,20,30,25,30,40,20]


    # num consumers
    consumers = [3,2,5,4,6,5,10,10,5,15,10,15,15,10,20,15,20,20,10,30,20,25,25,20,30,25,30,30,20,40]


    # build dataframe
    df = DataFrame(network_name = network_names,
                      size = sizes,
                      num_resources = resources,
                      num_consumers = consumers)

    return df

end

#########################
##### MAIN PROGRAM ######
#########################


function run_simulations()

  for type_evo in ["no_coevolution", "antagonistic_coevolution", "mutualistic_coevolution"]

	for distribution in ["truncated", "uniform"]

        for fraction in [0]

            for m in [0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95]

                for alpha in [0.01000000, 0.01258925, 0.01584893, 0.01995262, 0.02511886, 0.03162278, 0.03981072, 0.05011872, 0.06309573, 0.07943282, 0.10000000, 0.12589254, 0.15848932, 0.19952623, 0.25118864, 0.31622777, 0.39810717, 0.50118723, 0.63095734, 0.79432823, 1.00000000]

                    # build vector of dataframes for each replicate run
                    values = @distributed (append!) for i in 1:30
                    [run_routine_artifical(type_evo, "variable", m, alpha, i, distribution, fraction, "random")]
                    end

                    # merge all dataframes in vector
                    df_store = vcat(values..., cols=:union)

                    # create file name
                    file_name = string("interaction_type_",type_evo,"_variable_connectance_m_",m,"_alpha_",alpha,".csv")

                    # create relative path
                    rel_path = string("output/",distribution,"/",type_evo,"/",file_name)

                    # current location
                    current_directory = @__DIR__

                    # store
                    CSV.write(string(current_directory,"/../",rel_path), df_store)

                    end

                end

            end

        end

    end

    return true

end



#########################
##### RUN PROGRAM ######
#########################
run_simulations()
