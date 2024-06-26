# load packages
library(tidyverse)
library(here)
library(wesanderson)
library(paletteer)
library(patchwork)

# load data 
load(here('results', 'metrics.Rda'))

# functions
extract_raw_comparison_data <- function(data, metric_to_compare){
  
  # construct string for null version of metric
  null_metric_to_compare <- paste0('null_', metric_to_compare)
  
  # extract data corresponding to null metric
  null_data <- data %>% 
    dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
    dplyr::summarise_(no_coevolution = lazyeval::interp(~mean(x), x = as.name(null_metric_to_compare))) %>%
    select(no_coevolution) %>%
    gather(type_evo, metric, no_coevolution) %>%
    mutate(type_evo = 'before\ncoevolution')
  
  # extract data corresponding to observed metric
  observed_data <- data %>%
    dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
    dplyr::summarise_(metric = lazyeval::interp(~mean(x), x = as.name(metric_to_compare))) %>%
    select(metric, distribution) 
  
  # add information on distributions and merge
  null_data$distribution <- observed_data$distribution
  observed_data <- rbind(observed_data, null_data)
  
  return(observed_data)
  
}

build_comparison_figure <- function(data, metric_to_compare){
  
  # build data set and define colour palette
  data_for_plot <- extract_raw_comparison_data(data, metric_to_compare)
  pal_new <-  c("#E1BE6A", "#40B0A6")
  
  # construct plot
  fig <- ggplot(data = data_for_plot, aes(x = fct_relevel(type_evo,
                                                   "antagonistic\ncoevolution", 
                                                   "before\ncoevolution", 
                                                   "mutualistic\ncoevolution"), 
                                   y = metric, fill = distribution)) + 
    geom_boxplot(alpha = 0.7) + xlab('') + ggtitle(label = metric_to_compare) + theme_minimal() + ylab('') +
    theme(aspect.ratio = 1, legend.title = element_blank(),
          legend.text = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 12),
          strip.text.x = element_text(size = 10, face = 'bold.italic'),
          strip.text.y = element_text(size = 10, face = 'bold.italic'),
          plot.title = element_text(size = 10, face = 'bold', hjust = 0.5),
          axis.text = element_text(size = 10)) + coord_fixed() + scale_fill_manual(values=pal_new)
  
  return(fig)

}


extract_raw_correlation_data <- function(data, x_axis, y_axis){
  
  # summarise specified variables
  raw_correlation_data <- data %>% 
    dplyr::group_by(type_evo, network_name, m, alpha, distribution, size) %>% 
    summarise_(
      x = lazyeval::interp(~mean(x), x = as.name(x_axis)),
      y = lazyeval::interp(~mean(x), x = as.name(y_axis)),
      ratio_consumer_to_resource = lazyeval::interp(~x/y, x = as.name('num_consumers'), y = as.name('num_resources')))

  return(raw_correlation_data)
  
  
}

build_correlation_figure <- function(data, x_axis, y_axis){
  
  # build data set and define colour palette
  data_for_plot <- extract_raw_correlation_data(data, x_axis, y_axis)
  pal_type_int <- paletteer_d("lisa::MarcelDuchamp")[c(1,4)]
  
  # construct plot
  fig <- ggplot(data = data_for_plot, aes(x, y, col = type_evo)) + geom_point(size = 1, alpha = 0.5) +
    theme_minimal() + xlab(x_axis) +
    ylab(y_axis)    +
    facet_grid(~distribution) +
    guides(fill=guide_legend(title="p"), color = guide_legend(override.aes = list(size = 2)))  +
    theme(legend.position = 'bottom',aspect.ratio = 1,
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          strip.text.x = element_text(size = 10, face = 'italic'),
          strip.text.y = element_text(size = 10, face = 'italic'),
          plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
          axis.text = element_text(size = 10)) + scale_color_manual(values = pal_type_int)
  
  return(fig)
  
}


extract_difference_comparison_data <- function(data, metric_to_compare, compare_by){
  
  # if we want to compare interaction types, then remove data from truncated distribution
  if (compare_by == 'type_evo') {
    
    data <- data %>% filter(
      distribution != 'narrow niche'
    )
    
  }
  
  # construct string for null version of metric
  null_metric_to_compare <- paste0('null_', metric_to_compare)
  
  # calculate difference between coevolution and no coevolution treatments
  data_for_plot <- data %>% 
    dplyr::group_by(type_evo, network_name, m, alpha, distribution, size) %>%
    dplyr::summarise_(no_coevolution = lazyeval::interp(~mean(x), x = as.name(null_metric_to_compare)),
                      coevolution = lazyeval::interp(~mean(x), x = as.name(metric_to_compare)),
                      size = lazyeval::interp(~mean(x), x = as.name('size'))) %>%
    mutate(
      delta = ((coevolution - no_coevolution)/no_coevolution)
    )
  
  return(data_for_plot)
  
}

build_difference_comparison_figure <- function(data, metric_to_compare, compare_by){
  
  # build data set and define colour palette
  data_for_plot <- extract_difference_comparison_data(data, metric_to_compare, compare_by)
  
  if (compare_by == 'type_evo') {
    
    pal_new <- paletteer_d("lisa::MarcelDuchamp")[c(1,4)]
    fill_by_plot <- 'type_evo'
    compare_by_plot <- 'type_evo'
      
  }
  
  if (compare_by == 'both') {
    
    pal_new <- c("#E1BE6A", "#40B0A6")
    fill_by_plot <- 'distribution'
    compare_by_plot <- 'type_evo'
    
  }
  
  if (compare_by == 'distribution') {
    
    pal_new <-  c("#E1BE6A", "#40B0A6")
    fill_by_plot <- 'distribution'
    compare_by_plot <- 'distribution'
    
  }
  
  # construct plot
  fig <- ggplot(data = data_for_plot, aes_string(x = compare_by_plot, y = 'delta', fill = fill_by_plot))   +
    geom_boxplot(alpha = 0.7)  + ggtitle(label = paste0('relative change in ', metric_to_compare, '\n')) + theme_minimal() + ylab('') + xlab('') + 
    theme(aspect.ratio = 1, legend.title = element_blank(), legend.position = 'none',
          legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 14),
          strip.text.x = element_text(size = 12, face = 'bold.italic'),
          strip.text.y = element_text(size = 12, face = 'bold.italic'),
          plot.title = element_text(size = 14, face = 'bold', hjust = 0.5),
          axis.text = element_text(size = 12)) + coord_fixed() + scale_fill_manual(values=c(pal_new)) +
    geom_abline(slope = 0, intercept = 0)
  
  return(fig)
  
}


extract_difference_correlation_data <- function(data, metric_to_compare_x, metric_to_compare_y){
  
  # construct string for null version of metric
  null_metric_to_compare_x <- paste0('null_', metric_to_compare_x)
  null_metric_to_compare_y <- paste0('null_', metric_to_compare_y)
  
  # calculate difference between coevolution and no coevolution treatments
  data_for_plot <- data %>% 
    dplyr::group_by(type_evo, network_name, m, alpha, distribution, size) %>%
    dplyr::summarise_(no_coevolution_x = lazyeval::interp(~mean(x), x = as.name(null_metric_to_compare_x)),
                      coevolution_x = lazyeval::interp(~mean(x), x = as.name(metric_to_compare_x)),
                      no_coevolution_y = lazyeval::interp(~mean(x), x = as.name(null_metric_to_compare_y)),
                      coevolution_y = lazyeval::interp(~mean(x), x = as.name(metric_to_compare_y)),
                      size = lazyeval::interp(~mean(x), x = as.name('size')),
                      num_components = lazyeval::interp(~mean(x), x = as.name('num_components')),
                      null_num_components = lazyeval::interp(~mean(x), x = as.name('null_num_components'))) %>%
    mutate(
      delta_x = ((coevolution_x - no_coevolution_x)/no_coevolution_x),
      delta_y = ((coevolution_y - no_coevolution_y)/no_coevolution_y),
      delta_z = ((num_components - null_num_components)/null_num_components)
    )
  
  return(data_for_plot)
}




build_difference_correlation_figure <- function(data, x_axis, y_axis){
  
  # build data set and define colour palette
  data_for_plot <- extract_difference_correlation_data(data, x_axis, y_axis)
  pal_correlation <- paletteer_d("rcartocolor::Sunset")
  
  # construct plot
  fig <- ggplot(data = data_for_plot, aes(x = delta_x, y = delta_y, col = size)) + geom_point(size = 0.8, alpha = 0.8) +
    theme_minimal() + xlab(paste0('relative change in ', x_axis)) +
    ylab(paste0('relative change in ', y_axis)) + labs(color='number of species') +
    facet_wrap(~distribution, scales="free") +
    theme(legend.position = 'bottom', aspect.ratio = 1,
          legend.text = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          strip.text.x = element_text(size = 10, face = 'italic'),
          strip.text.y = element_text(size = 10, face = 'italic'),
          plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
          axis.text = element_text(size = 10)) + scale_colour_gradientn(colors=c(pal_correlation)) + geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0)
  
  return(fig)
  
}


extract_grid_data <- function(data){
  
  # compute averages and differences to build grids
  grid_data <- data %>%
    dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
    dplyr::summarise(connectance = mean(connectance),
                     robustness = mean(robustness),
                     null_connectance = mean(null_connectance),
                     null_robustness = mean(null_robustness),
                     components = mean(num_components), 
                     null_components = mean(null_num_components)) %>%
    dplyr::group_by(distribution, type_evo, m, alpha) %>%
    dplyr::summarise(
      median_connectance = median(connectance),
      median_robustness = median(robustness),
      median_components = median(components),
      null_median_connectance = median(null_connectance),
      null_median_robustness = median(null_robustness),
      null_median_components = median(null_components)) %>%
    mutate(
      delta_connectance = (median_connectance - null_median_connectance)/null_median_connectance,
      delta_robustness = (median_robustness - null_median_robustness)/null_median_robustness,
      delta_components = (median_components - null_median_components)/null_median_components,
      m = as.factor(m),
      alpha = as.factor(alpha)
    )
  
  # return
  return(grid_data)
  
  
}



build_grid_figure <- function(data, metric_to_compare, interaction_type, compare_by = 'distribution'){
  
  # check if filtering is needed
  if (interaction_type == 'antagonistic') {
    data <- data %>% filter(
      type_evo == 'antagonistic\ncoevolution')
  }
  
  else if (interaction_type == 'mutualistic'){
    data <- data %>% filter(
      type_evo == 'mutualistic\ncoevolution')
  }
  
  if (compare_by == 'interaction') {
    data <- data %>% filter(
      distribution != 'narrow niche'
    )
  }
  
  # build data set and define colour palette
  grid_data <- extract_grid_data(data)
  pal_two <- paletteer_d("rcartocolor::OrYel")
  
  # construct plot
  fig <- ggplot(data = grid_data,
                aes_string(x = 'm', y = 'alpha', fill = metric_to_compare)) +
    geom_tile(alpha = 0.9) +
    facet_grid(distribution~type_evo)  +
    scale_fill_gradientn(colours = pal_two) + theme_minimal() +
    scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) +
    scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
    xlab('strength of coevolution (m)') + ylab('strength of\nfunctional mechanism (\u03b1)')  +
    guides(fill=guide_legend(title="legend", reverse = TRUE)) + labs(subtitle = "") +
    theme(aspect.ratio = 1, legend.position = 'right',
          legend.title = element_blank(),
          legend.text = element_text(size = 11),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          strip.text.x = element_text(size = 12, face = 'italic'),
          strip.text.y = element_text(size = 12, face = 'italic'),
          plot.subtitle = element_text(size = 13, hjust = 0.5, face = 'bold'),
          axis.text = element_text(size = 12))
  
  return(fig)
}

build_correlation_figure_by_components <- function(data, x_axis, y_axis){
  
   # build data set and define colour palette
  grid_data <- extract_difference_correlation_data(data, x_axis, y_axis)
  pal_correlation <- paletteer_d("rcartocolor::Sunset")
  
  # construct plot
  fig <- ggplot(data = grid_data, aes(x = coevolution_x, y = coevolution_y, col = num_components)) + geom_point(size = 1, alpha = 0.5) +
    theme_minimal() + xlab('connectance') +
    ylab('robustness')    +
    facet_grid(~distribution) +
    guides(fill=guide_legend(title="p"))  +
    theme(legend.position = 'right', aspect.ratio = 1,
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          strip.text.x = element_text(size = 10, face = 'italic'),
          strip.text.y = element_text(size = 10, face = 'italic'),
          plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
          axis.text = element_text(size = 10)) +
    scale_colour_gradientn(colors=c(pal_correlation))
  
  return(fig)
  
  
}

build_difference_correlation_figure_by_components <- function(data, x_axis, y_axis){
  
  # build data set and define colour palette
  grid_data <- extract_difference_correlation_data(data, x_axis, y_axis)
  pal_correlation <- paletteer_d("rcartocolor::Sunset")
  
  # construct plot
  fig <- ggplot(data = grid_data, aes(x = delta_x, y = delta_y, col = delta_z)) + geom_point(size = 1, alpha = 0.5) +
    theme_minimal() + xlab('relative change in connectance') +
    ylab('relative change in robustness')    +
    facet_grid(~distribution) +
    guides(fill=guide_legend(title="p"))  +
    theme(legend.position = 'right', aspect.ratio = 1,
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          strip.text.x = element_text(size = 10, face = 'italic'),
          strip.text.y = element_text(size = 10, face = 'italic'),
          plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
          axis.text = element_text(size = 10)) +
    scale_colour_gradientn(colors=c(pal_correlation))
  
  return(fig)
  
  
}



build_difference_correlation_figure_by_delta_components <- function(data, x_axis, y_axis, compare_by = 'distribution', scale_plot = 'fixed'){
  
  # build data set and define colour palette
  grid_data <- extract_difference_correlation_data(data, x_axis, y_axis)
  pal_correlation <- paletteer_d("rcartocolor::Sunset")
  
  # construct plot
  fig <- ggplot(data = grid_data, aes(x = delta_x, y = delta_y, col = delta_z)) + geom_point(size = 1, alpha = 0.5) +
    theme_minimal() + xlab('relative change in connectance') +
    ylab('relative change in robustness') + 
    facet_wrap(sym(compare_by), scales = scale_plot) +
    guides(fill=guide_legend(title="p"))  +
    theme(legend.position = 'right', aspect.ratio = 1,
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          strip.text.x = element_text(size = 10, face = 'italic'),
          strip.text.y = element_text(size = 10, face = 'italic'),
          plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
          axis.text = element_text(size = 10)) +
    scale_colour_gradientn(colors=c(pal_correlation))
  
  return(fig)
  
  
}

build_correlation_figure_by_num_species <- function(data, x_axis, y_axis){
  
  # build data set and define colour palette
  grid_data <- extract_difference_correlation_data(data, x_axis, y_axis)
  pal_correlation <- paletteer_d("rcartocolor::Sunset")
  
  # construct plot
  fig <- ggplot(data = grid_data, aes(x = coevolution_x, y = coevolution_y, col = size)) + geom_point(size = 1, alpha = 0.5) +
    theme_minimal() + xlab('connectance') +
    ylab('robustness')    +
    facet_grid(~distribution) +
    guides(fill=guide_legend(title="p"))  +
    theme(legend.position = 'right', aspect.ratio = 1,
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          strip.text.x = element_text(size = 12, face = 'italic'),
          strip.text.y = element_text(size = 12, face = 'italic'),
          plot.subtitle = element_text(size = 12, hjust = 0.5, face = 'bold'),
          axis.text = element_text(size = 12)) +
    scale_colour_gradientn(colors=c(pal_correlation))
  
  return(fig)
  
  
}

build_difference_correlation_figure_by_num_species <- function(data, x_axis, y_axis, compare_by = 'distribution', scale_plot = 'fixed'){
  
  # build data set and define colour palette
  grid_data <- extract_difference_correlation_data(data, x_axis, y_axis)
  pal_correlation <- paletteer_d("rcartocolor::Sunset")
  
  # construct plot
  fig <- ggplot(data = grid_data, aes(x = delta_x, y = delta_y, col = size)) + geom_point(size = 1, alpha = 0.5) +
    theme_minimal() + xlab('relative change in connectance') +
    ylab('relative change in robustness') + 
    facet_wrap(sym(compare_by), scales = scale_plot) +
    guides(fill=guide_legend(title="p"))  +
    theme(legend.position = 'right', aspect.ratio = 1,
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          strip.text.x = element_text(size = 12, face = 'italic'),
          strip.text.y = element_text(size = 12, face = 'italic'),
          plot.subtitle = element_text(size = 12, hjust = 0.5, face = 'bold'),
          axis.text = element_text(size = 12)) +
    scale_colour_gradientn(colors=c(pal_correlation))
  
  return(fig)
  
  
}


build_correlation_figure_narrow_niche <- function(data, x_axis, y_axis, colour_by){
  
  # build data set and define colour palette
  data_for_plot <- extract_raw_correlation_data(data, x_axis, y_axis)
  data_for_plot <- data_for_plot %>%
    filter(distribution != 'narrow niche')
  
  
  # construct plot
  fig <- ggplot(data = data_for_plot, aes_string('x', 'y', col = colour_by)) + geom_point(size = 1, alpha = 0.5) +
    theme_minimal() + xlab(x_axis) +
    ylab(y_axis) +
    guides(fill=guide_legend(title="p"), color = guide_legend(override.aes = list(size = 2)))  +
    theme(legend.position = 'bottom',aspect.ratio = 1,
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          strip.text.x = element_text(size = 10, face = 'italic'),
          strip.text.y = element_text(size = 10, face = 'italic'),
          plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
          axis.text = element_text(size = 10)) 
  
  if (colour_by == 'type_evo') {
    pal <- paletteer_d("lisa::MarcelDuchamp")[c(1,4)]
    fig <- fig + scale_color_manual(values=pal)
  }
  
  if (colour_by == 'size') {
    pal <- paletteer_d("rcartocolor::Sunset")
    fig <- fig + scale_colour_gradientn(colors = pal)
  }
  
  return(fig)
  
}


###################
  # MAIN TEXT #
###################

###################
## FIGURE 2 #####

# produce figure 2
fig_2 <- build_difference_comparison_figure(metrics, 'robustness', 'type_evo')
fig_2

# store figure
ggsave(filename = here('figures', 'figure_2.pdf'),
       width=220, height=180, units="mm", dpi=600, device = cairo_pdf)

###################

###################
## FIGURE 3 ####

# produce figure

fig_3a <- build_correlation_figure_narrow_niche(metrics, 'connectance', 'robustness', 'size')
fig_3b <- build_correlation_figure_narrow_niche(metrics, 'connectance', 'robustness', 'type_evo')

fig_3a + fig_3b

# store plot
ggsave(filename = here('figures', 'figure_3.pdf'),
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)

###################

###################
## FIGURE 4 ####

# produce figure
fig_4 <- build_difference_correlation_figure_by_num_species(metrics, 'connectance', 'robustness', compare_by = 'type_evo', scale_plot = 'free')
fig_4  + labs(color='number of species') + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom') 

# store plot
ggsave(filename = here('figures', 'figure_4.pdf'),
       width=220, height=180, units="mm", dpi=600, device = cairo_pdf)

###################

###################
## FIGURE 5 #####

# produce figure 5
robustness_grid_anta_diff <- build_grid_figure(metrics, 'delta_robustness', 'antagonistic', compare_by = 'interaction')
robustness_grid_mut_diff <- build_grid_figure(metrics, 'delta_robustness', 'mutualistic', compare_by = 'interaction')
connectance_grid_anta_diff <- build_grid_figure(metrics, 'delta_connectance', 'antagonistic', compare_by = 'interaction')
connectance_grid_mut_diff <- build_grid_figure(metrics, 'delta_connectance', 'mutualistic', compare_by = 'interaction')
components_grid_anta_diff <- build_grid_figure(metrics, 'delta_components', 'antagonistic', compare_by = 'interaction')
components_grid_mut_diff <-  build_grid_figure(metrics, 'delta_components', 'mutualistic', compare_by = 'interaction')

top <- robustness_grid_anta_diff + robustness_grid_mut_diff + plot_annotation("relative change in robustness") & theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 14),
                                                                                                                       strip.text.y = element_blank())
mid <-  connectance_grid_anta_diff + connectance_grid_mut_diff + plot_annotation("relative change in connectance") & theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 14),
                                                                                                                           strip.text.y = element_blank())
bottom <- components_grid_anta_diff + components_grid_mut_diff +  plot_annotation("relative change in number of components") & theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 14),
                                                                                                                                     strip.text.y = element_blank())

wrap_elements(top) / wrap_elements(mid) / wrap_elements(bottom) + plot_layout(heights = c(1,1,1))

# store figure
ggsave(filename = here('figures', 'figure_5.pdf'),
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)


###################


###################
 # SUPPLEMENTARY #
###################

###################
## FIGURE S1 #####

# produce figure
s1a <- ggplot(data = metrics %>% group_by(network_name) %>%
                summarise(num_resources = mean(num_resources),
                          num_consumers = mean(num_consumers)),
              aes(x = num_resources + num_consumers)) + geom_histogram(bins = 10, alpha = 0.6) +
  theme_minimal() + xlab('number of species')    +
  theme(legend.position = 'right', aspect.ratio = 1,
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10))

s1b <-ggplot(data = metrics %>% group_by(network_name) %>%
               summarise(num_resources = mean(num_resources),
                         num_consumers = mean(num_consumers)),
             aes(x = num_consumers/num_resources)) + geom_histogram(bins = 10, alpha = 0.6) +
  theme_minimal() + xlab('consumer to resource ratio')    +
  theme(legend.position = 'right', aspect.ratio = 1,
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10))

s1a + s1b
# store figure
ggsave(filename = here('figures', 'figure_S1.pdf'),
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)

###################


###################
## FIGURE S3 #####

# produce figure S3A
fig_S3a <- build_comparison_figure(metrics, 'robustness')

# produce figure S3B
fig_S3b <- build_comparison_figure(metrics, 'connectance')

# produce figure S3C
fig_S3c <- build_correlation_figure(metrics, 'connectance', 'robustness')


fig_S3ab <- (fig_S3a + fig_S3b) +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = 'bottom')
fig_S3ab / fig_S3c

# store figure
ggsave(filename = here('figures', 'figure_S3.pdf'),
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)

###################

###################
## FIGURE S4 #####

# produce figure S4
robustness_grid_anta <- build_grid_figure(metrics, 'median_robustness', 'antagonistic')
robustness_grid_mut <- build_grid_figure(metrics, 'median_robustness', 'mutualistic')
connectance_grid_anta <- build_grid_figure(metrics, 'median_connectance', 'antagonistic')
connectance_grid_mut <- build_grid_figure(metrics, 'median_connectance', 'mutualistic')

(robustness_grid_anta + connectance_grid_anta)  /  (robustness_grid_mut + connectance_grid_mut)

# store figure
ggsave(filename = here('figures', 'figure_S4.pdf'),
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)

###################



###################
## FIGURE S5 #####


# produce figure S5
components_grid_mut_raw <- build_grid_figure(metrics, 'median_components', 'mutualistic')
components_grid_anta_raw <- build_grid_figure(metrics, 'median_components', 'antagonistic')
correlation_components_raw <- build_correlation_figure_by_components(metrics, 'connectance', 'robustness')
(components_grid_anta_raw + components_grid_mut_raw) / correlation_components_raw  +  plot_annotation("number of components") & theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# store figure
ggsave(filename = here('figures', 'figure_S5.pdf'),
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)

###################

###################
## FIGURE S6 #####


# produce figure S6
correlation_components_diff <- build_difference_correlation_figure_by_delta_components(metrics, 'connectance', 'robustness', compare_by = 'type_evo', scale_plot = 'free')
correlation_components_diff + geom_hline(yintercept = 0) +  geom_vline(xintercept = 0) + 
  plot_annotation("relative change in number of components") & theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# store figure
ggsave(filename = here('figures', 'figure_S6.pdf'),
       width=220, height=180, units="mm", dpi=600, device = cairo_pdf)

###################


###################
## FIGURE S7 #####

# produce figure S7A
fig_S7a <- build_difference_comparison_figure(metrics, 'robustness', 'both')

# produce figure S7B
fig_S7b <- build_difference_comparison_figure(metrics, 'connectance', 'both')

# produce figure S7C
fig_S7c <- build_difference_correlation_figure(metrics, 'connectance', 'robustness')


fig_S7ab <- (fig_S7a + fig_S7b) +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = 'bottom')
fig_S7ab / fig_S7c

# store figure
ggsave(filename = here('figures', 'figure_S7.pdf'),
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)

###################
## FIGURE S8 ####

# generate figure

fig_S8a <- build_grid_figure(metrics, 'delta_robustness', 'antagonistic')
fig_S8b <- build_grid_figure(metrics, 'delta_connectance', 'antagonistic')
fig_S8c <- build_grid_figure(metrics, 'delta_robustness', 'mutualistic')
fig_S8d <- build_grid_figure(metrics, 'delta_connectance', 'mutualistic')


fig_S8_left <- (fig_S8a/fig_S8c) + plot_annotation("relative change in robustness") & theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 12))
fig_S8_right <- (fig_S8a/fig_S8c) + plot_annotation("relative change in connectance") & theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 12))

wrap_elements(fig_S8_left) + wrap_elements(fig_S8_right)

# store figure
ggsave(filename = here('figures', 'figure_S8.pdf'),
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)


###################


###################
## FIGURE S9 ####

# generate figure
fig_S9 <- build_difference_correlation_figure_by_components(metrics, 'connectance', 'robustness')
fig_S9 + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  plot_annotation("relative change in number of components") & theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# store figure
ggsave(filename = here('figures', 'figure_S9.pdf'),
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)

###################


###################
## FIGURE S10 #####


# produce figure 
fig_s10a <- build_difference_correlation_figure_by_num_species(metrics, 'connectance', 'robustness')
fig_s10b <- build_correlation_figure_by_num_species(metrics, 'connectance', 'robustness')
fig_s10a <- fig_s10a + theme(legend.title = element_blank())

fig_s10a / fig_s10b +  
  plot_layout(guides = "collect") + plot_annotation("number of species in community") & theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 12), legend.position = 'top')

# store figure
ggsave(filename = here('figures', 'figure_S10.pdf'),
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)


###################
## FIGURE S11 ####

# produce figure
data_S11 <- extract_raw_correlation_data(metrics, 'connectance', 'robustness')
data_S11 <- data_S11 %>%
  mutate(network_name_arranged = factor(network_name, levels=c("network_1","network_2","network_3","network_4", "network_5",
                                                               "network_6", "network_7", "network_8", "network_9", "network_10",
                                                               "network_11", "network_12", "network_13", "network_14", "network_15",
                                                               "network_16", "network_17", "network_18", "network_19", "network_20",
                                                               "network_21", "network_22", "network_23","network_24", "network_25",
                                                               "network_26", "network_27", "network_28", "network_29", "network_30"))) %>%
  filter(distribution != 'narrow niche')

fig_S11 <- ggplot(data = data_S11, aes(x = x, y = y, col = ratio_consumer_to_resource)) + geom_point(size = 0.6, alpha = 0.5) +
  theme_minimal() + xlab('connectance') + facet_wrap(~size) +
  ylab('robustness') +  labs(color='consumer to resource ratio')  +
  theme(legend.position = 'bottom', aspect.ratio = 1,
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10)) +
  scale_colour_gradientn(colors=paletteer_d("rcartocolor::Sunset"))

# store figure
ggsave(filename = here('figures', 'figure_S11.pdf'),
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)
###################


###################
## FIGURE S12 ####

# produce figure
data_S12 <- extract_difference_correlation_data(metrics, 'connectance', 'robustness')

delta_connectance_by_components <- ggplot(data = data_S12, aes(x = delta_z, y = delta_y, col = type_evo)) + geom_point(alpha = 0.7, size = 0.8) +
  theme_minimal() + xlab('relative change in number of components') +
  ylab('relative change in robustness')    +
  facet_wrap(~distribution, ncol = 2) +
  guides(fill=guide_legend(title="p"))  +
  theme(legend.position = 'bottom', aspect.ratio = 1,
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10))  + scale_color_manual(values=paletteer_d("lisa::MarcelDuchamp")[c(1,4)])  +
  geom_abline(slope = 0, intercept = 0) + geom_vline(xintercept = 0)

delta_connectance_by_components

# store figure
ggsave(filename = here('figures', 'figure_S12.pdf'),
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)


#####################
## FIGURE S2 ####

# generate figure
load(here('results', 'empirical_metrics.Rda'))
pd <- position_dodge(width = 0.4)

grid_is_predicted <- metrics  %>%
  mutate(delta_connectance = (connectance - observed_connectance)) %>%
  dplyr::group_by(type_evo, distribution, m, alpha, network_name) %>%
  dplyr::summarise(delta_connectance = mean(delta_connectance)) %>%
  dplyr::group_by(type_evo, distribution, m, alpha) %>%
  dplyr::summarise(delta_connectance = median(delta_connectance))

was_predicted_summary <- metrics  %>%
  mutate(delta_connectance = (connectance - observed_connectance)) %>%
  dplyr::group_by(type_evo, distribution, network_name) %>%
  dplyr::summarise(delta_connectance = mean(delta_connectance))


s2a <- ggplot(data = was_predicted_summary %>%
                mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche'),
                       type_evo = if_else(type_evo == 'antagonistic', 'antagonistic\ncoevolution', 'mutualistic\ncoevolution')),
              aes(x = type_evo, y = delta_connectance, col = distribution))  +
  geom_boxplot()  +
  xlab('') + ylab('') + theme_minimal() +
  scale_color_manual(values=c(c("#E1BE6A", "#40B0A6"))) +
  theme(aspect.ratio = 1, legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10))


s2b <- ggplot(data = grid_is_predicted %>%
                mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')),
              aes(x = as.factor(m), y = as.factor(alpha), fill = delta_connectance)) +
  geom_tile(alpha = 0.9) +
  facet_grid(distribution~type_evo) +
  scale_fill_gradientn(colours = paletteer_d("rcartocolor::OrYel")) + theme_minimal() +
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) +
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution') + ylab('strength of functional mechanism')   +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10))

s2a + s2b +
  plot_annotation("difference in connectance\n(predicted - observed)") & theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 10))

# store figure
ggsave(filename = here('figures', 'figure_S2.pdf'),
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)

###################
