# figures 

# clean up
rm(list=ls())

# requires
require(dplyr)
require(ggplot2)
require(wesanderson)
require(paletteer)


pal <- wes_palette("Zissou1", 400, type = "continuous")
pal_two <-  paletteer_d("rcartocolor::OrYel")
pal_correlation <- paletteer_d("rcartocolor::Sunset")
pal_type_evo_full <- paletteer_d("rtist::picasso")
pal_type_evo <- c("#FAC484FF", "#5C53A5FF")
pal_new <-  c("#E1BE6A", "#40B0A6")
pal_type_int <- paletteer_d("lisa::MarcelDuchamp")[c(1,4)]


# load data
load("/home/fernando/network_rewiring/results/distributions/metrics.Rda")
load("/home/fernando/network_rewiring/results/metrics_no_coevolution.Rda")

# join data
metrics <- metrics %>% dplyr::left_join(metrics_no_coevolution)

# standardise connectance
metrics <- metrics %>% 
  dplyr::mutate(
    minimum_connectance = if_else(num_resources >= num_consumers, num_resources/(num_resources*num_consumers), num_consumers/(num_resources*num_consumers)),
    connectance_standard = (connectance - minimum_connectance)/(1 - minimum_connectance),
    null_connectance_standard = (null_connectance - minimum_connectance)/(1 - minimum_connectance)
  )


# figure 1 raw

metrics_null_robustness <- metrics %>%
  dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
  dplyr::summarise(null_robustness_lower = mean(null_robustness_lower),
                   size = mean(size)) %>%
  select(null_robustness_lower, size) %>% 
  gather(type_evo, robustness_lower, null_robustness_lower) %>%
  mutate(type_evo = 'no\ncoevolution') 

metrics_robustness <- metrics %>% 
  dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
  dplyr::summarise(robustness_lower = mean(robustness_lower),
                   size = mean(size)) %>%
  select(type_evo, robustness_lower, distribution, size) %>%
  mutate(type_evo = if_else(type_evo == 'antagonistic', 'antagonistic\ncoevolution', 'mutualistic\ncoevolution')) 

metrics_null_robustness$distribution <- metrics_robustness$distribution
metrics_robustness <- rbind(metrics_robustness, metrics_null_robustness)


robustness_comparison <- ggplot(data = metrics_robustness %>%
                                  mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                aes(
                                  x = fct_relevel(type_evo,"antagonistic\ncoevolution", "no\ncoevolution", "mutualistic\ncoevolution"), 
                                  y = robustness_lower, fill = distribution))  + 
  geom_boxplot(alpha = 0.7) + xlab('') + ggtitle(label = 'robustness\n') + theme_minimal() + ylab('') + 
  theme(aspect.ratio = 1, legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        strip.text.x = element_text(size = 10, face = 'bold.italic'),
        strip.text.y = element_text(size = 10, face = 'bold.italic'),
        plot.title = element_text(size = 10, face = 'bold', hjust = 0.5),
        axis.text = element_text(size = 10)) + coord_fixed() + scale_fill_manual(values=c(pal_new)) 

metrics_null_connectance <- metrics %>%
  dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
  dplyr::summarise(null_connectance = mean(null_connectance)) %>%
  select(null_connectance) %>% 
  gather(type_evo, connectance, null_connectance) %>%
  mutate(type_evo = 'no\ncoevolution')

metrics_connectance <- metrics %>% 
  dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
  dplyr::summarise(connectance = mean(connectance)) %>% 
  select(type_evo, connectance, distribution)  %>%
  mutate(type_evo = if_else(type_evo == 'antagonistic', 'antagonistic\ncoevolution', 'mutualistic\ncoevolution'))

metrics_null_connectance$distribution <- metrics_connectance$distribution
metrics_connectance <- rbind(metrics_connectance, metrics_null_connectance)


connectance_comparison <- ggplot(data = metrics_connectance %>% 
                                   mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                 aes(x = fct_relevel(type_evo,"antagonistic\ncoevolution", "no\ncoevolution", "mutualistic\ncoevolution"), y = connectance, fill = distribution)) + 
  geom_boxplot(alpha = 0.7) + xlab('') + ggtitle(label = 'connectance\n') + theme_minimal() + ylab('') + 
  theme(aspect.ratio = 1, legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        strip.text.x = element_text(size = 10, face = 'bold.italic'),
        strip.text.y = element_text(size = 10, face = 'bold.italic'),
        plot.title = element_text(size = 10, face = 'bold', hjust = 0.5),
        axis.text = element_text(size = 10)) + coord_fixed() + scale_fill_manual(values=c(pal_new)) 

correlation_raw <- metrics %>%
  dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
  dplyr::summarise(
    size_net = mean(num_resources + num_consumers),
    connectance = mean(connectance),
    null_connectance = mean(null_connectance),
    num_components = mean(num_components),
    null_components = mean(null_components),
    robustness_lower = mean(robustness_lower),
    null_robustness_lower  = mean(null_robustness_lower),
  ) %>%
  dplyr::mutate(
    distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche'),
    delta_connectance = (connectance - null_connectance)/null_connectance,
    delta_robustness = (robustness_lower - null_robustness_lower)/null_robustness_lower,
    delta_components = (num_components - null_components)/null_components
  ) %>% 
  ggplot(data = ., aes(x = connectance, y = robustness_lower, col = type_evo)) + geom_point(size = 1, alpha = 0.5) +
  theme_minimal() + xlab('connectance') + 
  ylab('robustness')    + 
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


metrics_components <- metrics %>%
  dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
  dplyr::summarise(
    num_components = mean(num_components),
    num_components_null = mean(null_components))

metrics_null_components <- metrics_components %>%
  select(num_components_null) %>% 
  gather(type_evo, num_components, num_components_null) %>%
  mutate(type_evo = 'no\ncoevolution')

metrics_null_components$distribution <- metrics_components$distribution

metrics_components <- metrics_components %>% select(type_evo, num_components, distribution)  %>%
  mutate(type_evo = if_else(type_evo == 'antagonistic', 'antagonistic\ncoevolution', 'mutualistic\ncoevolution')) %>%
  rbind(metrics_null_components)

num_components_comparison <- ggplot(data = metrics_components %>% 
                                      mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                    aes(x = fct_relevel(type_evo,"antagonistic\ncoevolution", "no\ncoevolution", "mutualistic\ncoevolution"), 
                                        y = num_components, fill = distribution)) + 
  geom_boxplot(alpha = 0.7) + xlab('') + ggtitle(label = 'number of components\n') + theme_minimal() + ylab('') + 
  theme(aspect.ratio = 1, legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        strip.text.x = element_text(size = 10, face = 'bold.italic'),
        strip.text.y = element_text(size = 10, face = 'bold.italic'),
        plot.title = element_text(size = 10, face = 'bold', hjust = 0.5),
        axis.text = element_text(size = 10)) + coord_fixed() + scale_fill_manual(values=c(pal_new)) 


a <- (robustness_comparison + connectance_comparison) +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = 'bottom')
a / correlation_raw 
ggsave("figure_S3.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/",
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)


# figure 1 differences

robustness_transition <- metrics %>%
  dplyr::group_by(network_name, distribution, type_evo, m, alpha) %>%
  dplyr::summarise(
    robustness = mean(robustness_lower),
    null_robustness = mean(null_robustness_lower),
    size = mean(size)
  ) %>%
  mutate(
    delta_robustness = (robustness - null_robustness) / null_robustness
  )


connectance_transition <- metrics %>%
  dplyr::group_by(network_name, distribution, type_evo, m, alpha) %>%
  dplyr::summarise(
    connectance = mean(connectance),
    null_connectance = mean(null_connectance),
    size = mean(size)
  ) %>%
  mutate(
    delta_connectance = (connectance - null_connectance) / null_connectance
  )


component_transition <- metrics %>% 
  dplyr::group_by(network_name, distribution, type_evo, m, alpha) %>%
  dplyr::summarise(
    num_components = mean(num_components),
    null_components = mean(null_components),
    size = mean(size)
  ) %>%
  mutate(
    delta_components = (num_components - null_components) / null_components
  )


robustness_difference <- ggplot(data = robustness_transition %>%
                                  mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche'),
                                         type_evo = if_else(type_evo == 'antagonistic', 'antagonistic\ncoevolution','mutualistic\ncoevolution')),
                                aes(x = type_evo, y = delta_robustness, fill = distribution))   +
  geom_boxplot(alpha = 0.7) + xlab('') + ggtitle(label = 'relative change in robustness\n') + theme_minimal() + ylab('') + 
  theme(aspect.ratio = 1, legend.title = element_blank(), legend.position = 'none',
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        strip.text.x = element_text(size = 10, face = 'bold.italic'),
        strip.text.y = element_text(size = 10, face = 'bold.italic'),
        plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
        axis.text = element_text(size = 10)) + coord_fixed() + scale_fill_manual(values=c(pal_new)) + 
  geom_abline(slope = 0, intercept = 0)

connectance_difference <- ggplot(data = connectance_transition %>%
                                   mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche'),
                                          type_evo = if_else(type_evo == 'antagonistic', 'antagonistic\ncoevolution','mutualistic\ncoevolution')),
                                 aes(x = type_evo, y = delta_connectance, fill = distribution)) +
  geom_boxplot(alpha = 0.7) + xlab('') + ggtitle(label = 'relative change in connectance\n') + theme_minimal() + ylab('') + 
  theme(aspect.ratio = 1, legend.title = element_blank(), legend.position = 'none',
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        strip.text.x = element_text(size = 10, face = 'bold.italic'),
        strip.text.y = element_text(size = 10, face = 'bold.italic'),
        plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
        axis.text = element_text(size = 10)) + coord_fixed() + scale_fill_manual(values=c(pal_new)) + 
  geom_abline(slope = 0, intercept = 0)

component_difference <- ggplot(data = component_transition  %>%
                                 mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche'),
                                        type_evo = if_else(type_evo == 'antagonistic', 'antagonistic\ncoevolution','mutualistic\ncoevolution')), 
                               aes(x = type_evo, y = delta_components, fill = distribution)) +
  geom_boxplot(alpha = 0.7) + xlab('') + ggtitle(label = 'relative change in number of components\n') + theme_minimal() + ylab('') + 
  theme(aspect.ratio = 1, legend.title = element_blank(), legend.position = 'none',
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        strip.text.x = element_text(size = 10, face = 'bold.italic'),
        strip.text.y = element_text(size = 10, face = 'bold.italic'),
        plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
        axis.text = element_text(size = 10)) + coord_fixed() + scale_fill_manual(values=c(pal_new)) + 
  geom_abline(slope = 0, intercept = 0) 


correlation_diff <- metrics %>%
  mutate(
    size_net = num_resources + num_consumers
  ) %>%
  dplyr::group_by(type_evo, m, alpha, distribution, size_net) %>%
  dplyr::summarise(
    connectance = mean(connectance),
    null_connectance = mean(null_connectance),
    num_components = mean(num_components),
    null_components = mean(null_components),
    robustness_lower = mean(robustness_lower),
    null_robustness_lower  = mean(null_robustness_lower),
  ) %>%
  dplyr::mutate(
    distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche'),
    delta_connectance = (connectance - null_connectance)/null_connectance,
    delta_robustness = (robustness_lower - null_robustness_lower)/null_robustness_lower,
    delta_components = (num_components - null_components)/null_components
  )  %>%
  ggplot(data = ., aes(x = delta_connectance, y = delta_robustness, col = size_net)) + geom_point(size = 0.8, alpha = 0.8) +
  theme_minimal() + xlab('relative change in connectance') + 
  ylab('relative change in robustness')    + labs(color='number of species') +
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


b1 <- robustness_difference + connectance_difference  + 
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = 'bottom')

b1 / correlation_diff 

ggsave("figure_S7.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/",
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)

robustness_difference <- ggplot(data = robustness_transition  %>% 
                                  filter(distribution != 'truncated') %>%
                                  mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche'),
                                         type_evo = if_else(type_evo == 'antagonistic', 'antagonistic\ncoevolution','mutualistic\ncoevolution')),
                                aes(x = type_evo, y = delta_robustness, fill = type_evo))  +
  geom_boxplot(alpha = 0.7) + xlab('') + ggtitle(label = 'relative change in robustness\n') + theme_minimal() + ylab('') + 
  theme(aspect.ratio = 1, legend.title = element_blank(), legend.position = 'none',
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        strip.text.x = element_text(size = 10, face = 'bold.italic'),
        strip.text.y = element_text(size = 10, face = 'bold.italic'),
        plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
        axis.text = element_text(size = 10)) + coord_fixed() + scale_fill_manual(values=c(pal_type_int)) + 
  geom_abline(slope = 0, intercept = 0)

connectance_difference <- ggplot(data = connectance_transition %>% 
                                   filter(distribution != 'truncated') %>%
                                   mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche'),
                                          type_evo = if_else(type_evo == 'antagonistic', 'antagonistic\ncoevolution','mutualistic\ncoevolution')),
                                 aes(x = type_evo, y = delta_connectance, fill = type_evo)) +
  geom_boxplot(alpha = 0.7) + xlab('') + ggtitle(label = 'relative change in connectance\n') + theme_minimal() + ylab('') + 
  theme(aspect.ratio = 1, legend.title = element_blank(), legend.position = 'none',
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        strip.text.x = element_text(size = 10, face = 'bold.italic'),
        strip.text.y = element_text(size = 10, face = 'bold.italic'),
        plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
        axis.text = element_text(size = 10)) + coord_fixed() + scale_fill_manual(values=c(pal_type_int)) + 
  geom_abline(slope = 0, intercept = 0)

component_difference <- ggplot(data = component_transition %>% 
                                 filter(distribution != 'truncated') %>%
                                 mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche'),
                                        type_evo = if_else(type_evo == 'antagonistic', 'antagonistic\ncoevolution','mutualistic\ncoevolution')), 
                               aes(x = type_evo, y = delta_components, fill = type_evo)) + 
  geom_boxplot(alpha = 0.7) + xlab('') + ggtitle(label = 'relative change in number of components\n') + theme_minimal() + ylab('') + 
  theme(aspect.ratio = 1, legend.title = element_blank(), legend.position = 'none',
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        strip.text.x = element_text(size = 10, face = 'bold.italic'),
        strip.text.y = element_text(size = 10, face = 'bold.italic'),
        plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
        axis.text = element_text(size = 10)) + coord_fixed() + scale_fill_manual(values=c(pal_type_int)) + 
  geom_abline(slope = 0, intercept = 0) 

correlation_diff <- metrics %>%
  mutate(
    size_net = num_resources + num_consumers
  ) %>%
  dplyr::group_by(type_evo, m, alpha, distribution, size_net) %>%
  dplyr::summarise(
    connectance = mean(connectance),
    null_connectance = mean(null_connectance),
    num_components = mean(num_components),
    null_components = mean(null_components),
    robustness_lower = mean(robustness_lower),
    null_robustness_lower  = mean(null_robustness_lower),
  ) %>%
  dplyr::mutate(
    distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche'),
    delta_connectance = (connectance - null_connectance)/null_connectance,
    delta_robustness = (robustness_lower - null_robustness_lower)/null_robustness_lower,
    delta_components = (num_components - null_components)/null_components
  ) %>% 
  filter(distribution != 'truncated') %>%
  ggplot(data = ., aes(x = delta_connectance, y = delta_robustness, col = size_net)) + geom_point(size = 0.8, alpha = 0.8) +
  theme_minimal() + xlab('relative change in connectance') + 
  ylab('relative change in robustness')    + labs(color='number of species') +
  facet_wrap(~type_evo, scales="free") +
  theme(legend.position = 'bottom', aspect.ratio = 1,
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10)) + scale_colour_gradientn(colors=c(pal_correlation)) + geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)


#b <- robustness_difference + connectance_difference  + 
 # plot_layout(guides = "collect", ncol = 2) & theme(legend.position = 'none')
b <- robustness_difference
#b / correlation_diff 
b
ggsave("figure_2.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/",
       width=180, height=140, units="mm", dpi=600, device = cairo_pdf)

# figure 2 raw

metrics_grid <- metrics  %>% 
  mutate(has_one_component = if_else(num_components == 1, TRUE, FALSE),
         null_has_one_component = if_else(null_components == 1, TRUE, FALSE)) %>%
  dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
  dplyr::summarise(fraction_species_largest_component = mean(fraction_species_largest_component),
                   connectance = mean(connectance),
                   robustness_lower = mean(robustness_lower),
                   null_fraction_species_largest_component = mean(null_fraction_species_largest_component),
                   null_connectance = mean(null_connectance),
                   null_robustness_lower = mean(null_robustness_lower),
                   p = sum(has_one_component)/length(has_one_component),
                   p_null = sum(null_has_one_component)/length(null_has_one_component),
                   num_components = mean(num_components),
                   null_components = mean(null_components)) %>%
  dplyr::group_by(distribution, type_evo, m, alpha) %>%
  dplyr::summarise(
    median_fraction_species_largest_component = median(fraction_species_largest_component),
    median_connectance = median(connectance),
    median_robustness = median(robustness_lower),
    null_median_fraction_species_largest_component =  median(null_fraction_species_largest_component),
    null_median_connectance = median(null_connectance),
    null_median_robustness = median(null_robustness_lower),
    p = median(p),
    p_null = median(p_null),
    num_components = median(num_components),
    null_components = median(null_components)) %>% 
  mutate(
    delta_fraction_species_largest_component = (median_fraction_species_largest_component - null_median_fraction_species_largest_component)/null_median_fraction_species_largest_component,
    delta_connectance = (median_connectance - null_median_connectance)/null_median_connectance,
    delta_robustness = (median_robustness - null_median_robustness)/null_median_robustness,
    delta_p = (p - p_null)/p_null,
    delta_components = (num_components - null_components)/null_components
  )


robustness_grid_mut <- ggplot(data = metrics_grid %>% 
                                filter(type_evo == 'mutualistic') %>%
                                mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                              aes(x = as.factor(m), y = as.factor(alpha), fill = median_robustness)) +
  geom_tile(alpha = 0.9) +
  facet_grid(distribution~type_evo)  + 
  scale_fill_gradientn(colours = pal_two) + theme_minimal() +
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('strength of functional mechanism (\u03b1)')  +
  guides(fill=guide_legend(title="robustness", reverse = TRUE)) + labs(subtitle = "") +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10))  

robustness_grid_anta <- ggplot(data = metrics_grid %>% 
                                 filter(type_evo == 'antagonistic') %>%
                                 mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                               aes(x = as.factor(m), y = as.factor(alpha), fill = median_robustness)) +
  geom_tile(alpha = 0.9) +
  facet_grid(distribution~type_evo)  + 
  scale_fill_gradientn(colours = pal_two) + theme_minimal()+
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('strength of functional mechanism (\u03b1)')  +
  guides(fill=guide_legend(title="robustness", reverse = TRUE)) + labs(subtitle = "robustness") +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10))  

connectance_grid_mut <-  ggplot(data = metrics_grid %>% 
                                  filter(type_evo == 'mutualistic') %>%
                                  mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                aes(x = as.factor(m), y = as.factor(alpha), fill = median_connectance)) +
  geom_tile(alpha = 0.9) +
  facet_grid(distribution~type_evo) + theme_minimal() +
  scale_fill_gradientn(colours = pal_two) +
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('')  +
  guides(fill=guide_legend(title="connectance", reverse = TRUE)) + labs(subtitle = "") +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10)) 

connectance_grid_anta <-  ggplot(data = metrics_grid %>% 
                                   filter(type_evo == 'antagonistic') %>%
                                   mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                 aes(x = as.factor(m), y = as.factor(alpha), fill = median_connectance)) +
  geom_tile(alpha = 0.9) +
  facet_grid(distribution~type_evo) + theme_minimal() +
  scale_fill_gradientn(colours = pal_two) +
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('')  +
  guides(fill=guide_legend(title="connectance", reverse = TRUE)) + labs(subtitle = "connectance") +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10)) 



(robustness_grid_anta + connectance_grid_anta)  /  (robustness_grid_mut + connectance_grid_mut) 
ggsave("figure_S4.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/", width=180, height=240, units="mm", dpi=600, device = cairo_pdf)


robustness_grid_mut_diff <- ggplot(data = metrics_grid %>% 
                                     filter(type_evo == 'mutualistic',
                                            distribution != 'truncated') %>%
                                     mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                   aes(x = as.factor(m), y = as.factor(alpha), fill = delta_robustness)) +
  geom_tile(alpha = 0.9) +
  facet_grid(~type_evo) +
  scale_fill_gradientn(colours = pal_two, labels=function(x) paste0(format(round(x, 2), nsmall = 2))) + theme_minimal()+
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('strength of\nfunctional mechanism (\u03b1)')  +
  guides(fill=guide_legend(title="relative change in robustness", reverse = TRUE))  +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        strip.text.x = element_text(size = 9, face = 'italic'),
        strip.text.y = element_text(size = 9, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 9)) 

robustness_grid_anta_diff <- ggplot(data = metrics_grid %>% 
                                      filter(type_evo == 'antagonistic',
                                             distribution != 'truncated') %>%
                                      mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                    aes(x = as.factor(m), y = as.factor(alpha), fill = delta_robustness)) +
  geom_tile(alpha = 0.9) +
  facet_grid(~type_evo) +
  scale_fill_gradientn(colours = pal_two, labels=function(x) paste0(format(round(x, 2), nsmall = 2))) + theme_minimal()+
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('strength of\nfunctional mechanism (\u03b1)')  +
  guides(fill=guide_legend(title="relative change in robustness", reverse = TRUE))  +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        strip.text.x = element_text(size = 9, face = 'italic'),
        strip.text.y = element_text(size = 9, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 9)) 


connectance_grid_mut_diff <-  ggplot(data = metrics_grid %>% 
                                       filter(type_evo == 'mutualistic',
                                              distribution != 'truncated') %>%
                                       mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                     aes(x = as.factor(m), y = as.factor(alpha), fill = delta_connectance)) +
  geom_tile(alpha = 0.9) +
  facet_grid(~type_evo) + theme_minimal() +
  scale_fill_gradientn(colours = pal_two, labels=function(x) paste0(format(round(x, 2), nsmall = 2))) +
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('strength of\nfunctional mechanism (\u03b1)') + 
  guides(fill=guide_legend(title="relative change in connectance", reverse = TRUE))  +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        strip.text.x = element_text(size = 9, face = 'italic'),
        strip.text.y = element_text(size = 9, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 9)) 

connectance_grid_anta_diff <-  ggplot(data = metrics_grid %>% 
                                        filter(type_evo == 'antagonistic',
                                               distribution != 'truncated') %>%
                                        mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                      aes(x = as.factor(m), y = as.factor(alpha), fill = delta_connectance)) +
  geom_tile(alpha = 0.9) +
  facet_grid(~type_evo) + theme_minimal() +
  scale_fill_gradientn(colours = pal_two, labels=function(x) paste0(format(round(x, 2), nsmall = 2))) +
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('strength of\nfunctional mechanism (\u03b1)') +
  guides(fill=guide_legend(title="relative change in connectance", reverse = TRUE))  +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        strip.text.x = element_text(size = 9, face = 'italic'),
        strip.text.y = element_text(size = 9, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 9)) 

components_grid_mut_diff <- ggplot(data = metrics_grid %>% 
                                     filter(type_evo == 'mutualistic',
                                            distribution != 'truncated') %>%
                                     mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                   aes(x = as.factor(m), y = as.factor(alpha), fill = delta_components)) +
  geom_tile(alpha = 0.9) +
  facet_grid(~type_evo) + theme_minimal() +
  scale_fill_gradientn(colours = pal_two, labels=function(x) paste0(format(round(x, 2), nsmall = 2))) + 
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('strength of\nfunctional mechanism (\u03b1)') +
  guides(fill=guide_legend(title="relative change in number of components", reverse = TRUE))  +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        strip.text.x = element_text(size = 9, face = 'italic'),
        strip.text.y = element_text(size = 9, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 9)) 

components_grid_anta_diff <- ggplot(data = metrics_grid %>% 
                                      filter(type_evo == 'antagonistic',
                                             distribution != 'truncated') %>%
                                      mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                    aes(x = as.factor(m), y = as.factor(alpha), fill = delta_components)) +
  geom_tile(alpha = 0.9) +
  facet_grid(~type_evo) + theme_minimal() +
  scale_fill_gradientn(colours = pal_two, labels=function(x) paste0(format(round(x, 2), nsmall = 2))) +
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('strength of\nfunctional mechanism (\u03b1)')   + 
  guides(fill=guide_legend(title="relative change in number of components", reverse = TRUE))  +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        strip.text.x = element_text(size = 9, face = 'italic'),
        strip.text.y = element_text(size = 9, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 9)) 


top <- robustness_grid_anta_diff + robustness_grid_mut_diff +   plot_annotation("relative change in robustness") & theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 9))
mid <-  connectance_grid_anta_diff + connectance_grid_mut_diff + plot_annotation("relative change in connectance") & theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 9))
bottom <- components_grid_anta_diff + components_grid_mut_diff +   plot_annotation("relative change in number of components") & theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 9))

wrap_elements(top) / wrap_elements(mid) / wrap_elements(bottom)  + plot_layout(heights = c(1,1,1))
ggsave("figure_5.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/", width=180, height=220, units="mm", dpi=600, device = cairo_pdf)

# new figure 3 raw

components_grid_mut_raw <- ggplot(data = metrics_grid %>% 
                                    filter(type_evo == 'mutualistic') %>%
                                    mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                  aes(x = as.factor(m), y = as.factor(alpha), fill = num_components)) +
  geom_tile(alpha = 0.9) +
  facet_grid(distribution~type_evo) + theme_minimal() +
  scale_fill_gradientn(colours = pal_two) +
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('') +
  guides(fill=guide_legend(title="number of components", reverse = TRUE))  +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10)) 

components_grid_anta_raw <- ggplot(data = metrics_grid %>% 
                                     filter(type_evo == 'antagonistic') %>%
                                     mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                   aes(x = as.factor(m), y = as.factor(alpha), fill = num_components)) +
  geom_tile(alpha = 0.9) +
  facet_grid(distribution~type_evo) + theme_minimal() +
  scale_fill_gradientn(colours = pal_two) +
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('strength of functional mechanism (\u03b1)')   +
  guides(fill=guide_legend(title="number of components", reverse = TRUE))  +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10)) 


correlation_components_raw <- metrics %>%
  dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
  dplyr::summarise(
    size_net = mean(num_resources + num_consumers),
    connectance = mean(connectance),
    null_connectance = mean(null_connectance),
    num_components = mean(num_components),
    null_components = mean(null_components),
    robustness_lower = mean(robustness_lower),
    null_robustness_lower  = mean(null_robustness_lower),
  ) %>%
  dplyr::mutate(
    distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche'),
    delta_connectance = (connectance - null_connectance)/null_connectance,
    delta_robustness = (robustness_lower - null_robustness_lower)/null_robustness_lower,
    delta_components = (num_components - null_components)/null_components
  ) %>% 
  ggplot(data = ., aes(x = connectance, y = robustness_lower, col = num_components)) + geom_point(size = 1, alpha = 0.5) +
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

(components_grid_anta_raw + components_grid_mut_raw) / correlation_components_raw  +  plot_annotation("number of components") & theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
ggsave("figure_S5.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/",
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)


# new figure 3 diff
correlation_components_diff <- metrics %>%
  filter(distribution != 'truncated') %>% 
  dplyr::group_by(type_evo, size, m, alpha, distribution) %>%
  dplyr::summarise(
    size_net = mean(num_resources + num_consumers),
    connectance = mean(connectance),
    null_connectance = mean(null_connectance),
    num_components = mean(num_components),
    null_components = mean(null_components),
    robustness_lower = mean(robustness_lower),
    null_robustness_lower  = mean(null_robustness_lower),
  ) %>%
  dplyr::mutate(
    distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche'),
    delta_connectance = (connectance - null_connectance)/null_connectance,
    delta_robustness = (robustness_lower - null_robustness_lower)/null_robustness_lower,
    delta_components = (num_components - null_components)/null_components
  ) %>% 
  ggplot(data = ., aes(x = delta_connectance, y = delta_robustness, col = delta_components)) + geom_point(size = 1, alpha = 0.5) +
  theme_minimal() + xlab('relative change in connectance') + 
  ylab('relative change in robustness')    + 
  facet_wrap(~type_evo, scales = "free") + 
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
  scale_colour_gradientn(colors=c(pal_correlation)) + geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)


correlation_components_diff  +  plot_annotation("relative change in number of components") & theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

ggsave("figure_S6.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/",
       width=180, height=100, units="mm", dpi=600, device = cairo_pdf)



# supplementary figures 

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
ggsave("figure_S1.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/",
       width=180, height=100, units="mm", dpi=600, device = cairo_pdf)


s3a <- metrics %>%
  dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
  dplyr::summarise(
    size_net = mean(num_resources + num_consumers),
    connectance = mean(connectance),
    null_connectance = mean(null_connectance),
    num_components = mean(num_components),
    null_components = mean(null_components),
    robustness_lower = mean(robustness_lower),
    null_robustness_lower  = mean(null_robustness_lower),
  ) %>%
  dplyr::mutate(
    delta_connectance = (connectance - null_connectance)/null_connectance,
    delta_robustness = (robustness_lower - null_robustness_lower)/null_robustness_lower,
    delta_components = (num_components - null_components)/null_components,
    distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')
  ) %>% 
  ggplot(data = ., aes(x = delta_connectance, y = delta_robustness, col = size_net)) + geom_point(size = 1, alpha = 0.5) +
  theme_minimal() + xlab('relative change in connectance') + 
  ylab('relative change in robustness')    + 
  facet_grid(~distribution) + 
  guides(fill=guide_legend(title="p"))  +
  theme(legend.position = 'top', aspect.ratio = 1,
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10)) + 
  scale_colour_gradientn(colors=c(pal_correlation))  + labs(subtitle = "number of species in community") 


s3b <- metrics %>%
  dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
  dplyr::summarise(
    size_net = mean(num_resources + num_consumers),
    connectance = mean(connectance),
    null_connectance = mean(null_connectance),
    num_components = mean(num_components),
    null_components = mean(null_components),
    robustness_lower = mean(robustness_lower),
    null_robustness_lower  = mean(null_robustness_lower),
  ) %>%
  dplyr::mutate(
    delta_connectance = (connectance - null_connectance)/null_connectance,
    delta_robustness = (robustness_lower - null_robustness_lower)/null_robustness_lower,
    delta_components = (num_components - null_components)/null_components,
    distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')
  ) %>% 
  ggplot(data = ., aes(x = connectance, y = robustness_lower, col = size_net)) + geom_point(size = 1, alpha = 0.5) +
  theme_minimal() + xlab('connectance') + 
  ylab('robustness')    + 
  facet_grid(~distribution) + 
  guides(fill=guide_legend(title="p"))  +
  theme(legend.position = 'none', aspect.ratio = 1,
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10)) + 
  scale_colour_gradientn(colors=c(pal_correlation))

s3a / s3b
ggsave("figure_S10.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/",
       width=180, height=200, units="mm", dpi=600, device = cairo_pdf)

# relatative change in number of components vs relative change in robustness
delta_connectance_by_components <- metrics %>%
  dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
  dplyr::summarise(
    size_net = mean(num_resources + num_consumers),
    connectance = mean(connectance),
    null_connectance = mean(null_connectance),
    num_components = mean(num_components),
    null_components = mean(null_components),
    robustness_lower = mean(robustness_lower),
    null_robustness_lower  = mean(null_robustness_lower),
  ) %>%
  dplyr::mutate(
    distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche'),
    delta_connectance = (connectance - null_connectance)/null_connectance,
    delta_robustness = (robustness_lower - null_robustness_lower)/null_robustness_lower,
    delta_components = (num_components - null_components)/null_components
  ) %>% 
  ggplot(data = ., aes(x = delta_components, y = delta_robustness, col = type_evo)) + geom_point(alpha = 0.7, size = 0.8) +
  theme_minimal() + xlab('relative change in number of components') + 
  ylab('relative change in robustness')    + 
  facet_wrap(~distribution, ncol = 1) +
  guides(fill=guide_legend(title="p"))  +
  theme(legend.position = 'bottom', aspect.ratio = 1,
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10))  + scale_color_manual(values=c(pal_type_int))  + 
  geom_abline(slope = 0, intercept = 0) + geom_vline(xintercept = 0) 

delta_connectance_by_components

ggsave("figure_S12.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/",
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)


# new figure 2.5

figure_2.5_a <- metrics %>%
  filter(distribution != "truncated") %>% 
  dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
  dplyr::summarise(
    size_net = mean(num_resources + num_consumers),
    connectance = mean(connectance),
    null_connectance = mean(null_connectance),
    num_components = mean(num_components),
    null_components = mean(null_components),
    robustness_lower = mean(robustness_lower),
    null_robustness_lower  = mean(null_robustness_lower),
  ) %>%
  dplyr::mutate(
    delta_connectance = (connectance - null_connectance)/null_connectance,
    delta_robustness = (robustness_lower - null_robustness_lower)/null_robustness_lower,
    delta_components = (num_components - null_components)/null_components,
    distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')
  ) %>% 
  ggplot(data = ., aes(x = connectance, y = robustness_lower, col = type_evo)) + geom_point(size = 0.6, alpha = 0.7) +
  theme_minimal() + xlab('connectance') + 
  ylab('robustness') + 
  guides(fill=guide_legend(title="p"))  +
  theme(legend.position = 'bottom', aspect.ratio = 1,
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10)) + 
  scale_color_manual(values=c(pal_type_int)) +
  guides(color = guide_legend(override.aes = list(size = 2)))


figure_2.5_b <- metrics %>%
  filter(distribution != "truncated") %>% 
  dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
  dplyr::summarise(
    size_net = mean(num_resources + num_consumers),
    connectance = mean(connectance),
    null_connectance = mean(null_connectance),
    num_components = mean(num_components),
    null_components = mean(null_components),
    robustness_lower = mean(robustness_lower),
    null_robustness_lower  = mean(null_robustness_lower),
  ) %>%
  dplyr::mutate(
    delta_connectance = (connectance - null_connectance)/null_connectance,
    delta_robustness = (robustness_lower - null_robustness_lower)/null_robustness_lower,
    delta_components = (num_components - null_components)/null_components,
    distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')
  ) %>% 
  ggplot(data = ., aes(x = connectance, y = robustness_lower, col = size_net)) + geom_point(size = 0.6, alpha = 0.5) +
  theme_minimal() + xlab('connectance') + 
  ylab('robustness') +  labs(color='number of species')  +
  theme(legend.position = 'bottom', aspect.ratio = 1,
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10)) + 
  scale_colour_gradientn(colors=c(pal_correlation)) 

correlation_diff +  
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("figure_4.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/",
       width=180, height=140, units="mm", dpi=600, device = cairo_pdf)

figure_2.5_b + figure_2.5_a
ggsave("figure_3.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/",
       width=180, height=140, units="mm", dpi=600, device = cairo_pdf)

figure_2.5_a <- metrics %>%
  filter(distribution != "truncated") %>%
  dplyr::group_by(type_evo, size, m, alpha, distribution) %>%
  dplyr::summarise(
    size_net = mean(num_resources + num_consumers),
    connectance = mean(connectance),
    null_connectance = mean(null_connectance),
    num_components = mean(num_components),
    null_components = mean(null_components),
    robustness_lower = mean(robustness_lower),
    null_robustness_lower  = mean(null_robustness_lower),
  ) %>%
  dplyr::mutate(
    delta_connectance = (connectance - null_connectance)/null_connectance,
    delta_robustness = (robustness_lower - null_robustness_lower)/null_robustness_lower,
    delta_components = (num_components - null_components)/null_components,
    distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')
  ) %>% 
  ggplot(data = ., aes(x = connectance, y = robustness_lower, col = size_net)) + geom_point(size = 0.6, alpha = 0.5) +
  theme_minimal() + xlab('connectance') + 
  ylab('robustness') + 
  facet_grid(~type_evo) +
  guides(fill=guide_legend(title="p"))  +
  theme(legend.position = 'bottom', aspect.ratio = 1,
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10)) + 
    scale_colour_gradientn(colors=c(pal_correlation)) + labs(color='number of species')

figure_2.5_a / correlation_diff + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("new_figure_2.5_v2.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/",
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)


# new figure connectance vs robustness by ratio (network name)

metrics %>%
  filter(distribution != "truncated") %>% 
  dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
  dplyr::summarise(
    size_net = mean(num_resources + num_consumers),
    connectance = mean(connectance),
    null_connectance = mean(null_connectance),
    num_components = mean(num_components),
    null_components = mean(null_components),
    robustness_lower = mean(robustness_lower),
    null_robustness_lower  = mean(null_robustness_lower),
    ratio_consumer_to_resource = num_consumers/num_resources
  ) %>%
  dplyr::mutate(
    delta_connectance = (connectance - null_connectance)/null_connectance,
    delta_robustness = (robustness_lower - null_robustness_lower)/null_robustness_lower,
    delta_components = (num_components - null_components)/null_components,
    distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche'),
    network_name_arranged = factor(network_name, levels=c("network_1","network_2","network_3","network_4", "network_5",
                                                          "network_6", "network_7", "network_8", "network_9", "network_10",
                                                          "network_11", "network_12", "network_13", "network_14", "network_15",
                                                          "network_16", "network_17", "network_18", "network_19", "network_20",
                                                          "network_21", "network_22", "network_23","network_24", "network_25",
                                                          "network_26", "network_27", "network_28", "network_29", "network_30"))) %>%
  ggplot(data = ., aes(x = connectance, y = robustness_lower, col = ratio_consumer_to_resource)) + geom_point(size = 0.6, alpha = 0.5) +
  theme_minimal() + xlab('connectance') + facet_wrap(~size_net) + 
  ylab('robustness') +  labs(color='consumer to resource ratio')  +
  theme(legend.position = 'bottom', aspect.ratio = 1,
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10)) + 
  scale_colour_gradientn(colors=c(pal_correlation)) 
ggsave("figure_S11.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/",
       width=180, height=220, units="mm", dpi=600, device = cairo_pdf)

# component difference plot
components_grid_mut_diff <- ggplot(data = metrics_grid %>% 
                                     filter(type_evo == 'mutualistic') %>%
                                     mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                   aes(x = as.factor(m), y = as.factor(alpha), fill = delta_components)) +
  geom_tile(alpha = 0.9) +
  facet_grid(distribution~type_evo) + theme_minimal() +
  scale_fill_gradientn(colours = pal_two) +
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('') +
  guides(fill=guide_legend(title="relative change in number of components", reverse = TRUE))  +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10)) 

components_grid_anta_diff <- ggplot(data = metrics_grid %>% 
                                      filter(type_evo == 'antagonistic') %>%
                                      mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                    aes(x = as.factor(m), y = as.factor(alpha), fill = delta_components)) +
  geom_tile(alpha = 0.9) +
  facet_grid(distribution~type_evo) + theme_minimal() +
  scale_fill_gradientn(colours = pal_two) +
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('strength of functional mechanism (\u03b1)')   +
  guides(fill=guide_legend(title="relative change in number of components", reverse = TRUE))  +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10)) 


correlation_components_diff <- metrics %>%
  dplyr::group_by(type_evo, network_name, m, alpha, distribution) %>%
  dplyr::summarise(
    size_net = mean(num_resources + num_consumers),
    connectance = mean(connectance),
    null_connectance = mean(null_connectance),
    num_components = mean(num_components),
    null_components = mean(null_components),
    robustness_lower = mean(robustness_lower),
    null_robustness_lower  = mean(null_robustness_lower),
  ) %>%
  dplyr::mutate(
    distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche'),
    delta_connectance = (connectance - null_connectance)/null_connectance,
    delta_robustness = (robustness_lower - null_robustness_lower)/null_robustness_lower,
    delta_components = (num_components - null_components)/null_components
  ) %>% 
  ggplot(data = ., aes(x = delta_connectance, y = delta_robustness, col = delta_components)) + geom_point(size = 1, alpha = 0.5) +
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
  scale_colour_gradientn(colors=c(pal_correlation)) + geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

correlation_components_diff +  plot_annotation("relative change in number of components") & theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
ggsave("figure_S9.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/",
       width=180, height=140, units="mm", dpi=600, device = cairo_pdf)


robustness_grid_mut_diff <- ggplot(data = metrics_grid %>% 
                                     filter(type_evo == 'mutualistic') %>%
                                     mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                   aes(x = as.factor(m), y = as.factor(alpha), fill = delta_robustness)) +
  geom_tile(alpha = 0.9) +
  facet_grid(distribution~type_evo)  + 
  scale_fill_gradientn(colours = pal_two) + theme_minimal()+
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('strength of functional mechanism (\u03b1)')  +
  guides(fill=guide_legend(title="relative change in robustness", reverse = TRUE)) + labs(subtitle = "") +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10))  

robustness_grid_anta_diff <- ggplot(data = metrics_grid %>% 
                                      filter(type_evo == 'antagonistic') %>%
                                      mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                    aes(x = as.factor(m), y = as.factor(alpha), fill = delta_robustness)) +
  geom_tile(alpha = 0.9) +
  facet_grid(distribution~type_evo)  + 
  scale_fill_gradientn(colours = pal_two) + theme_minimal()+
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('strength of functional mechanism (\u03b1)')  +
  guides(fill=guide_legend(title="relative change in robustness", reverse = TRUE)) + labs(subtitle = "relative change in robustness") +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10))  

connectance_grid_mut_diff <-  ggplot(data = metrics_grid %>% 
                                       filter(type_evo == 'mutualistic') %>%
                                       mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                     aes(x = as.factor(m), y = as.factor(alpha), fill = delta_connectance)) +
  geom_tile(alpha = 0.9) +
  facet_grid(distribution~type_evo) + theme_minimal() +
  scale_fill_gradientn(colours = pal_two) +
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('')  +
  guides(fill=guide_legend(title="relative change in connectance", reverse = TRUE)) + labs(subtitle = "") +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10)) 

connectance_grid_anta_diff <-  ggplot(data = metrics_grid %>% 
                                        filter(type_evo == 'antagonistic') %>%
                                        mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche')), 
                                      aes(x = as.factor(m), y = as.factor(alpha), fill = delta_connectance)) +
  geom_tile(alpha = 0.9) +
  facet_grid(distribution~type_evo) + theme_minimal() +
  scale_fill_gradientn(colours = pal_two) +
  scale_x_discrete(breaks = scales::pretty_breaks(n = 4)) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 100)) +
  xlab('strength of coevolution (m)') + ylab('')  +
  guides(fill=guide_legend(title="relative change in connectance", reverse = TRUE)) + labs(subtitle = "relative change in connectance") +
  theme(aspect.ratio = 1, legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = 'italic'),
        strip.text.y = element_text(size = 10, face = 'italic'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 10)) 


(robustness_grid_anta_diff + connectance_grid_anta_diff)  /  (robustness_grid_mut_diff + connectance_grid_mut_diff)
ggsave("figure_S8.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/", width=180, height=240, 
       units="mm", dpi=600, device = cairo_pdf)


# remove all from before
rm(list=ls())

pal <- wes_palette("Zissou1", 400, type = "continuous")
pal_two <-  paletteer_d("rcartocolor::OrYel")
pal_correlation <- paletteer_d("rcartocolor::Sunset")
pal_type_evo_full <- paletteer_d("rtist::picasso")
pal_type_evo <- c("#FAC484FF", "#5C53A5FF")
pal_new <-  c("#E1BE6A", "#40B0A6")
pal_type_int <- paletteer_d("lisa::MarcelDuchamp")[c(1,4)]

# load results
load("/home/fernando/network_rewiring/results/empirical_size/metrics.Rda")

# determine whether observed connectance is predicted
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


# define position
pd = position_dodge(width = 0.4)

s2a <- ggplot(data = was_predicted_summary %>%
                mutate(distribution = if_else(distribution == 'truncated', 'narrow niche', 'broad niche'),
                       type_evo = if_else(type_evo == 'antagonistic', 'antagonistic\ncoevolution', 'mutualistic\ncoevolution')), 
              aes(x = type_evo, y = delta_connectance, col = distribution))  + 
  geom_boxplot()  + 
  xlab('') + ylab('') + theme_minimal() + 
  scale_color_manual(values=c(pal_new)) + 
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
  scale_fill_gradientn(colours = pal_two) + theme_minimal() + 
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
ggsave("figure_S2.pdf", path="~/network_rewiring_pitch_figures/coevolution_no_coevolution/",
       width=180, height=140, units="mm", dpi=600, device = cairo_pdf)

