rm(list = ls())
source(here::here("0-config.R"))

# Load mediator-outcome effect estimates ----
mediator_out_effects = readRDS(paste0(med_out_res_directory, "mediator_outcome_effects.RDS")) %>% 
  mutate(type = ifelse(outcome %in% continuous_outcomes, "cont", "cat")) %>% 
  label_mediators() %>% 
  label_outcomes() %>% 
  filter(!str_detect(mediator_label, "(Symptomatic)"))

# Plot all mediators ----
med_out_cat_all_mediators = 
  plot_effects(df = mediator_out_effects, 
               y_var = "mediator_label", 
               facet_var = "outcome_label", 
               y_type = "cat",   
               color_var = "mediator_group",
               include_facets = TRUE,
               scale = "fixed",
               plt_title = "A) Categorical Outcomes") + 
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"))

med_out_cont_all_mediators = 
  plot_effects(df = mediator_out_effects, 
               y_var = "mediator_label", 
               facet_var = "outcome_label", 
               y_type = "cont",   
               color_var = "mediator_group",
               include_facets = TRUE,
               scale = "fixed",
               plt_title = "B) Continuous Outcomes") + 
  xlab("Mean Difference (95% CI)") + 
  theme(legend.position = "none")

med_out_plot_all_mediators = 
  grid.arrange(med_out_cat_all_mediators, med_out_cont_all_mediators, 
               ncol = 1, heights = c(5, 4))

ggsave(here("figures", "mediator-outcome-effects", "med_out_effects_all_mediators.png"), 
       med_out_plot_all_mediators,
       width = 10, height = 8, bg = "white")

# Plot subset of mediators ----
med_out_cat_subset_mediators = 
  plot_effects(df = mediator_out_effects %>% filter_mediators(), 
               y_var = "mediator_label", 
               facet_var = "outcome_label", 
               y_type = "cat",   
               color_var = "mediator_group",
               include_facets = TRUE,
               scale = "fixed",
               plt_title = "A) Categorical Outcomes") + 
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines")) + 
  geom_hline(yintercept = 8.5, linewidth = 6, color = "white") + 
  geom_hline(yintercept = 7.5, linewidth = 6, color = "white") + 
  geom_hline(yintercept = 6.5, linewidth = 6, color = "white") + 
  geom_hline(yintercept = 3.5, linewidth = 6, color = "white") 

med_out_cont_subset_mediators = 
  plot_effects(df = mediator_out_effects %>% filter_mediators(), 
               y_var = "mediator_label", 
               facet_var = "outcome_label", 
               y_type = "cont",   
               color_var = "mediator_group",
               include_facets = TRUE,
               scale = "fixed",
               plt_title = "B) Continuous Outcomes") + 
  xlab("Mean Difference (95% CI)") + 
  theme(legend.position = "none")

med_out_plot_subset_mediators = 
  grid.arrange(med_out_cat_subset_mediators, med_out_cont_subset_mediators, 
               ncol = 1, heights = c(6.5, 4))

ggsave(here("figures", "mediator-outcome-effects", "med_out_effects_subset_mediators.png"), 
       med_out_plot_subset_mediators,
       width = 10, height = 6, bg = "white")
