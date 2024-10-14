rm(list = ls())
source(here::here("0-config.R"))

# Load treatment-mediator effect estimates ----
tr_mediator_effects = readRDS(paste0(tr_med_res_directory, "tr_mediator_effects.RDS")) %>% 
  mutate(type = ifelse(mediator == "n_virus", "cont", "cat")) %>% 
  label_treatments() %>% 
  label_mediators() %>% 
  label_sig_effects() %>% 
  filter(!str_detect(mediator_label, "(Symptomatic)"))

# Pooled treatment, all mediators ----
tr_med_cat_pooled_tr = 
  plot_effects(df = tr_mediator_effects %>% filter(tr == "tr_pooled"), 
               y_var = "mediator_label", 
               facet_var = "treatment_label", 
               y_type = "cat", 
               color_var = "mediator_group",
               include_facets = FALSE,
               plt_title = "A) Categorical Mediators") + 
  theme(legend.position = "none")

tr_med_cont_pooled_tr = 
  plot_effects(df = tr_mediator_effects %>% filter(tr == "tr_pooled"),  
               y_var = "mediator_label", 
               facet_var = "treatment_label", 
               y_type = "cont",  
               color_var = "mediator_group",
               include_facets = FALSE,
               plt_title = "B) Continuous Mediators") + 
  xlab("Mean Difference (95% CI)") + 
  theme(legend.position = "none")

tr_med_plot_pooled_tr = grid.arrange(tr_med_cat_pooled_tr, tr_med_cont_pooled_tr,
                                     ncol = 1,
                                     heights = c(5, 2.5))

ggsave(here("figures", "treatment-mediator-effects", "tr_med_effects_pooled_tr_all_mediators.png"), 
       tr_med_plot_pooled_tr,
       width = 7, height = 6)

# Pooled treatment, filtered mediators ----
tr_med_cat_pooled_tr_subset_mediators = 
  plot_effects(df = tr_mediator_effects %>% filter_mediators() %>% filter(tr == "tr_pooled"), 
               y_var = "mediator_label", 
               facet_var = "treatment_label", 
               y_type = "cat",  
               color_var = "mediator_group",
               include_facets = FALSE,
               plt_title = "A) Categorical Mediators") + 
  theme(legend.position = "none") + 
  geom_hline(yintercept = 7.5, linewidth = 8, color = "white") + 
  geom_hline(yintercept = 6.5, linewidth = 8, color = "white") + 
  geom_hline(yintercept = 5.5, linewidth = 8, color = "white") +
  geom_hline(yintercept = 2.5, linewidth = 8, color = "white")


tr_med_cont_pooled_tr_subset_mediators = 
  plot_effects(df = tr_mediator_effects %>% filter(tr == "tr_pooled"),  
               y_var = "mediator_label", 
               facet_var = "treatment_label", 
               y_type = "cont",  
               color_var = "mediator_group",
               include_facets = FALSE,
               plt_title = "B) Continuous Mediators") + 
  xlab("Mean Difference (95% CI)") + 
  theme(legend.position = "none")

tr_med_plot_pooled_tr_subset_mediators = grid.arrange(tr_med_cat_pooled_tr_subset_mediators, 
                                                      tr_med_cont_pooled_tr_subset_mediators,
                                                      ncol = 1,
                                                      heights = c(5, 2.5))

ggsave(here("figures", "treatment-mediator-effects", "tr_med_effects_pooled_tr_subset_mediators.png"), tr_med_plot_pooled_tr_subset_mediators,
       width = 7, height = 6)

# All treatment groups, all mediators ----
tr_med_cat_all_tr = 
  plot_effects(df = tr_mediator_effects, 
               y_var = "mediator_label", 
               facet_var = "treatment_label", 
               y_type = "cat",  
               color_var = "mediator_group",
               include_facets = TRUE,
               scale = "fixed",
               plt_title = "A) Categorical Mediators") + 
  theme(legend.position = "none")

tr_med_cont_all_tr = 
  plot_effects(df = tr_mediator_effects, 
               y_var = "mediator_label", 
               facet_var = "treatment_label", 
               y_type = "cont",  
               color_var = "mediator_group",
               include_facets = TRUE,
               scale = "fixed",
               plt_title = "B) Continuous Mediators") + 
  xlab("Mean Difference (95% CI)") + 
  theme(legend.position = "none")

tr_med_plot_all_tr = grid.arrange(tr_med_cat_all_tr, tr_med_cont_all_tr,
                                     ncol = 1,
                                     heights = c(5, 2.5))

ggsave(here("figures", "treatment-mediator-effects", "tr_med_effects_all_tr_all_mediators.png"), 
       tr_med_plot_all_tr,
       width = 11, height = 6)

# All treatment, filtered mediators ----
tr_med_cat_all_tr_subset_mediators = 
  plot_effects(df = tr_mediator_effects %>% filter_mediators(), 
               y_var = "mediator_label", 
               facet_var = "treatment_label", 
               y_type = "cat",  
               color_var = "mediator_group",
               include_facets = TRUE,
               scale = "fixed",
               plt_title = "A) Categorical Mediators") + 
  theme(legend.position = "none") + 
  geom_hline(yintercept = 7.5, linewidth = 8, color = "white") + 
  geom_hline(yintercept = 6.5, linewidth = 8, color = "white") + 
  geom_hline(yintercept = 5.5, linewidth = 8, color = "white") + 
  geom_hline(yintercept = 2.5, linewidth = 8, color = "white")

tr_med_cont_all_tr_subset_mediators = 
  plot_effects(df = tr_mediator_effects, 
               y_var = "mediator_label", 
               facet_var = "treatment_label", 
               y_type = "cont",  
               color_var = "mediator_group",
               include_facets = TRUE,
               scale = "fixed",
               plt_title = "B) Continuous Mediators") + 
  xlab("Mean Difference (95% CI)") + 
  theme(legend.position = "none")


tr_med_plot_all_tr_subset_mediators = grid.arrange(tr_med_cat_all_tr_subset_mediators,
                                                   tr_med_cont_all_tr_subset_mediators,
                                                   ncol = 1,
                                                   heights = c(5, 2.5))

ggsave(here("figures", "treatment-mediator-effects", "tr_med_effects_all_tr_subset_mediators.png"), tr_med_plot_all_tr_subset_mediators,
       width = 11, height = 6)
