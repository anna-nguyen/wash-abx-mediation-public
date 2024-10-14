rm(list = ls())
source(here::here("0-config.R"))

# Load treatment-outcome effect estimates ----
tr_outcome_effects_PD = readRDS(paste0(tr_out_res_directory, "tr_out_effects_adjusted_PDs.RDS")) %>% 
  mutate(type = "cont", 
         out_typ = ifelse(outcome %in% continuous_outcomes, "cont", "cat")) %>% 
  label_treatments() %>% 
  label_outcomes() %>% 
  label_sig_effects()

tr_outcome_effects = readRDS(paste0(tr_out_res_directory, "tr_out_effects_adjusted.RDS")) %>% 
  mutate(type = ifelse(outcome %in% continuous_outcomes, "cont", "cat")) %>% 
  label_treatments() %>% 
  label_outcomes() %>% 
  label_sig_effects()

# Pooled treatment, absolute scale ----
tr_out_cat_pooled_tr_PD = 
  plot_effects(df = tr_outcome_effects_PD %>% filter(out_typ == "cat", tr == "tr_pooled"), 
               y_var = "outcome_label", 
               facet_var = "treatment_label", 
               y_type = "cont", 
               color_var = "outcome_group",
               include_facets = FALSE,
               plt_title = "A) Categorical Outcomes")

tr_out_cont_pooled_tr_PD = 
  plot_effects(df = tr_outcome_effects_PD %>% filter(out_typ == "cont", tr == "tr_pooled"), 
               y_var = "outcome_label", 
               facet_var = "treatment_label", 
               y_type = "cont", 
               color_var = "outcome_group",
               include_facets = FALSE,
               plt_title = "B) Continuous Outcomes") + 
  xlab("Mean Difference (95% CI)")

tr_out_plot_pooled_tr_PD = grid.arrange(tr_out_cat_pooled_tr_PD, tr_out_cont_pooled_tr_PD,
                                        ncol = 2)

ggsave(here("figures", "treatment-outcome-effects", "tr_out_effects_pooled_PD.png"), tr_out_plot_pooled_tr_PD,
       width = 11, height = 5, bg = "white")

# Pooled treatment, relative scale ----
tr_out_cat_pooled_tr = 
  plot_effects(df = tr_outcome_effects %>% filter(tr == "tr_pooled"), 
               y_var = "outcome_label", 
               facet_var = "treatment_label", 
               y_type = "cat", 
               color_var = "outcome_group",
               include_facets = FALSE,
               plt_title = "A) Categorical Outcomes")
  
tr_out_cont_pooled_tr = 
  plot_effects(df = tr_outcome_effects %>% filter(tr == "tr_pooled"),  
               y_var = "outcome_label", 
               facet_var = "treatment_label", 
               y_type = "cont", 
               color_var = "outcome_group",
               include_facets = FALSE,
               plt_title = "B) Continuous Outcomes") + 
  xlab("Mean Difference (95% CI)")

tr_out_plot_pooled_tr = grid.arrange(tr_out_cat_pooled_tr, tr_out_cont_pooled_tr, 
                                     ncol = 2)

ggsave(here("figures", "treatment-outcome-effects", "tr_out_effects_pooled_PR.png"), tr_out_plot_pooled_tr,
       width = 11, height = 5, bg = "white")

# All treatment groups, absolute scale ----
tr_out_cat_all_tr_PD = 
  plot_effects(df = tr_outcome_effects_PD %>% filter(out_typ == "cat"), 
               y_var = "outcome_label", 
               facet_var = "treatment_label", 
               y_type = "cont", 
               color_var = "outcome_group",
               include_facets = TRUE,
               plt_title = "A) Categorical Outcomes") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = c(-0.12, -0.08, -0.04, 0),
                     labels = c(-0.12, -0.08, -0.04, 0))

tr_out_cont_all_tr_PD = 
  plot_effects(df = tr_outcome_effects_PD %>% filter(out_typ == "cont"), 
               y_var = "outcome_label", 
               facet_var = "treatment_label", 
               y_type = "cont", 
               color_var = "outcome_group",
               include_facets = TRUE,
               plt_title = "B) Continuous Outcomes") + 
  xlab("Mean Difference (95% CI)")


tr_out_plot_all_tr_PD = grid.arrange(tr_out_cat_all_tr_PD, tr_out_cont_all_tr_PD,
                                     ncol = 1, 
                                     heights = c(5, 5))

ggsave(here("figures", "treatment-outcome-effects", "tr_out_effects_all_tr_PD.png"), tr_out_plot_all_tr_PD,
       width = 11, height = 6, bg = "white")

# All treatment, relative scale ----
tr_out_cat_all_tr = 
  plot_effects(df = tr_outcome_effects, 
               y_var = "outcome_label", 
               facet_var = "treatment_label", 
               y_type = "cat", 
               color_var = "outcome_group",
               include_facets = TRUE,
               plt_title = "A) Categorical Outcomes") + 
  theme(legend.position = "none")

tr_out_cont_all_tr = 
  plot_effects(df = tr_outcome_effects, 
               y_var = "outcome_label", 
               facet_var = "treatment_label", 
               y_type = "cont", 
               color_var = "outcome_group",
               include_facets = TRUE,
               plt_title = "B) Continuous Outcomes") + 
  xlab("Mean Difference (95% CI)")

tr_out_plot_all_tr = grid.arrange(tr_out_cat_all_tr, tr_out_cont_all_tr,
                                  ncol = 1, 
                                  heights = c(5, 5))

ggsave(here("figures", "treatment-outcome-effects", "tr_out_effects_all_tr_PR.png"), tr_out_plot_all_tr,
       width = 11, height = 6, bg = "white")

