rm(list = ls())
source(here::here("0-config.R"))

# Load treatment-mediator effect estimates ----
tr_mediator_effects = readRDS(paste0(tr_med_res_directory, "tr_mediator_effects.RDS")) %>% 
  mutate(type = ifelse(str_detect(mediator, "n_virus"), "cont", "cat")) %>% 
  label_treatments() %>% 
  label_mediators() %>% 
  label_sig_effects() %>% 
  filter(str_detect(mediator_label, "(Symptomatic)"),
         !str_detect(mediator_label, "Number")) %>% 
  mutate(mediator = str_remove(mediator, "_symp")) %>% 
  select(-mediator_group) %>% 
  label_mediators()

# Treatment-mediator plot ----
tr_med_cat_all_tr = 
  plot_effects(df = tr_mediator_effects, 
               y_var = "mediator_label", 
               facet_var = "treatment_label", 
               y_type = "cat",  
               color_var = "mediator_group",
               include_facets = TRUE,
               plt_title = "",
               scale = "fixed") + 
  theme(legend.position = "none")

ggsave(here("figures", "effects-virus-diarrhea-etiology", "tr_med_effects_symp.png"), 
       tr_med_cat_all_tr,
       width = 11, height = 4, bg = "white")

# Mediator-outcome plot ----
mediator_out_effects = readRDS(paste0(med_out_res_directory, "mediator_outcome_effects.RDS")) %>% 
  mutate(type = ifelse(outcome %in% continuous_outcomes, "cont", "cat")) %>% 
  label_mediators() %>% 
  label_outcomes() %>% 
  filter(str_detect(mediator_label, "(Symptomatic)"),
         !str_detect(mediator_label, "Number")) %>% 
  mutate(mediator = str_remove(mediator, "_symp")) %>% 
  select(-mediator_group) %>% 
  label_mediators()

med_out_cat_all_mediators = 
  plot_effects(df = mediator_out_effects, 
               y_var = "mediator_label", 
               facet_var = "outcome_label", 
               y_type = "cat",   
               color_var = "mediator_group",
               include_facets = TRUE,
               plt_title = "A) Categorical Outcomes", 
               scale = "free_y") + 
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"))

med_out_cont_all_mediators = 
  plot_effects(df = mediator_out_effects, 
               y_var = "mediator_label", 
               facet_var = "outcome_label", 
               y_type = "cont",   
               color_var = "mediator_group",
               include_facets = TRUE,
               scale = "free_y",
               plt_title = "B) Continuous Outcomes") + 
  xlab("Mean Difference (95% CI)") + 
  theme(legend.position = "none")

med_out_plot_all_mediators = 
  grid.arrange(med_out_cat_all_mediators, med_out_cont_all_mediators, 
               ncol = 1)

ggsave(here("figures", "effects-virus-diarrhea-etiology", "med_out_effects_symp.png"), 
       med_out_plot_all_mediators,
       width = 11, height = 6, bg = "white")

# Mediated effects plot ----

mediated_effects = readRDS(paste0(mediated_effects_results_directory, "mediated_effects.RDS"))

plt_df_mediated_effects = mediated_effects %>% 
  select(tr, mediator, outcome, est = ACME_treated, ci_lb = ACME_treated_ci_lb, ci_ub = ACME_treated_ci_ub) %>% 
  mutate(out_type = "cont", 
         sig_effect = ifelse((ci_lb > 0 | ci_ub < 0), TRUE, FALSE)) %>% 
  label_treatments() %>% 
  label_mediators() %>% 
  label_outcomes(include_time_labels = FALSE) %>% 
  label_sig_effects() %>% 
  filter(str_detect(mediator_label, "(Symptomatic)"),
         !str_detect(mediator_label, "Number")) %>% 
  mutate(mediator = str_remove(mediator, "_symp")) %>% 
  select(-mediator_group) %>% 
  label_mediators()

ablastmo_all_tr_all_mediators = 
  plot_mediated_effects(df = plt_df_mediated_effects %>% filter(outcome == "ablastmo"),
                        facet_var = "treatment_label", 
                        includes_facets = TRUE, 
                        scale = "fixed") +
  ggtitle("Mediated Effects on Antibiotic Use in Past Month") + 
  theme(legend.position = "none")

ggsave(here("figures", "effects-virus-diarrhea-etiology", "med_effects_symp.png"), 
       ablastmo_all_tr_all_mediators,
       width = 11, height = 3, bg = "white")
