rm(list = ls())
source(here::here("0-config.R"))

mediated_effects = readRDS(paste0(mediated_effects_results_directory, "mediated_effects.RDS"))

plt_df_mediated_effects = mediated_effects %>% 
  select(tr, mediator, outcome, est = ACME_treated, ci_lb = ACME_treated_ci_lb, ci_ub = ACME_treated_ci_ub) %>% 
  mutate(out_type = "cont", 
         sig_effect = ifelse((ci_lb > 0 | ci_ub < 0), TRUE, FALSE)) %>% 
  label_treatments() %>% 
  label_mediators() %>% 
  label_outcomes(include_time_labels = FALSE) %>% 
  label_sig_effects() %>% 
  filter(!str_detect(mediator_label, "(Symptomatic)"))

# Plot pooled treatment, all mediators ----
abx_past_month_pooled_tr_all_mediators = 
  plot_mediated_effects(df = plt_df_mediated_effects %>% filter(tr == "tr_pooled", outcome == "ablastmo"),
                        facet_var = "outcome_label", 
                        includes_facets = TRUE) +
  ggtitle("A) Antibiotic Use in Past Month") + 
  theme(legend.position = "none")

abx_past_3_months_pooled_tr_all_mediators = 
  plot_mediated_effects(df = plt_df_mediated_effects %>% filter(tr == "tr_pooled", outcome != "ablastmo"),
                        facet_var = "outcome_label", 
                        includes_facets = TRUE, 
                        n_facet_row = 2) +
  xlab("Differences (95% CI)") +
  ggtitle("B) Antibiotic Use in Past 3 Months") + 
  theme(legend.position = "none")

med_effects_pooled_tr_all_mediators = 
  grid.arrange(abx_past_month_pooled_tr_all_mediators, abx_past_3_months_pooled_tr_all_mediators, 
               nrow = 1)

ggsave(here("figures", "mediated-effects", "med_effects_pooled_tr_all_mediators.png"), 
       med_effects_pooled_tr_all_mediators,
       width = 12, height = 6, bg = "white")

# Plot pooled treatment, subset mediators ----
abx_past_month_pooled_tr_subset_mediators = 
  plot_mediated_effects(df = plt_df_mediated_effects %>% filter_mediators() %>% filter(tr == "tr_pooled", outcome == "ablastmo"),
                        facet_var = "outcome_label", 
                        includes_facets = TRUE) +
  ggtitle("A) Antibiotic Use in Past Month")  + 
  theme(legend.position = "none") + 
  geom_hline(yintercept = 8.5, linewidth = 6, color = "white") + 
  geom_hline(yintercept = 7.5, linewidth = 6, color = "white") + 
  geom_hline(yintercept = 6.5, linewidth = 6, color = "white") + 
  geom_hline(yintercept = 4.5, linewidth = 6, color = "white") + 
  geom_hline(yintercept = 3.5, linewidth = 6, color = "white") 

abx_past_3_months_pooled_tr_subset_mediators = 
  plot_mediated_effects(df = plt_df_mediated_effects %>% filter_mediators() %>% filter(tr == "tr_pooled", outcome != "ablastmo"),
                        facet_var = "outcome_label", 
                        includes_facets = TRUE,
                        n_facet_row = 2) +  
  xlab("Differences (95% CI)") +
  ggtitle("B) Antibiotic Use in Past 3 Months")  + 
  theme(legend.position = "none")

med_effects_pooled_tr_subset_mediators = 
  grid.arrange(abx_past_month_pooled_tr_subset_mediators, abx_past_3_months_pooled_tr_subset_mediators, 
               nrow = 1)

ggsave(here("figures", "mediated-effects", "med_effects_pooled_tr_subset_mediators.png"), 
       med_effects_pooled_tr_subset_mediators,
       width = 12, height = 6, bg = "white")

# Plot all treatment groups, all mediators ----
ablastmo_all_tr_all_mediators = 
  plot_mediated_effects(df = plt_df_mediated_effects %>% filter(outcome == "ablastmo"),
                        facet_var = "treatment_label", 
                        includes_facets = TRUE, 
                        scale = "fixed") +
  ggtitle("A) Antibiotic Use in Past Month") + 
  theme(legend.position = "none")

abany_all_tr_all_mediators = 
  plot_mediated_effects(df = plt_df_mediated_effects %>% filter(outcome == "abany"),
                        facet_var = "treatment_label", 
                        includes_facets = TRUE, 
                        scale = "fixed") +
  ggtitle("B) Antibiotic Use in Past 3 Months") + 
  theme(legend.position = "none")

abtimes_all_tr_all_mediators = 
  plot_mediated_effects(df = plt_df_mediated_effects %>% filter(tr != "tr_pooled", outcome == "abtimes"),
                        facet_var = "treatment_label", 
                        includes_facets = TRUE, 
                        scale = "fixed") +
  ggtitle("C) Episodes of Antibiotic Use in Past 3 Months") + 
  theme(legend.position = "none")

abmult_all_tr_all_mediators = 
  plot_mediated_effects(df = plt_df_mediated_effects %>% filter(tr != "tr_pooled", outcome == "abmult"),
                        facet_var = "treatment_label", 
                        includes_facets = TRUE, 
                        scale = "fixed") +
  ggtitle("D) Multiple Episodes of Antibiotic Use in Past 3 Months") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = c(-0.02, -0.01, 0))


med_effects_all_tr_all_mediators = 
  grid.arrange(ablastmo_all_tr_all_mediators, abany_all_tr_all_mediators, 
               abtimes_all_tr_all_mediators, abmult_all_tr_all_mediators,
               ncol = 1, 
               heights = c(5, 3, 3, 3))

ggsave(here("figures", "mediated-effects", "med_effects_all_tr_all_mediators.png"), 
       med_effects_all_tr_all_mediators,
       width = 14, height = 10, bg = "white")

