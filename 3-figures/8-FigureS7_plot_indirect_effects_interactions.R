rm(list = ls())
source(here::here("0-config.R"))

mediated_effects_with_interaction = readRDS(paste0(mediated_effects_results_directory, "mediated_effects_with_interaction.RDS")) %>%
  mutate(sig_interaction = case_when(interaction_pval < 0.2 ~ TRUE, 
                                     !(outcome %in% continuous_outcomes) & abs(interaction_diff_est) >= 0.01 ~ TRUE,
                                     T ~ FALSE)) %>% 
  filter(sig_interaction)

average_acme = mediated_effects_with_interaction %>% 
  select(tr, mediator, outcome, est = ACME_average, ci_lb = ACME_average_ci_lb, ci_ub = ACME_average_ci_ub) %>% 
  mutate(type = "Average")

treated_acme = mediated_effects_with_interaction %>% 
  select(tr, mediator, outcome, est = ACME_treated, ci_lb = ACME_treated_ci_lb, ci_ub = ACME_treated_ci_ub) %>% 
  mutate(type = "Total Natural Indirect Effect")

control_acme = mediated_effects_with_interaction %>% 
  select(tr, mediator, outcome, est = ACME_control, ci_lb = ACME_control_ci_lb, ci_ub = ACME_control_ci_ub) %>% 
  mutate(type = "Pure Natural Indirect Effect")

acme_plot_df = average_acme %>% 
  bind_rows(treated_acme) %>% 
  bind_rows(control_acme) %>% 
  mutate(type = factor(type, levels = c("Average", "Total Natural Indirect Effect", "Pure Natural Indirect Effect"))) %>% 
  label_treatments() %>% 
  label_mediators() %>% 
  label_outcomes(include_time_labels = FALSE) %>% 
  filter(!(str_detect(mediator_label, "(Symptomatic)")))

# Plot pooled treatment ----
med_interactions_1month_pooled_tr = 
  plot_mediation_with_interactions(df = acme_plot_df %>% filter(tr == "tr_pooled", outcome == "ablastmo"),
                                   facet_var = "outcome_label", 
                                   include_facets = TRUE)  +
  ggtitle("A) Antibiotic Use in Past Month") 

med_interactions_3month_pooled_tr = 
  plot_mediation_with_interactions(df = acme_plot_df %>% filter(tr == "tr_pooled", outcome != "ablastmo"),
                                   facet_var = "outcome_label", 
                                   include_facets = TRUE)  +
  ggtitle("B) Antibiotic Use in Past 3 Months")


med_interactions_pooled_tr = 
  grid.arrange(med_interactions_1month_pooled_tr, med_interactions_3month_pooled_tr, 
               nrow = 1)

ggsave(here("figures", "mediated-effects", "med_effects_interactions_pooled_tr.png"), 
       med_interactions_pooled_tr,
       width = 10, height = 4, bg = "white")


# Plot all treatment groups ----

plot_tr_group_grid = function(outcome_tbl_row) {
  med_interactions_1month_pooled_tr = 
    plot_mediation_with_interactions(df = acme_plot_df %>% filter(outcome == outcome_tbl_row["out_str"]),
                                     facet_var = "treatment_label", 
                                     include_facets = TRUE) +
    ggtitle(glue("{outcome_tbl_row['letter_label']}) {outcome_tbl_row['out_label_str']}")) 
  
  return(med_interactions_1month_pooled_tr)
}

outcome_tbl = acme_plot_df %>% 
  label_outcomes(include_time_labels = TRUE) %>%
  arrange(outcome_label) %>% 
  select(out_str = outcome, out_label_str = outcome_label) %>% 
  distinct() %>% 
  mutate(letter_label = LETTERS[1:nrow(.)],
         out_label_str = str_replace(out_label_str, "\n", " "))

tr_interaction_plots = apply(outcome_tbl, 1, plot_tr_group_grid)
tr_interaction_grid = 
  grid.arrange(tr_interaction_plots[[1]] + theme(legend.position = "none"),
               tr_interaction_plots[[2]] + theme(legend.position = "none"), 
               tr_interaction_plots[[3]] + theme(legend.position = "none"),
               grid.arrange(tr_interaction_plots[[4]], ggplot(), nrow = 1), 
               heights = c(5, 2, 2, 3),
               ncol = 1)

ggsave(here("figures", "mediated-effects", "med_effects_interactions_all_tr.png"), 
       tr_interaction_grid,
       width = 13, height = 10, bg = "white")
