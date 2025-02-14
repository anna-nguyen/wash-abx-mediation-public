rm(list = ls())
source(here::here("0-config.R"))

table_s16 = readRDS(paste0(mediated_effects_results_directory, "negcontrol_mediated_effects.RDS")) %>% 
  mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>% 
  mutate(prev_diff = glue("{ACME_treated} ({ACME_treated_ci_lb}, {ACME_treated_ci_ub})")) %>% 
  select(tr, mediator, outcome, prev_diff) %>% 
  label_treatments() %>% 
  label_mediators() %>% 
  label_outcomes() %>% 
  arrange(treatment_label, mediator_label, outcome_label) %>% 
  select(treatment_label, mediator_label, outcome_label, everything(), -tr, -mediator, -outcome)

write_csv(table_s16, here::here("tables", "TableS16-negative_control_mediated_effects.csv"))
