
rm(list = ls())
source(here::here("0-config.R"))

# Table S1: Covariate Adjustments for Mediation ----
table_s1 = readRDS(paste0(mediated_effects_results_directory, "mediated_effects.RDS")) %>% 
  filter(!(mediator %in% symp_mediators)) %>% 
  label_treatments() %>% 
  label_mediators() %>% 
  label_outcomes() %>% 
  label_covariates() %>% 
  arrange(treatment_label, mediator_label, outcome_label) %>% 
  select(treatment_label, mediator_label, outcome_label, covars)

write_csv(table_s1, here::here("tables", "TableS1-adjusted_covar_list.csv"))

# Table S8: Mediated Effects, no interactions ----
mediated_effects = readRDS(paste0(mediated_effects_results_directory, "mediated_effects.RDS")) %>% 
  mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>% 
  mutate(prev_diff = glue("{ACME_treated} ({ACME_treated_ci_lb}, {ACME_treated_ci_ub})")) %>% 
  select(tr, mediator, outcome, prev_diff)

mediated_effects_RR = readRDS(paste0(mediated_effects_results_directory, "mediated_effects_RR.RDS")) %>% 
  filter(!(outcome %in% continuous_outcomes)) %>% 
  mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>% 
  mutate(prev_ratio = glue("{ACME_treated} ({ACME_treated_ci_lb}, {ACME_treated_ci_ub})")) %>% 
  select(tr, mediator, outcome, prev_ratio)

mediated_effects_s8 = mediated_effects %>% filter(!(mediator %in% symp_mediators))
mediated_effects_RR_s8 = mediated_effects_RR  %>% filter(!(mediator %in% symp_mediators))

table_s8 = mediated_effects_s8 %>% 
  left_join(mediated_effects_RR_s8, by = c("tr", "mediator", "outcome")) %>% 
  mutate(prev_ratio = ifelse(is.na(prev_ratio), "----", prev_ratio)) %>% 
  label_treatments() %>% 
  label_mediators() %>% 
  label_outcomes() %>% 
  arrange(treatment_label, mediator_label, outcome_label) %>% 
  select(treatment_label, mediator_label, outcome_label, everything(), -tr, -mediator, -outcome)

write_csv(table_s8, here::here("tables", "TableS8-mediated_effects_no_interaction.csv"))


# Table s15: Mediated Effects, no interactions, diarrheal etiology ----
mediated_effects_s15 = mediated_effects %>% filter(mediator %in% symp_mediators, mediator != "n_virus_symp") 
mediated_effects_RR_s15 = mediated_effects_RR  %>% filter(mediator %in% symp_mediators, mediator != "n_virus_symp")

table_s15 = mediated_effects_s15 %>% 
  left_join(mediated_effects_RR_s15, by = c("tr", "mediator", "outcome")) %>% 
  mutate(prev_ratio = ifelse(is.na(prev_ratio), "----", prev_ratio)) %>% 
  label_treatments() %>% 
  label_mediators() %>% 
  label_outcomes() %>% 
  arrange(treatment_label, mediator_label, outcome_label) %>% 
  select(treatment_label, mediator_label, outcome_label, everything(), -tr, -mediator, -outcome)

write_csv(table_s15, here::here("tables", "TableS15-symp_mediated_effects_no_interaction.csv"))

# Table s9: Mediated Effects, interactions ----
table_s9 = readRDS(paste0(mediated_effects_results_directory, "mediated_effects_with_interaction.RDS")) %>% 
  filter(!(mediator %in% symp_mediators)) %>% 
  mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>% 
  mutate(diff_control = glue("{ACME_control} ({ACME_control_ci_lb}, {ACME_control_ci_ub})"), 
         diff_tr = glue("{ACME_treated} ({ACME_treated_ci_lb}, {ACME_treated_ci_ub})")) %>% 
  select(tr, mediator, outcome, diff_control, diff_tr) %>% 
  label_treatments() %>% 
  label_mediators() %>% 
  label_outcomes() %>% 
  arrange(treatment_label, mediator_label, outcome_label) %>% 
  select(treatment_label, mediator_label, outcome_label, everything(), -tr, -mediator, -outcome)

write_csv(table_s9, here::here("tables", "TableS9-mediated_effects_with_interaction.csv"))


