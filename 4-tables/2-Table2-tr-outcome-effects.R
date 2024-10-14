
rm(list = ls())
source(here::here("0-config.R"))

tr_out_effects = readRDS(paste0(tr_out_res_directory, "tr_out_effects.RDS")) %>% 
  filter(!(outcome %in% continuous_outcomes)) %>% 
  mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>% 
  mutate(PR = glue("{est} ({ci_lb}, {ci_ub})")) %>% 
  select(tr, outcome, PR)

tr_out_effects_PDs = readRDS(paste0(tr_out_res_directory, "tr_out_effects_PDs.RDS")) %>% 
  mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>% 
  mutate(PD = glue("{est} ({ci_lb}, {ci_ub})")) %>% 
  select(tr, outcome, PD)

table2 = tr_out_effects_PDs %>% 
  left_join(tr_out_effects, by = c("tr", "outcome")) %>% 
  filter(tr == "tr_pooled") %>% 
  label_outcomes() %>% 
  select(Outcome = outcome_label, 
         `Prevalence/Mean Difference` = PD, 
         `Prevalence Ratio` = PR) %>% 
  mutate(Outcome = str_replace(Outcome, "\n", " "))

write_csv(table2, here::here("tables", "Table2-pooled_tr_outcome_effects.csv"))
