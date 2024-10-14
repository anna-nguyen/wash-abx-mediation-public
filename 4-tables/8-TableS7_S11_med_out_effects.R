
rm(list = ls())
source(here::here("0-config.R"))

merged_df = readRDS(merged_df_filepath)

med_out_effects = readRDS(paste0(med_out_res_directory, "mediator_outcome_effects.RDS")) %>% 
  mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>% 
  mutate(effect_est = glue("{est} ({ci_lb}, {ci_ub})"),
         mean_diff = ifelse(outcome %in% continuous_outcomes, effect_est, "----"),
         prev_ratio = ifelse(outcome %in% continuous_outcomes, "----", effect_est)) %>% 
  select(mediator, outcome, mean_diff, prev_ratio)

format_table_s7 = function(med_str, out_str) {
  df_intervention = merged_df %>% filter(!!sym(med_str) != 0) 
  risk_intervention = washb_mean(df_intervention %>% pull(!!sym(out_str)), 
                                 id = df_intervention$block, print = F) %>% 
    as.data.frame() %>% 
    mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>%
    mutate(mediator = med_str, outcome = out_str, risk_intervention = glue("{Mean} ({`Lower 95%CI`}, {`Upper 95%CI`})")) %>% 
    select(mediator, outcome, risk_intervention)
  
  df_control = merged_df %>% filter(!!sym(med_str) == 0)
  risk_control = washb_mean(df_control %>% pull(!!sym(out_str)), 
                            id = df_control$block, print = F) %>% 
    as.data.frame()  %>% 
    mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>%
    mutate(mediator = med_str, outcome = out_str, risk_control = glue("{Mean} ({`Lower 95%CI`}, {`Upper 95%CI`})")) %>% 
    select(mediator, outcome, risk_control)
  
  return(
    risk_control %>% 
      left_join(risk_intervention, by = c("mediator", "outcome")) %>% 
      left_join(med_out_effects, by = c("mediator", "outcome")) 
  )
}

med_out_grid = expand_grid(med = mediator_list, out = outcome_list)

med_out_grid_s7 = med_out_grid %>% filter(!(med %in% symp_mediators))
med_out_grid_s11 = med_out_grid %>% filter(med %in% symp_mediators, med != "n_virus_symp")

table_s7 = lapply(1:nrow(med_out_grid_s7), function(i) format_table_s7(med_out_grid_s7$med[i], med_out_grid_s7$out[i])) %>% 
  bind_rows() %>% 
  label_mediators() %>% 
  label_outcomes() %>% 
  arrange(mediator_label, outcome_label) %>% 
  filter(!is.na(mean_diff) | !is.na(prev_ratio)) %>% 
  select(mediator_label, outcome_label, everything(), -mediator, -outcome)

write_csv(table_s7, here::here("tables", "TableS7-med_out_effects.csv"))

table_s11 = lapply(1:nrow(med_out_grid_s11), function(i) format_table_s7(med_out_grid_s11$med[i], med_out_grid_s11$out[i])) %>% 
  bind_rows() %>% 
  label_mediators() %>% 
  label_outcomes() %>% 
  arrange(mediator_label, outcome_label) %>% 
  filter(!is.na(mean_diff) | !is.na(prev_ratio)) %>% 
  select(mediator_label, outcome_label, everything(), -mediator, -outcome)

write_csv(table_s11, here::here("tables", "TableS11-symp_med_out_effects.csv"))

