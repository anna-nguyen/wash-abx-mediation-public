
rm(list = ls())
source(here::here("0-config.R"))

merged_df = readRDS(merged_df_filepath)

tr_out_effects = readRDS(paste0(tr_out_res_directory, "tr_out_effects.RDS")) %>% 
  mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>% 
  filter(!(outcome %in% continuous_outcomes)) %>% 
  mutate(prev_ratio = glue("{est} ({ci_lb}, {ci_ub})")) %>% 
  select(tr, outcome, prev_ratio)

tr_out_effects_PDs = readRDS(paste0(tr_out_res_directory, "tr_out_effects_PDs.RDS")) %>% 
  mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>% 
  mutate(prev_diff = glue("{est} ({ci_lb}, {ci_ub})")) %>% 
  select(tr, outcome, prev_diff)

format_table_s5 = function(tr_str, out_str) {
  df_intervention = merged_df %>% filter(!!sym(tr_str) != "Control") 
  risk_intervention = washb_mean(df_intervention %>% pull(!!sym(out_str)), 
                                 id = df_intervention$block, print = F) %>% 
    as.data.frame() %>% 
    mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>%
    mutate(tr = tr_str, outcome = out_str, risk_intervention = glue("{Mean} ({`Lower 95%CI`}, {`Upper 95%CI`})")) %>% 
    select(tr, outcome, risk_intervention)
  
  df_control = merged_df %>% filter(!!sym(tr_str)  == "Control")
  risk_control = washb_mean(df_control %>% pull(!!sym(out_str)), 
                            id = df_control$block, print = F) %>% 
    as.data.frame()  %>% 
    mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>%
    mutate(tr = tr_str, outcome = out_str, risk_control = glue("{Mean} ({`Lower 95%CI`}, {`Upper 95%CI`})")) %>% 
    select(tr, outcome, risk_control)
  
  return(
    risk_control %>% 
      left_join(risk_intervention, by = c("tr", "outcome")) %>% 
      left_join(tr_out_effects_PDs, by = c("tr", "outcome"))%>% 
      left_join(tr_out_effects, by = c("tr", "outcome")) 
  )
}

tr_out_grid = expand_grid(tr = tr_list, out = outcome_list)

table_s5 = lapply(1:nrow(tr_out_grid), function(i) format_table_s5(tr_out_grid$tr[i], tr_out_grid$out[i])) %>% 
  bind_rows() %>% 
  label_treatments() %>% 
  label_outcomes() %>% 
  arrange(treatment_label, outcome_label) %>% 
  select(treatment_label, outcome_label, everything(), -tr, -outcome) %>% 
  mutate(prev_ratio = ifelse(is.na(prev_ratio), "----", prev_ratio))
  
write_csv(table_s5, here::here("tables", "TableS5-tr_outcome_effects.csv"))

table3 = table_s5 %>% filter(treatment_label == "Pooled Intervention")

write_csv(table3, here::here("tables", "Table3-pooled_tr_outcome_effects.csv"))

