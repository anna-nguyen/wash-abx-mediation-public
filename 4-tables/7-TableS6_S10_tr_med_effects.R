
rm(list = ls())
source(here::here("0-config.R"))

merged_df = readRDS(merged_df_filepath)

tr_med_effects = readRDS(paste0(tr_med_res_directory, "tr_mediator_effects.RDS")) %>% 
  mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>% 
  mutate(effect_est = glue("{est} ({ci_lb}, {ci_ub})"),
         mean_diff = ifelse(mediator %in% continuous_mediators, effect_est, "----"),
         prev_ratio = ifelse(mediator %in% continuous_mediators, "----", effect_est)) %>% 
  select(tr, mediator, mean_diff, prev_ratio)

format_table_s6 = function(tr_str, med_str) {
  df_intervention = merged_df %>% filter(!!sym(tr_str) != "Control") 
  risk_intervention = washb_mean(df_intervention %>% pull(!!sym(med_str)), 
                                 id = df_intervention$block, print = F) %>% 
    as.data.frame() %>% 
    mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>%
    mutate(tr = tr_str, mediator = med_str, risk_intervention = glue("{Mean} ({`Lower 95%CI`}, {`Upper 95%CI`})")) %>% 
    select(tr, mediator, risk_intervention)
  
  df_control = merged_df %>% filter(!!sym(tr_str)  == "Control")
  risk_control = washb_mean(df_control %>% pull(!!sym(med_str)), 
                            id = df_control$block, print = F) %>% 
    as.data.frame()  %>% 
    mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>%
    mutate(tr = tr_str, mediator = med_str, risk_control = glue("{Mean} ({`Lower 95%CI`}, {`Upper 95%CI`})")) %>% 
    select(tr, mediator, risk_control)
  
  return(
    risk_control %>% 
      left_join(risk_intervention, by = c("tr", "mediator")) %>% 
      left_join(tr_med_effects, by = c("tr", "mediator")) 
  )
}

tr_med_grid = expand_grid(tr = tr_list, med = mediator_list)
tr_med_grid_s6 = tr_med_grid %>% filter(!(med %in% symp_mediators))
tr_med_grid_s10 = tr_med_grid %>% filter(med %in% symp_mediators, med != "n_virus_symp")

table_s6 = lapply(1:nrow(tr_med_grid_s6), function(i) format_table_s6(tr_med_grid_s6$tr[i], tr_med_grid_s6$med[i])) %>% 
  bind_rows() %>% 
  label_treatments() %>% 
  label_mediators() %>% 
  arrange(treatment_label, mediator_label) %>% 
  select(treatment_label, mediator_label, everything(), -tr, -mediator) 

write_csv(table_s6, here::here("tables", "TableS6-tr_mediator_effects.csv"))

table_s10 = lapply(1:nrow(tr_med_grid_s10), function(i) format_table_s6(tr_med_grid_s10$tr[i], tr_med_grid_s10$med[i])) %>% 
  bind_rows() %>% 
  label_treatments() %>% 
  label_mediators() %>% 
  arrange(treatment_label, mediator_label) %>% 
  select(treatment_label, mediator_label, everything(), -tr, -mediator, -mean_diff) 

write_csv(table_s10, here::here("tables", "TableS10-symp_tr_mediator_effects.csv"))


