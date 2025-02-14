
rm(list = ls())
source(here::here("0-config.R"))
set.seed(0)

# Load data
merged_df = readRDS(merged_df_filepath) 

# Create grid of all treatment + outcome combinations
tr_out_grid = expand_grid(tr_list, outcome_list) %>%
  rename(x = tr_list, y = outcome_list) %>% 
  mutate(formula_string = glue("{y} ~ {x} + agem"), 
         y_type = ifelse(y %in% continuous_outcomes, "continuous", "categorical"))

# Unadjusted effects, on relative scale
tr_out_effects = 
  lapply(1:nrow(tr_out_grid), 
         function(i) fit_tr_outcome_model(df = merged_df,
                                          model_params_row = tr_out_grid[i, ])) %>% 
  bind_rows() %>% 
  rename(tr = x, outcome = y) %>% 
  mutate(sig_effect = ifelse(pval < 0.05, T, F))

saveRDS(tr_out_effects, paste0(tr_out_res_directory, "tr_out_effects.RDS"))

# Unadjusted effects, on absolute scale
tr_out_effects_PDs = 
  lapply(1:nrow(tr_out_grid), 
         function(i) fit_tr_outcome_model(df = merged_df,
                                          model_params_row = tr_out_grid[i, ],
                                          cat_estimate_type = "PD")) %>% 
  bind_rows() %>% 
  rename(tr = x, outcome = y) %>% 
  mutate(sig_effect = ifelse(ci_lb > 0 | ci_ub < 0, T, F))

saveRDS(tr_out_effects_PDs, paste0(tr_out_res_directory, "tr_out_effects_PDs.RDS"))

# Adjusted effects, on relative scale
tr_out_effects_adjusted = 
  lapply(1:nrow(tr_out_grid), 
         function(i) fit_tr_outcome_model(df = merged_df,
                                          model_params_row = tr_out_grid[i, ],
                                          adjusted = TRUE)) %>% 
  bind_rows() %>% 
  rename(tr = x, outcome = y) %>% 
  mutate(sig_effect = ifelse(pval < 0.05, T, F))

saveRDS(tr_out_effects_adjusted, paste0(tr_out_res_directory, "tr_out_effects_adjusted.RDS"))

# Adjusted effects, on absolute scale
tr_out_effects_adjusted_PDs = 
  lapply(1:nrow(tr_out_grid), 
         function(i) fit_tr_outcome_model(df = merged_df,
                                          model_params_row = tr_out_grid[i, ],
                                          adjusted = TRUE,
                                          cat_estimate_type = "PD")) %>% 
  bind_rows() %>% 
  rename(tr = x, outcome = y) %>% 
  mutate(sig_effect = ifelse(ci_lb > 0 | ci_ub < 0, T, F))

saveRDS(tr_out_effects_adjusted_PDs, paste0(tr_out_res_directory, "tr_out_effects_adjusted_PDs.RDS"))

