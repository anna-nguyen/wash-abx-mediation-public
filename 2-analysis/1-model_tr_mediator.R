rm(list = ls())
source(here::here("0-config.R"))
set.seed(0)

# Load data
merged_df = readRDS(merged_df_filepath)

# Create grid of all treatment + mediator combinations
tr_mediator_grid = expand_grid(tr_list, mediator_list) %>%
  rename(x = tr_list, y = mediator_list) %>% 
  mutate(formula_string = glue("{y} ~ {x} + agem"), 
         y_type = ifelse(y %in% continuous_mediators, "continuous", "categorical"))

# Unadjusted treatment-mediator effects
tr_mediator_effects = 
  lapply(1:nrow(tr_mediator_grid), 
         function(i) fit_tr_mediator_model(df = merged_df,
                                           model_params_row = tr_mediator_grid[i, ])) %>% 
  bind_rows() %>% 
  rename(tr = x, mediator = y) %>% 
  mutate(sig_effect = ifelse(pval < 0.05, T, F))

saveRDS(tr_mediator_effects, paste0(tr_med_res_directory, "tr_mediator_effects.RDS"))

# Adjusted treatment-mediator effects
tr_mediator_effects_adjusted = 
  lapply(1:nrow(tr_mediator_grid), 
         function(i) fit_tr_mediator_model(df = merged_df,
                                           model_params_row = tr_mediator_grid[i, ],
                                           adjusted = TRUE)) %>% 
  bind_rows() %>% 
  rename(tr = x, mediator = y) %>% 
  mutate(sig_effect = ifelse(pval < 0.05, T, F))

saveRDS(tr_mediator_effects_adjusted, paste0(tr_med_res_directory, "tr_mediator_effects_adjusted.RDS"))


