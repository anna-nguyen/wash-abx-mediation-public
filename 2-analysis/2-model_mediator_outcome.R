rm(list = ls())
source(here::here("0-config.R"))
set.seed(0)

# Load data
merged_df = readRDS(merged_df_filepath)

# Create grid of all mediator + outcome combinations
mediator_outcome_grid = 
  lapply(outcome_list, 
         function(out_name) expand.grid(mediator_outcome_mappings[[out_name]], out_name)) %>% 
    bind_rows() %>% 
    rename(x = Var1, y = Var2) %>%
    mutate(x = as.character(x), 
           y = as.character(y), 
           formula_string = glue("{y} ~ {x} + agem"), 
           y_type = ifelse(y %in% continuous_outcomes, "continuous", "categorical"))

# Mediator-outcome effects
mediator_outcome_effects = 
  lapply(1:nrow(mediator_outcome_grid), 
         function(i) fit_mediator_outcome_model(df = merged_df,
                                                model_params_row = mediator_outcome_grid[i, ],
                                                adjusted = TRUE)) %>% 
  bind_rows() %>% 
  rename(mediator = x, outcome  = y) %>% 
  mutate(sig_effect = ifelse(pval < 0.05, T, F))

saveRDS(mediator_outcome_effects, paste0(med_out_res_directory, "mediator_outcome_effects.RDS"))
