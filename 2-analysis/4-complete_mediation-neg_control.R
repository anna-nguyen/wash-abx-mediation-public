rm(list = ls())
source(here::here("0-config.R"))
set.seed(0)

merged_df = readRDS(merged_df_filepath)

filtered_mediator_outcomes = 
  expand.grid("bruise7d", outcome_list) %>% 
  as.data.frame() %>% 
  rename(mediator = Var1, outcome = Var2) %>% 
  mutate(mediator = as.character(mediator), 
         outcome = as.character(outcome))

tr_med_out_grid = 
  lapply(tr_list, function(tr_str) filtered_mediator_outcomes %>% mutate(tr = tr_str)) %>% 
  bind_rows()

mediated_effects = 
  lapply(1:nrow(tr_med_out_grid),
         function(i) {
           print(i)
           fit_mediation_models(df = merged_df,
                                tr_string = tr_med_out_grid[i, "tr"], 
                                med_string = tr_med_out_grid[i, "mediator"], 
                                out_string = tr_med_out_grid[i, "outcome"],
                                effect_scale = "absolute")}) %>% 
  bind_rows()

rownames(mediated_effects) = NULL
saveRDS(mediated_effects, paste0(mediated_effects_results_directory, "negcontrol_mediated_effects.RDS"))
