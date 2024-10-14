rm(list = ls())
source(here::here("0-config.R"))
set.seed(0)

# Load data
merged_df = readRDS(merged_df_filepath)

# Load mediator-outcome effects, filter to those with a significant effect
mediator_outcome_effects = readRDS(paste0(med_out_res_directory, "mediator_outcome_effects.RDS"))

filtered_mediator_outcomes = mediator_outcome_effects %>% 
  filter(sig_effect) %>% 
  select(mediator, outcome)

# Create grid of all treatment + mediator + outcome combinations, for significant mediator-outcome effects
tr_med_out_grid = 
  lapply(tr_list, function(tr_str) filtered_mediator_outcomes %>% mutate(tr = tr_str)) %>% 
  bind_rows() 

tr_med_out_grid_rr = tr_med_out_grid %>% filter(!(outcome %in% continuous_outcomes))

# Mediated effects, no interaction, absolute scale
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
saveRDS(mediated_effects, paste0(mediated_effects_results_directory, "mediated_effects.RDS"))

# Mediated effects, no interaction, relative scale
mediated_effects_RR = 
  lapply(1:nrow(tr_med_out_grid_rr),
         function(i) {
           print(i)
           fit_mediation_models(df = merged_df,
                                tr_string = tr_med_out_grid_rr[i, "tr"], 
                                med_string = tr_med_out_grid_rr[i, "mediator"], 
                                out_string = tr_med_out_grid_rr[i, "outcome"],
                                effect_scale = "relative")}) %>% 
  bind_rows()

rownames(mediated_effects_RR) = NULL
saveRDS(mediated_effects_RR, paste0(mediated_effects_results_directory, "mediated_effects_RR.RDS"))

# Mediated effects, interaction, absolute scale
mediated_effects_with_interaction = 
  lapply(1:nrow(tr_med_out_grid),
         function(i) {
           print(i)
           fit_mediation_models(df = merged_df,
                                tr_string = tr_med_out_grid[i, "tr"], 
                                med_string = tr_med_out_grid[i, "mediator"], 
                                out_string = tr_med_out_grid[i, "outcome"],
                                include_tr_med_interaction = TRUE,
                                effect_scale = "absolute")}) %>% 
  bind_rows()

rownames(mediated_effects_with_interaction) = NULL
saveRDS(mediated_effects_with_interaction, paste0(mediated_effects_results_directory, "mediated_effects_with_interaction.RDS"))

# Mediated effects, interaction, relative scale
mediated_effects_with_interaction_RR = 
  lapply(1:nrow(tr_med_out_grid_rr),
         function(i) {
           print(i)
           fit_mediation_models(df = merged_df,
                                tr_string = tr_med_out_grid_rr[i, "tr"], 
                                med_string = tr_med_out_grid_rr[i, "mediator"], 
                                out_string = tr_med_out_grid_rr[i, "outcome"],
                                include_tr_med_interaction = TRUE,
                                effect_scale = "relative")}) %>% 
  bind_rows()

rownames(mediated_effects_with_interaction_RR) = NULL
saveRDS(mediated_effects_with_interaction_RR, paste0(mediated_effects_results_directory, "mediated_effects_with_interaction_RR.RDS"))
