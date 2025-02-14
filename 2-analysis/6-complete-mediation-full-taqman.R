rm(list = ls())
source(here::here("0-config.R"))
set.seed(0)

taqman_results_filepath = here::here("results", "full_taqman_panel/")

# Load data
merged_df = readRDS(merged_df_filepath)

virus_list = c("Norovirus GI", "Norovirus GII", "Astrovirus", "Adenovirus40_41", "Rotavirus", "Sapovirus")
virus_list = paste0("pos_", virus_list) %>% str_replace(" ", "_")
virus_list = c(virus_list, "pos_Adenovirus40_41_symp", "pos_Astrovirus_symp", "pos_Norovirus_GII_symp", "pos_Rotavirus_symp", "pos_Sapovirus_symp",
               "n_virus", "pos_virus")

bacteria_list = c("EAEC", "ETEC.any", "EPEC.any", "STEC", "Shigella_EIEC", "Salmonella", "B.fragilis", "H.pylori", "V.cholerae", "C.difficile", "Plesiomonas", "Campylobacter", "Aeromonas")
bacteria_list = paste0("pos_", bacteria_list) %>% str_replace(" ", "_")
bacteria_list = c(bacteria_list, "pos_Campylobacter_symp", "pos_Shigella_EIEC_symp", "pos_ETEC.any_symp", "pos_EPEC.any_symp",
               "n_bact", "pos_bact")

parasite_list = c("Ancyclostoma", "pan Entamoeba", "Giardia", "Cryptosporidium", "Ascaris", "Trichuris",  "Schistosoma", "Cyclospora", "Isospora","Blastocystis", "E.bieneusi")
parasite_list = paste0("pos_", parasite_list) %>% str_replace(" ", "_")
parasite_list = c(parasite_list, "pos_Cryptosporidium_symp", "n_parasite", "pos_parasite")

taqman_path_list = c(virus_list, bacteria_list, parasite_list)

always_keep = c( "n_virus", "pos_virus", "n_bact", "pos_bact", "n_parasite", "pos_parasite")
pathogen_prev = merged_df %>% select(taqman_path_list) %>% colMeans(na.rm = T)
filtered_pathogen_list = names(pathogen_prev[pathogen_prev > 0.05])
filtered_pathogen_list = filtered_pathogen_list[!(filtered_pathogen_list %in% always_keep)]
filtered_pathogen_list = c(filtered_pathogen_list, always_keep)

# Create grid of all treatment + mediator combinations
tr_mediator_grid = expand_grid(tr_list, filtered_pathogen_list) %>%
  rename(x = tr_list, y = filtered_pathogen_list) %>% 
  mutate(formula_string = glue("{y} ~ {x} + agem"), 
         y_type = ifelse(y %in% always_keep & str_detect(y, "n_") , "continuous", "categorical")) %>% 
  filter(x == "tr_pooled")

# Unadjusted treatment-mediator effects
tr_mediator_effects = 
  lapply(1:nrow(tr_mediator_grid), 
         function(i) fit_tr_mediator_model(df = merged_df,
                                           model_params_row = tr_mediator_grid[i, ])) %>% 
  bind_rows() %>% 
  rename(tr = x, mediator = y) %>% 
  mutate(sig_effect = ifelse(pval < 0.05, T, F))

saveRDS(tr_mediator_effects, paste0(taqman_results_filepath, "taqman_tr_med_effects.RDS"))
        
# Create grid of all mediator + outcome combinations
mediator_outcome_grid = 
  lapply(outcome_list, 
         function(out_name) expand.grid(filtered_pathogen_list, out_name)) %>% 
  bind_rows() %>% 
  rename(x = Var1, y = Var2) %>%
  mutate(x = as.character(x), 
         y = as.character(y), 
         formula_string = glue("{y} ~ {x} + agem"), 
         y_type = ifelse(y %in% continuous_outcomes, "continuous", "categorical")) %>% 
  mutate(tr = "tr_pooled")

# Mediator-outcome effects
mediator_outcome_effects = 
  lapply(1:nrow(mediator_outcome_grid), 
         function(i) fit_mediator_outcome_model(df = merged_df,
                                                model_params_row = mediator_outcome_grid[i, ],
                                                adjusted = TRUE)) %>% 
  bind_rows() %>% 
  rename(mediator = x, outcome  = y) %>% 
  mutate(sig_effect = ifelse(pval < 0.05, T, F),
         sig_effect_bonferroni = ifelse(pval < (0.05 / nrow(.)), T, F))


saveRDS(mediator_outcome_effects, paste0(taqman_results_filepath, "taqman_med_out_effects.RDS"))

filtered_mediator_outcomes = mediator_outcome_effects %>% 
  #filter(sig_effect) %>% 
  select(mediator, outcome) 

# Create grid of all treatment + mediator + outcome combinations, for significant mediator-outcome effects
tr_med_out_grid = 
  filtered_mediator_outcomes %>% 
  mutate(tr = "tr_pooled")

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

saveRDS(mediated_effects, paste0(taqman_results_filepath, "taqman_mediated_effects.RDS"))