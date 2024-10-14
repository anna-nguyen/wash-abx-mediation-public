# Load required packages
#renv::restore()
library(here)
library(washb)
library(mediation)
library(lubridate)
library(tidyverse)
library(glue)
library(lmtest) 
library(gridExtra)
library(haven)
library(grid)

#--------------------------------------------
# File paths
#--------------------------------------------
if(Sys.getenv("LOGNAME")=="annanguyen"){
  box_path = "/Users/annanguyen/Box Sync/"
}

if(Sys.getenv("LOGNAME")=="gabriellabh"){
  box_path = "/Users/gabriellabh/Library/CloudStorage/Box-Box/"
}

box_wash_data_folder = paste0(box_path, "WBB abx mediation/washb_data/")
box_raw_data_folder = paste0(box_path, "WBB abx mediation/raw_data/")
box_clean_data_folder = paste0(box_path, "WBB abx mediation/clean_data/")
table_folder = paste0(box_path, "WBB abx mediation/tables/")

cleaned_wash_filepath = paste0(box_wash_data_folder, "washb-bangladesh-diar.csv")
ab_df_filepath = paste0(box_raw_data_folder, "ab-wbb.csv")
midline_child_df_filepath = paste0(box_raw_data_folder, "ChildID_Midline_Cleaned_MatchedwEnrollment_2Feb16.csv")
endline_child_df_filepath = paste0(box_raw_data_folder, "WASHB_Endline_childhealthinfo_cleaned.dta")

path_df_filepath = paste0(box_raw_data_folder, "wbb_mapping_bangladesh_pathogen_data.RDS")

merged_df_filepath = paste0(box_clean_data_folder, "wbb_merged_abx_mediation_df.RDS")

#--------------------------------------------
# Results paths
#--------------------------------------------
# Set treatment-outcome results directory, create if doesn't exist
tr_out_res_directory = here::here("results", "tr_outcome_effects/")

if (!(dir.exists(tr_out_res_directory))) {
  dir.create(tr_out_res_directory)
}

# Set results directory, create if doesn't exist
tr_med_res_directory = here::here("results", "tr_mediator_effects/")

if (!(dir.exists(tr_med_res_directory))) {
  dir.create(tr_med_res_directory)
}

# Set results directory, create if doesn't exist
med_out_res_directory = here::here("results", "mediator_outcome_effects/")

if (!(dir.exists(med_out_res_directory))) {
  dir.create(med_out_res_directory)
}

# Set mediated effects results directory, create if doesn't exist
mediated_effects_results_directory = here::here("results", "mediated_effects/")

if (!(dir.exists(mediated_effects_results_directory))) {
  dir.create(mediated_effects_results_directory)
}

#--------------------------------------------
# List variables of interest
#--------------------------------------------
tr_list = c("tr_pooled", "tr_wsh", "tr_n", "tr_n_wsh", "tr_wsh_pooled", "tr_n_pooled")

covariate_list = c("timeptsub", "sex", "birthord", 
                   "momage", "momheight", "momedu", "hfiacatrev", "Nlt18", "Ncomp", 
                   "watmin", "roof", "walls", "floor", "HHwealth_quart")
categorical_covariates = c("sex", "birthord", "momedu", "hfiacatrev", "roof", "walls", "floor", "HHwealth_quart")

mediator_list = c("diar7d", "diffbreathing7d", "ari7d", "ari_fever7d", "fever7d", "fever14d",
                  "any_infection_14mo", "any_infection_14_28mo",
                  "pos_Adenovirus40_41_symp", "pos_Norovirus_GII_symp", "pos_Sapovirus_symp", 
                  "pos_Adenovirus40_41", "pos_Norovirus_GII", "pos_Sapovirus", 
                  "n_virus", "pos_virus", "symp_virus", "n_virus_symp", "pos_virus_symp")
continuous_mediators = c("n_virus", "n_virus_symp")
symp_mediators = c("pos_virus_symp", "n_virus_symp", "pos_Adenovirus40_41_symp", "pos_Norovirus_GII_symp", "pos_Sapovirus_symp")

outcome_list = c("ablastmo", "abany", "abdays", "abtimes", "abmult")
continuous_outcomes = c("abdays", "abtimes")

outcome_group_mappings = 
  list("Past 1 Month" = c("ablastmo"),
       "Past 3 Months" = c("abany", "abdays", "abtimes", "abmult"))

outcome_groups = data.frame(outcome = character(), outcome_group = character())

for (group_name in names(outcome_group_mappings)) {
  outcome_groups = outcome_groups %>% 
    bind_rows(data.frame(outcome = outcome_group_mappings[[group_name]],
                         outcome_group = group_name))
}

mediator_outcome_mappings = 
  list("ablastmo" = c("diar7d", "diffbreathing7d", "ari7d", "ari_fever7d", "fever7d", "fever14d",
                      "pos_Adenovirus40_41", "pos_Norovirus_GII", "pos_Sapovirus", 
                      "pos_Adenovirus40_41_symp", "pos_Norovirus_GII_symp", "pos_Sapovirus_symp", 
                      "n_virus", "pos_virus", "symp_virus", "n_virus_symp", "pos_virus_symp",
                      "any_infection_14mo", "any_infection_14_28mo"),  
       "abany" = c("pos_Adenovirus40_41", "pos_Norovirus_GII", "pos_Sapovirus", 
                   "pos_Adenovirus40_41_symp", "pos_Norovirus_GII_symp", "pos_Sapovirus_symp", 
                   "n_virus", "pos_virus", "symp_virus", "n_virus_symp", "pos_virus_symp"), 
       "abdays" = c("pos_Adenovirus40_41", "pos_Norovirus_GII", "pos_Sapovirus", 
                    "pos_Adenovirus40_41_symp", "pos_Norovirus_GII_symp", "pos_Sapovirus_symp", 
                    "n_virus", "pos_virus", "symp_virus", "n_virus_symp", "pos_virus_symp"), 
       "abtimes" = c("pos_Adenovirus40_41", "pos_Norovirus_GII", "pos_Sapovirus", 
                     "pos_Adenovirus40_41_symp", "pos_Norovirus_GII_symp", "pos_Sapovirus_symp", 
                     "n_virus", "pos_virus", "symp_virus", "n_virus_symp", "pos_virus_symp"), 
       "abmult" = c("pos_Adenovirus40_41", "pos_Norovirus_GII", "pos_Sapovirus", 
                    "pos_Adenovirus40_41_symp", "pos_Norovirus_GII_symp", "pos_Sapovirus_symp", 
                    "n_virus", "pos_virus", "symp_virus", "n_virus_symp", "pos_virus_symp"))
  
mediator_group_mappings = 
  list("diarrhea" = c("diar7d"),
       "ari" = c("diffbreathing7d", "ari7d", "ari_fever7d", "fever7d", "fever14d"),
       "virus" = c("pos_Adenovirus40_41", "pos_Norovirus_GII", "pos_Sapovirus",
                   "pos_Adenovirus40_41_symp", "pos_Norovirus_GII_symp", "pos_Sapovirus_symp",
                   "n_virus", "pos_virus", "symp_virus", "n_virus_symp", "pos_virus_symp"),
       "any_infection" = c("any_infection_14mo", "any_infection_14_28mo"))

mediator_groups = data.frame(mediator = character(), mediator_group = character())
             
for (group_name in names(mediator_group_mappings)) {
  mediator_groups = mediator_groups %>% 
    bind_rows(data.frame(mediator = mediator_group_mappings[[group_name]],
                         mediator_group = group_name))
}
#--------------------------------------------
# Load utility functions
#--------------------------------------------
util_functions = list.files(paste0(here::here(), "/0-utils/"), pattern = "*.R")
for (util in util_functions) {
  source(paste0(here::here(), "/0-utils/", util))
} 
  
  
  
