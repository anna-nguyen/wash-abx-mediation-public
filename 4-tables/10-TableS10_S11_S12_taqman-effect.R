rm(list = ls())
source(here::here("0-config.R"))
set.seed(0)
merged_df = readRDS(merged_df_filepath)

taqman_results_filepath = here::here("results", "full_taqman_panel/")
tr_mediator_effects = readRDS(paste0(taqman_results_filepath, "taqman_tr_med_effects.RDS"))

mediator_list = tr_mediator_effects$mediator %>% unique()
tr_list = tr_mediator_effects$tr %>% unique()

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


top_factors = c("Any Virus", "Number of Viruses", "Any Bacteria", "Number of Bacteria", "Any Parasite", "Number of Parasites")
format_taqman = function(df) {
  df = df %>% 
    filter(!str_detect(mediator, "_symp"), mediator != "pos_bact",  !str_detect(mediator, "n_")) %>% 
    mutate(pathogen_group = case_when(
      mediator %in% bacteria_list ~ "Bacteria",
      mediator %in% virus_list ~ "Virus", 
      mediator %in% parasite_list ~ "Parasite"), 
      mediator_label = mediator %>% str_remove("pos_") %>% str_replace(".any", " (Any)") %>% str_replace("_", " "),
      mediator_label = case_when(mediator_label == "Shigella EIEC" ~ "Shigella/EIEC",
                                 mediator_label == "Adenovirus40 41" ~ "Adenovirus 40/41",
                                 mediator_label == "n virus" ~ "Number of Viruses",
                                 mediator_label == "virus" ~ "Any Virus",
                                 mediator_label == "n bact" ~ "Number of Bacteria",
                                 mediator_label == "bact" ~ "Any Bacteria",
                                 mediator_label == "n parasite" ~ "Number of Parasites",
                                 mediator_label == "parasite" ~ "Any Parasite",
                                 T ~ mediator_label), 
      type = ifelse(!(mediator %in% c("n_virus", "n_bact", "n_parasite")), "cat", "cont"), 
      pathogen_color = ifelse(mediator_label %in% top_factors, "agg", "individual"))  
  
  df$mediator_label <- 
    factor(df$mediator_label, 
           levels = c(top_factors, sort(setdiff(unique(df$mediator_label), top_factors))))
  
  return(df)
}

############################################
# Treatment-mediator table
############################################

format_tr_med_table = function(tr_str, med_str) {
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
      left_join(tr_mediator_effects, by = c("tr", "mediator")) 
  )
}

tr_mediator_effects = readRDS(paste0(taqman_results_filepath, "taqman_tr_med_effects.RDS")) %>% 
  mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>% 
  mutate(effect_est = glue("{est} ({ci_lb}, {ci_ub})"),
         mean_diff = ifelse(mediator %in% continuous_mediators, effect_est, "----"),
         prev_ratio = ifelse(mediator %in% continuous_mediators, "----", effect_est)) %>% 
  select(tr, mediator, prev_ratio)

tr_med_grid = expand_grid(tr = tr_list, med = mediator_list)

taqman_tr_med_tbl = lapply(1:nrow(tr_med_grid), 
                  function(i) format_tr_med_table(tr_med_grid$tr[i], tr_med_grid$med[i])) %>% 
  bind_rows() %>% 
  label_treatments() %>% 
  format_taqman() %>% 
  arrange(pathogen_group, mediator_label) %>% 
  select(pathogen_group, mediator_label, everything(), -tr, -mediator) 

write_csv(taqman_tr_med_tbl, here::here("tables", "TableS10-taqman-tr-med-table.csv"))

############################################
# Mediator-outcome table
############################################

format_med_out_table = function(med_str, out_str) {
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

med_out_effects = readRDS(paste0(taqman_results_filepath, "taqman_med_out_effects.RDS"))%>% 
  mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>% 
  mutate(effect_est = glue("{est} ({ci_lb}, {ci_ub})"),
         mean_diff = ifelse(outcome %in% continuous_outcomes, effect_est, "----"),
         prev_ratio = ifelse(outcome %in% continuous_outcomes, "----", effect_est)) %>% 
  select(mediator, outcome, mean_diff, prev_ratio)

med_out_grid = expand_grid(med = mediator_list, out = outcome_list)

med_out_table = lapply(1:nrow(med_out_grid), function(i) format_med_out_table(med_out_grid$med[i], med_out_grid$out[i])) %>% 
  bind_rows() %>% 
  format_taqman() %>% 
  label_outcomes() %>% 
  arrange(outcome_label, pathogen_group, mediator_label) %>% 
  filter(!is.na(mean_diff) | !is.na(prev_ratio)) %>% 
  select(outcome_label, mediator_label, everything(), -mediator, -outcome)

write_csv(med_out_table, here::here("tables", "TableS11-taqman-med-out-table.csv"))

############################################
# Mediation table
############################################

mediated_effects = readRDS(paste0(taqman_results_filepath, "taqman_mediated_effects.RDS")) %>% 
  mutate_if(is.numeric, ~format(round(., 3), nsmall = 3)) %>% 
  mutate(prev_diff = glue("{ACME_treated} ({ACME_treated_ci_lb}, {ACME_treated_ci_ub})")) %>% 
  label_treatments() %>% 
  format_taqman() %>% 
  label_outcomes() %>% 
  arrange(outcome_label, pathogen_group, mediator_label) %>% 
  select(pathogen_group, outcome_label, mediator_label, prev_diff)

write_csv(mediated_effects, here::here("tables", "TableS12-taqman-mediated-effects-table.csv"))


