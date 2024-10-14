label_treatments = function(df) {
  df %>% 
    mutate(treatment_label = 
             case_when(tr == "tr_pooled" ~ "Pooled Intervention",
                       tr == "tr_n" ~ "Nutrition",
                       tr == "tr_n_pooled" ~ "Pooled Nutrition + NWSH",
                       tr == "tr_n_wsh" ~ "NWSH",
                       tr == "tr_wsh" ~ "WSH",
                       tr == "tr_wsh_pooled" ~ "Pooled WSH + NWSH"),
           treatment_label = factor(treatment_label, 
                                    levels = c("Pooled Intervention", "Pooled Nutrition + NWSH", "Pooled WSH + NWSH", "Nutrition", "WSH", "NWSH")))
}

filter_mediators = function(df) {
  df %>% 
    filter(mediator %in% c("diar7d", "ari7d", "ari_fever7d", "fever7d", "n_virus", "pos_virus", "symp_virus", "any_infection_14mo"))
}

label_mediators = function(df, include_time_labels = T) {
  if (include_time_labels) {
    labeled_df = df %>% 
      mutate(mediator_label = 
               case_when(mediator == "bruise7d" ~ "Bruising (Past 7 Days)",
                         mediator == "diar7d" ~ "Diarrhea (Past 7 Days)",
                         mediator == "diffbreathing7d" ~ "Difficulty Breathing (Past 7 Days)",
                         mediator == "ari7d" ~ "ARI (Past 7 Days)", 
                         mediator == "ari_fever7d" ~ "ARI with Fever (Past 7 Days)", 
                         mediator == "fever7d" ~ "Fever (Past 7 Days)", 
                         mediator == "fever14d" ~ "Fever (Past 14 Days)",
                         mediator == "pos_Adenovirus40_41" ~ "Adenovirus 40/41", 
                         mediator == "pos_Norovirus_GII" ~ "Norovirus GII", 
                         mediator == "pos_Sapovirus" ~ "Sapovirus", 
                         mediator == "n_virus" ~ "Number of Enteric Viruses", 
                         mediator == "pos_virus" ~ "Any Enteric Viruses",
                         mediator == "symp_virus" ~ "Any Enteric Virus with Diarrhea",
                         mediator == "n_virus" ~ "Number of Enteric Viruses", 
                         mediator == "Total Effect" ~ "Total Effect",
                         mediator == "Total Effect (from glm)" ~ "Total Effect (from glm)",
                         mediator == "Total Effect (from mediation)" ~ "Total Effect (from mediation)", 
                         mediator == "pos_virus_symp" ~ "Any Enteric Viruses (Symptomatic)", 
                         mediator == "n_virus_symp" ~ "Number of Enteric Viruses (Symptomatic)",
                         mediator == "pos_Adenovirus40_41_symp" ~ "Adenovirus 40/41 (Symptomatic)", 
                         mediator == "pos_Norovirus_GII_symp" ~ "Norovirus GII (Symptomatic)", 
                         mediator == "pos_Sapovirus_symp" ~ "Sapovirus (Symptomatic)",
                         mediator == "any_infection_14mo" ~ "Diarrhea, Fever, ARI, or Enteric Virus",
                         mediator == "any_infection_14_28mo" ~ "Diarrhea, Fever, or ARI"),
             mediator_label = factor(mediator_label,
                                     levels = c("Total Effect", 
                                                "Total Effect (from glm)", 
                                                "Total Effect (from mediation)", 
                                                "Diarrhea, Fever, ARI, or Enteric Virus",
                                                "Diarrhea, Fever, or ARI",
                                                "Bruising (Past 7 Days)",
                                                "Diarrhea (Past 7 Days)", 
                                                "ARI (Past 7 Days)", 
                                                "ARI with Fever (Past 7 Days)", 
                                                "Difficulty Breathing (Past 7 Days)", 
                                                "Fever (Past 7 Days)",
                                                "Fever (Past 14 Days)",
                                                "Any Enteric Viruses",
                                                "Any Enteric Virus with Diarrhea",
                                                "Number of Enteric Viruses",
                                                "Any Enteric Viruses (Symptomatic)",
                                                "Number of Enteric Viruses (Symptomatic)",
                                                "Adenovirus 40/41",
                                                "Adenovirus 40/41 (Symptomatic)",
                                                "Norovirus GII",
                                                "Norovirus GII (Symptomatic)", 
                                                "Sapovirus",
                                                "Sapovirus (Symptomatic)")))
  } else {
    labeled_df = df %>% 
      mutate(mediator_label = 
               case_when(mediator == "bruise7d" ~ "Bruising",
                         mediator == "diar7d" ~ "Diarrhea",
                         mediator == "diffbreathing7d" ~ "Difficulty Breathing",
                         mediator == "ari7d" ~ "ARI", 
                         mediator == "ari_fever7d" ~ "ARI with Fever", 
                         mediator == "fever7d" ~ "Fever", 
                         mediator == "fever14d" ~ "Fever (Past 14 Days)",
                         mediator == "pos_Adenovirus40_41" ~ "Adenovirus 40/41", 
                         mediator == "pos_Norovirus_GII" ~ "Norovirus GII", 
                         mediator == "pos_Sapovirus" ~ "Sapovirus", 
                         mediator == "n_virus" ~ "Number of Enteric Viruses", 
                         mediator == "pos_virus" ~ "Any Enteric Viruses", 
                         mediator == "symp_virus" ~ "Any Enteric Virus with Diarrhea",
                         mediator == "pos_virus_symp" ~ "Any Enteric Viruses (Symptomatic)", 
                         mediator == "n_virus_symp" ~ "Number of Enteric Viruses (Symptomatic)",
                         mediator == "pos_Adenovirus40_41_symp" ~ "Adenovirus 40/41 (Symptomatic)", 
                         mediator == "pos_Norovirus_GII_symp" ~ "Norovirus GII (Symptomatic)", 
                         mediator == "pos_Sapovirus_symp" ~ "Sapovirus (Symptomatic)",
                         mediator == "any_infection_14mo" ~ "Diarrhea, Fever, ARI, or Enteric Virus",
                         mediator == "any_infection_14_28mo" ~ "Diarrhea, Fever, or ARI"),
             mediator_label = factor(mediator_label,
                                     levels = c("Diarrhea, Fever, ARI, or Enteric Virus",
                                                "Diarrhea, Fever, or ARI",
                                                "Bruising",
                                                "Diarrhea", 
                                                "ARI", 
                                                "ARI with Fever", 
                                                "Difficulty Breathing", 
                                                "Fever",
                                                "Fever (Past 14 Days)",
                                                "Any Enteric Viruses",
                                                "Any Enteric Virus with Diarrhea",
                                                "Number of Enteric Viruses",
                                                "Any Enteric Viruses (Symptomatic)",
                                                "Number of Enteric Viruses (Symptomatic)",
                                                "Adenovirus 40/41",
                                                "Adenovirus 40/41 (Symptomatic)",
                                                "Norovirus GII",
                                                "Norovirus GII (Symptomatic)", 
                                                "Sapovirus",
                                                "Sapovirus (Symptomatic)")))
  }
  
  labeled_df = labeled_df %>% left_join(mediator_groups, by = "mediator")
  return(labeled_df)
}

label_outcomes = function(df, include_time_labels = T) {
  if (include_time_labels) {
    outcome_df = df %>% 
      mutate(outcome_label = 
               case_when(outcome == "ablastmo" ~ "Any Antibiotics\n(Past Month)", 
                         outcome == "abany" ~ "Any Antibiotics\n(Past 3 Months)", 
                         outcome == "abdays" ~ "Days of Antibiotic Use\n(Past 3 Months)", 
                         outcome == "abtimes" ~ "Episodes of Antibiotic Use\n(Past 3 Months)", 
                         outcome == "abmult" ~ "Multiple Episodes of Antibiotic Use\n(Past 3 Months)"),
             outcome_label = factor(outcome_label, 
                                    levels = c("Any Antibiotics\n(Past Month)",
                                               "Any Antibiotics\n(Past 3 Months)", 
                                               "Days of Antibiotic Use\n(Past 3 Months)", 
                                               "Episodes of Antibiotic Use\n(Past 3 Months)", 
                                               "Multiple Episodes of Antibiotic Use\n(Past 3 Months)")))
  } else {
    outcome_df = df %>% 
      mutate(outcome_label = 
               case_when(outcome == "ablastmo" ~ "Any Antibiotics", 
                         outcome == "abany" ~ "Any Antibiotics", 
                         outcome == "abdays" ~ "Days of Antibiotic Use", 
                         outcome == "abtimes" ~ "Episodes of Antibiotic Use", 
                         outcome == "abmult" ~ "Multiple Episodes of Antibiotic Use"),
             outcome_label = factor(outcome_label, 
                                    levels = c("Any Antibiotics",
                                               "Days of Antibiotic Use", 
                                               "Episodes of Antibiotic Use", 
                                               "Multiple Episodes of Antibiotic Use")))
  }
  
  outcome_df = outcome_df %>% left_join(outcome_groups, by = "outcome")
  
  return(outcome_df)
}

label_sig_effects = function(df) {
  df %>% 
    mutate(sig_effect_label = ifelse(sig_effect, "Significant Effect", "Null Effect"),
           sig_effect_label = factor(sig_effect_label, levels = c("Significant Effect", "Null Effect")))
}

label_covariates = function(df) {
  df %>% mutate(covars = covars %>%
                  str_replace("timeptsub", "Measurement date") %>%
                  str_replace("agem", "Child's age") %>%
                  str_replace("sex", "Child's sex") %>%
                  str_replace("birthord", "Birth order") %>%
                  str_replace("momage", "Mother's age") %>% 
                  str_replace("momheight", "Mother's height") %>%
                  str_replace("momedu", "Mother's education") %>%
                  str_replace("hfiacatrev", "Household food insecurity") %>%
                  str_replace("Nlt18", "Number of individuals in household under age 18") %>%
                  str_replace("Ncomp", "Number of individuals in compound") %>%
                  str_replace("watmin", "Distance to water source") %>%
                  str_replace("roof", "Household Roof Material") %>%
                  str_replace("walls", "Household Wall Material") %>%
                  str_replace("floor", "Household Floor Material") %>%
                  str_replace("HHwealth_quart", "Household Wealth Index"))
}
