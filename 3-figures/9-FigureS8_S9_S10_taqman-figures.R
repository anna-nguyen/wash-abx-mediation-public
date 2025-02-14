rm(list = ls())
source(here::here("0-config.R"))
set.seed(0)

taqman_results_filepath = here::here("results", "full_taqman_panel/")


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

#################################################
# Treatment-mediator plot
#################################################
tr_mediator_effects = readRDS(paste0(taqman_results_filepath, "taqman_tr_med_effects.RDS")) %>% 
  label_treatments() %>% 
  format_taqman()

# Pooled treatment, absolute scale ----
tr_mediator_effects_plt = 
  plot_effects(df = tr_mediator_effects, 
               y_var = "mediator_label", 
               y_type = "cat", 
               color_var = "pathogen_color",
               plt_title = "") + 
  facet_wrap(as.formula(glue("~ pathogen_group")), scales = "free") + 
  theme(legend.position = "none")
  
ggsave(here::here("figures", "taqman", "taqman_tr_mediator_effects_plt.png"), tr_mediator_effects_plt,
       height = 3.5, width = 10)

#################################################
# Mediator-outcome plot
#################################################

mediator_outcome_effects = readRDS(paste0(taqman_results_filepath, "taqman_med_out_effects.RDS")) %>% 
  label_outcomes() %>% 
  format_taqman()

# Pooled treatment, absolute scale ----
plot_taqman_mediator_outcomes = function(i) {
  outcome_string = outcome_list[i]
  filtered_df = mediator_outcome_effects %>% filter(outcome == outcome_string)
  outcome_label_string = filtered_df %>% pull(outcome_label) %>% unique() %>% str_replace("\n", " ")
  
  type_str = ifelse(outcome_string %in% continuous_outcomes, "cont", "cat")
  plot_effects(df = filtered_df %>% mutate(type = type_str),
               y_var = "mediator_label",
               y_type = type_str,
               color_var = "pathogen_color",
               plt_title = "") + 
    facet_wrap(as.formula(glue("~ pathogen_group")), scales = "free") + 
    theme(legend.position = "none") + 
    ggtitle(paste0(LETTERS[i], ") ", outcome_label_string))
}

med_out_plots = lapply(1:length(outcome_list), plot_taqman_mediator_outcomes)

mediator_outcome_effects_plt = 
  grid.arrange(med_out_plots[[1]], med_out_plots[[2]], med_out_plots[[3]],
               med_out_plots[[4]], med_out_plots[[5]],
               nrow = 3)

ggsave(here::here("figures", "taqman", "taqman_mediator_outcome_effects_plt.png"), mediator_outcome_effects_plt,
       height = 10, width = 18)

#################################################
# Mediated effects plot
#################################################

mediated_effects = readRDS(paste0(taqman_results_filepath, "taqman_mediated_effects.RDS")) %>% 
  label_outcomes() %>% 
  format_taqman() 

plt_df_mediated_effects = mediated_effects %>% 
  select(tr, pathogen_group, pathogen_color, mediator_label, outcome, outcome_label, est = ACME_treated, ci_lb = ACME_treated_ci_lb, ci_ub = ACME_treated_ci_ub) 

# Pooled treatment, absolute scale ----
plot_taqman_mediated_effects = function(i) {
  outcome_string = outcome_list[i]
  filtered_df = plt_df_mediated_effects %>% filter(outcome == outcome_string)
  outcome_label_string = filtered_df %>% pull(outcome_label) %>% unique() %>% str_replace("\n", " ")
  
  ggplot(filtered_df,
         aes(x = est, y = mediator_label, color = pathogen_color)) + 
    geom_vline(xintercept = 0, alpha = 0.8, color = "darkgray", linetype = "dashed") + 
    geom_point(position = position_dodge(width = 0.7)) + 
    geom_linerange(aes(xmin = ci_lb, xmax = ci_ub), position = position_dodge(width = 0.7)) + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    scale_y_discrete(limits=rev)+ 
    theme_minimal() + 
    xlab("Prevalence Difference (95% CI)") +
    ylab("") +
    facet_wrap(~ pathogen_group, scales = "free") + 
    scale_color_manual(drop = FALSE, 
                       values = c("#653239", "#D0B4AD") , 
                       limits = c("agg", "individual")) + 
    theme(legend.position = "none",
          legend.title=element_blank(),
          plot.title.position = "plot",
          panel.spacing = unit(1, "lines")) + 
    ggtitle(paste0(LETTERS[i], ") ", outcome_label_string))
}

mediation_plots = lapply(1:length(outcome_list), plot_taqman_mediated_effects)

mediated_effects_plt = 
  grid.arrange(mediation_plots[[1]], mediation_plots[[2]], mediation_plots[[3]],
               mediation_plots[[4]], mediation_plots[[5]],
               nrow = 3)

ggsave(here::here("figures", "taqman", "taqman_mediated_effects_plt.png"), mediated_effects_plt,
       height = 10, width = 18)
