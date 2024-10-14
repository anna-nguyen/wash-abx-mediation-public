rm(list = ls())
source(here::here("0-config.R"))

mediator_subset = c("diar7d", "ari7d", "ari_fever7d", "fever7d", "pos_virus", "symp_virus", "any_infection_14mo")

mediated_effects = readRDS(paste0(mediated_effects_results_directory, "mediated_effects.RDS")) %>% 
  select(tr, mediator, outcome, est = ACME_treated, ci_lb = ACME_treated_ci_lb, ci_ub = ACME_treated_ci_ub) %>% 
  label_mediators(include_time_labels = F) %>% 
  filter(tr == "tr_pooled", 
         outcome == "ablastmo",
         mediator %in% mediator_subset) %>% 
  mutate(type = "continuous", 
         est = est * 100, 
         ci_lb = ci_lb * 100, 
         ci_ub = ci_ub * 100)

mediation_plt = 
  plot_effects(df = mediated_effects, 
               y_var = "mediator_label", 
               y_type = "continuous",  
               color_var = "mediator_group", 
               include_facets = FALSE, 
               plt_title = "") + 
  theme(legend.position = "none")  + 
  ggtitle("") + 
  geom_hline(yintercept = 8.5, linewidth = 8, color = "white") + 
  geom_hline(yintercept = 7.5, linewidth = 8, color = "white") + 
  geom_hline(yintercept = 6.5, linewidth = 8, color = "white") + 
  geom_hline(yintercept = 5.5, linewidth = 8, color = "white") +
  geom_hline(yintercept = 2.5, linewidth = 8, color = "white")

mediation_plt

ggsave(here("figures", "Figure2_mediated_effects.png"),
       mediation_plt,
       height = 3, width = 5, bg = "white")

