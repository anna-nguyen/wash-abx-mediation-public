rm(list = ls())
source(here::here("0-config.R"))

mediator_subset = c("diar7d", "ari7d", "ari_fever7d", "fever7d", "pos_virus", "symp_virus", "any_infection_14mo")

# Load treatment-mediator effect estimates ----
tr_mediator_effects = readRDS(paste0(tr_med_res_directory, "tr_mediator_effects.RDS")) %>% 
  label_mediators(include_time_labels = F) %>% 
  filter(tr == "tr_pooled",
         mediator %in% mediator_subset) %>% 
  mutate(type = "categorical")

# Load treatment-mediator effect estimates ----
mediator_out_effects = readRDS(paste0(med_out_res_directory, "mediator_outcome_effects.RDS")) %>% 
  label_mediators(include_time_labels = F) %>% 
  filter(outcome == "ablastmo", 
         mediator %in% mediator_subset) %>% 
  mutate(type = "categorical")

tr_med_plt = 
  plot_effects(df = tr_mediator_effects, 
               y_var = "mediator_label", 
               y_type = "categorical",  
               color_var = "mediator_group", 
               include_facets = FALSE, 
               plt_title = "A) Intervention-Mediator Effects") +
  theme(legend.position = "none")  + 
  geom_hline(yintercept = 7.5, linewidth = 8, color = "white") + 
  geom_hline(yintercept = 6.5, linewidth = 8, color = "white") +
  geom_hline(yintercept = 5.5, linewidth = 8, color = "white") +
  geom_hline(yintercept = 2.5, linewidth = 8, color = "white")

med_out_plt = 
  plot_effects(df = mediator_out_effects, 
               y_var = "mediator_label", 
               y_type = "categorical",  
               color_var = "mediator_group", 
               include_facets = FALSE, 
               plt_title = "B) Mediator-Outcome Effects") +
  theme(legend.position = "none")  + 
  geom_hline(yintercept = 7.5, linewidth = 8, color = "white") + 
  geom_hline(yintercept = 6.5, linewidth = 8, color = "white") +
  geom_hline(yintercept = 5.5, linewidth = 8, color = "white") +
  geom_hline(yintercept = 2.5, linewidth = 8, color = "white")

combined_plt = grid.arrange(tr_med_plt, med_out_plt, nrow = 1)

ggsave(here("figures", "Figure1_tr-med_med-out_effects.png"),
       combined_plt,
       height = 3.5, width = 11, bg = "white")
