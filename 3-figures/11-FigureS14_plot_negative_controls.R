rm(list = ls())
source(here::here("0-config.R"))

negcontrol_mediated_effects = readRDS(paste0(mediated_effects_results_directory, "negcontrol_mediated_effects.RDS"))

#####################
# with no interactions
#####################

plt_df_mediated_effects = negcontrol_mediated_effects %>% 
  select(tr, mediator, outcome, est = ACME_treated, ci_lb = ACME_treated_ci_lb, ci_ub = ACME_treated_ci_ub) %>% 
  mutate(out_type = "cont", 
         sig_effect = ifelse((ci_lb > 0 | ci_ub < 0), TRUE, FALSE)) %>% 
  label_treatments() %>% 
  label_mediators() %>% 
  label_outcomes(include_time_labels = T) %>% 
  label_sig_effects()

negcontrol_plt = 
  ggplot(plt_df_mediated_effects, aes(x = outcome_label, y = est, color = outcome_group)) + 
  geom_hline(yintercept = 0, alpha = 0.8, color = "darkgray", linetype = "dashed") + 
  geom_point(position = position_dodge(width = 0.7)) + 
  geom_linerange(aes(ymin = ci_lb, ymax = ci_ub), position = position_dodge(width = 0.7)) + 
  facet_wrap(~treatment_label) + 
  coord_flip() + 
  scale_x_discrete(limits=rev)+ 
  theme_minimal() + 
  scale_color_manual(drop = FALSE, 
                     values = c("#CB7972", "#7AA4BB"), 
                     limits = c("Past 1 Month", "Past 3 Months")) + 
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title.position = "plot") +
  xlab("") + 
  ylab("Prevalence Differences (95% CI)") + 
  ggtitle(glue("Negative Control (Bruising in Past 7 Days) Mediated Effects"))

ggsave(here("figures", "mediated-effects", "negcontrol_med_effects.png"), 
       negcontrol_plt,
       width = 10, height = 8, bg = "white")
