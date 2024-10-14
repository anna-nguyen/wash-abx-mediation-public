
plot_mediated_effects = function(df, facet_var, includes_facets = TRUE, scale = "free", n_facet_row = 1) {
  mediation_plot = 
    ggplot(df,
           aes(x = est, y = mediator_label, color = mediator_group)) + 
    geom_vline(xintercept = 0, alpha = 0.8, color = "darkgray", linetype = "dashed") + 
    geom_point(position = position_dodge(width = 0.7)) + 
    geom_linerange(aes(xmin = ci_lb, xmax = ci_ub), position = position_dodge(width = 0.7)) + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    scale_y_discrete(limits=rev)+ 
    theme_minimal() + 
    scale_color_manual(drop = FALSE, 
                       values = c("#38182F", "#E6756B", "#639A95", "#8D91A9") , 
                       limits = c("any_infection", "ari", "diarrhea", "virus")) + 
    theme(legend.position = "bottom",
          legend.title=element_blank(),
          plot.title.position = "plot",
          panel.spacing = unit(1, "lines")) + 
    xlab("Prevalence Differences (95% CI)") +
    ylab("")  
  
  if (includes_facets) {
    mediation_plot = mediation_plot + facet_wrap(as.formula(glue("~ {facet_var}")), scales = scale, nrow = n_facet_row)
  }
  
  return(mediation_plot)
}

plot_mediation_with_interactions = function(df, facet_var, include_facets, n_facet_row = 1) {
  plt = ggplot(df, aes(x = est, y = mediator_label, group = forcats::fct_rev(type), color = forcats::fct_rev(type))) + 
    geom_point(position = position_dodge(width = 0.7)) + 
    geom_linerange(aes(xmin = ci_lb, xmax = ci_ub), position = position_dodge(width = 0.7)) + 
    geom_vline(xintercept = 0, alpha = 0.8, color = "darkgray", linetype = "dashed") + 
    scale_color_discrete(breaks=c("Average", "Total Natural Indirect Effect", "Pure Natural Indirect Effect")) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
    scale_y_discrete(limits=rev) +  
    theme_minimal() + 
    theme(legend.position = "bottom",
          legend.title=element_blank(),
          plot.title.position = "plot") +
    ylab("") + 
    xlab("Prevalence Differences (95% CI)")
  
  if (include_facets){
    plt = plt + facet_wrap(as.formula(glue("~ {facet_var}")), scales = "fixed", nrow = n_facet_row)
  }
  
  return(plt)
}
