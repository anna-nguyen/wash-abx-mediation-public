plot_effects = function(df, y_var, y_type, facet_var = NA, color_var = "sig_effect_label", include_facets = FALSE, scale = "free", plt_title) {
  if (color_var == "sig_effect_label") {
    color_values = c("#F8766D", "#00BFC4")
    color_labels = c("Significant Effect", "Null Effect")
  } 
  
  if (color_var == "mediator_group"){
    color_values = c("#38182F", "#E6756B", "#639A95", "#8D91A9") 
    color_labels = c("any_infection", "ari", "diarrhea", "virus")
  }
  
  if (color_var == "outcome_group"){
    color_values = c("#CB7972", "#7AA4BB") 
    color_labels = c("Past 1 Month", "Past 3 Months")
  }
  
  base_plot = 
    ggplot(df %>% filter(type == y_type), 
           aes_string(x = "est", y = y_var, color = color_var)) + 
    geom_point(show.legend = TRUE) + 
    geom_linerange(aes(xmin = ci_lb, xmax = ci_ub), show.legend=TRUE) + 
    scale_y_discrete(limits = rev) + 
    scale_color_manual(drop = FALSE, 
                       values = color_values, 
                       limits = color_labels) + 
    theme_minimal() + 
    theme(legend.position = "bottom",
          legend.title=element_blank(),
          plot.title.position = "plot") +
    ylab("") + 
    ggtitle(plt_title)
  
  if(include_facets) {
    base_plot = base_plot + 
      facet_grid(as.formula(glue("~ {facet_var}")), scales = scale) 
  }
  
  if (y_type %in% c("cat", "categorical")) {
    final_plot = base_plot +
      xlab("Prevalence Ratio (95% CI)") + 
      geom_vline(xintercept = 1, alpha = 0.8, color = "darkgray", linetype = "dashed") 
  } else {
    final_plot = base_plot +
      xlab("Prevalence Difference (95% CI)") + 
      geom_vline(xintercept = 0, alpha = 0.8, color = "darkgray", linetype = "dashed") 
  }
  
  return(final_plot)
}
