fit_mediation_models = function(df, tr_string, med_string, out_string, 
                                include_tr_med_interaction = FALSE, 
                                effect_scale = "absolute"){
  model_df = df %>% filter(!is.na(!!sym(med_string)),
                           !is.na(!!sym(out_string)),
                           !is.na(!!sym(tr_string)),
                           !is.na(agem))
  
  out_covars = screen_covariates(model_df, outcome_str = out_string)
  
  model_df = model_df %>% 
    drop_na(all_of(out_covars))
  
  tr_mediator_grid = expand_grid(tr_string, med_string) %>%
    rename(x = tr_string, y = med_string) %>% 
    mutate(formula_string = glue("{y} ~ {x} + agem"), 
           formula_string = ifelse(length(out_covars) > 0, 
                                   glue::glue("{formula_string} + {paste(out_covars, collapse = ' + ')}"),
                                   formula_string),
           y_type = ifelse(y %in% continuous_mediators, "continuous", "categorical"))
  
  mediator_model = fit_tr_mediator_model(df = model_df,
                                         model_params_row = tr_mediator_grid[1, ],
                                         adjusted = FALSE,
                                         return_model = TRUE)
  
  tr_med_out_grid = tr_mediator_grid %>% 
    select(tr = x, x = y) %>% 
    mutate(y = out_string)  %>% 
    mutate(formula_string = ifelse(include_tr_med_interaction, glue("{y} ~ {tr} * {x} + agem"), glue("{y} ~ {tr} + {x} + agem")),
           formula_string = ifelse(length(out_covars) > 0, 
                                   glue::glue("{formula_string} + {paste(out_covars, collapse = ' + ')}"),
                                   formula_string),
           y_type = ifelse(y %in% continuous_outcomes, "continuous", "categorical"))
  
  outcome_model = fit_tr_mediator_model(df = model_df,
                                        model_params_row = tr_med_out_grid[1, ],
                                        adjusted = FALSE,
                                        return_model = TRUE)
  
  if (effect_scale == "absolute") {
    mediate_function = mediate
  } else {
    mediate_function = mediate.RR
  }
  
  for (i in 1:10) {
    mediate.fit <- try({
      mediate_function(
        model.m = mediator_model,
        model.y = outcome_model,
        sims = 1000,
        treat = tr_string,
        control.value = "Control",
        treat.value = model_df %>% filter(!!sym(tr_string) != "Control") %>% pull(!!sym(tr_string)) %>% unique(),
        mediator = med_string,
        conf.level = 0.95, 
        boot = FALSE,
        cluster = model_df %>% pull(block))
    })
    
    if (class(mediate.fit) != "try-error") {
      break
    } else{
      message(glue("Mediation model failed to converge on try {i}, trying again"))
    }
  }
  

  if (include_tr_med_interaction) {
    int_test = test.TMint(mediate.fit)
    interaction_diff_est = int_test$statistic %>% as.numeric()
    interaction_pval = int_test$p.value
  } else {
    interaction_diff_est = NA
    interaction_pval = NA
  }

  return(
    data.frame(tr = tr_string, mediator = med_string, outcome = out_string, 
               covars = paste(out_covars, collapse = ", "),
               interaction_diff_est = interaction_diff_est, 
               interaction_pval = interaction_pval) %>% 
      bind_cols(extract_mediated_effects(summary(mediate.fit)))
  )
}

extract_mediated_effects = function(mediate_summary, n_round = 4){
  data.frame(
    total_effect = round(mediate_summary$tau.coef, n_round),
    total_effect_ci_lb = round(mediate_summary$tau.ci[1],n_round),
    total_effect_ci_ub = round(mediate_summary$tau.ci[2],n_round),
    
    p_mediated_avg = round(mediate_summary$n.avg, n_round),
    p_mediated_avg_ci_lb = round(mediate_summary$n.avg.ci[1],n_round),
    p_mediated_avg_ci_ub = round(mediate_summary$n.avg.ci[2],n_round),
    
    p_mediated_control = round(mediate_summary$n0, n_round),
    p_mediated_control_ci_lb = round(mediate_summary$n0.ci[1],n_round),
    p_mediated_control_ci_ub = round(mediate_summary$n0.ci[2],n_round),
    
    p_mediated_treated = round(mediate_summary$n1, n_round),
    p_mediated_treated_ci_lb = round(mediate_summary$n1.ci[1],n_round),
    p_mediated_treated_ci_ub = round(mediate_summary$n1.ci[2],n_round),
    
    ACME_average = mediate_summary$d.avg,
    ACME_average_ci_lb = round(mediate_summary$d.avg.ci[1],n_round),
    ACME_average_ci_ub = round(mediate_summary$d.avg.ci[2],n_round),
    ACME_p_val = round(mediate_summary$d.avg.p,n_round),
    
    ADE_average = mediate_summary$z.avg,
    ADE_average_ci_lb = round(mediate_summary$z.avg.ci[1],n_round),
    ADE_average_ci_ub = round(mediate_summary$z.avg.ci[2],n_round),
    
    ACME_control = mediate_summary$d0,
    ACME_control_ci_lb = round(mediate_summary$d0.ci[1],n_round),
    ACME_control_ci_ub = round(mediate_summary$d0.ci[2],n_round),
    
    ACME_treated = mediate_summary$d1,
    ACME_treated_ci_lb = round(mediate_summary$d1.ci[1],n_round), 
    ACME_treated_ci_ub = round(mediate_summary$d1.ci[2],n_round),
    
    ADE_control = mediate_summary$z0,
    ADE_control_ci_lb = round(mediate_summary$z0.ci[1],n_round), 
    ADE_control_ci_ub = round(mediate_summary$z0.ci[2],n_round),
    
    ADE_treated = mediate_summary$z1,
    ADE_treated_ci_lb = round(mediate_summary$z1.ci[1],n_round), 
    ADE_treated_ci_ub = round(mediate_summary$z1.ci[2],n_round)
  )
}
