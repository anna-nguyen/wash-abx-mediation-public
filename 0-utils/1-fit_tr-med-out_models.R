fit_tr_mediator_model = fit_mediator_outcome_model = fit_tr_outcome_model = 
  function(df, model_params_row, adjusted = FALSE, return_model = FALSE, cat_estimate_type = "PR"){
    model_df = df %>% 
      filter(!is.na(!!sym(model_params_row$x)),
             !is.na(!!sym(model_params_row$y)))
    
    if (exists("model_params_row$tr")){
      model_df = model_df %>% filter(!is.na(!!sym(model_params_row$tr)))
    }
    
    formula_string = model_params_row$formula_string
    
    if (adjusted){
      filtered_covariates = covariate_list # screen_covariates(df = model_df, outcome_str = model_params_row$y)
      if(length(filtered_covariates) > 0) {
        covariates_formula = paste(filtered_covariates, collapse = " + ")
        formula_string = glue("{formula_string} + {covariates_formula}") 
      }
    }
    
    if (model_params_row$y_type == "continuous"){
      model_fit = eval(bquote(glm(formula = .(as.formula(formula_string)), data = model_df)))
    } else {
      model_fit = eval(bquote(glm(formula = .(as.formula(formula_string)),                       
                                  family = poisson(link = "log"),
                                  data = model_df)))
    }
    
    if (return_model){
      return(model_fit)
    }
    
    if((cat_estimate_type == "PD" & model_params_row$y_type == "categorical")) {
      PDs = c()
      for (i in 1:1000) {
        bootstrap_blocks = model_df %>% select(block) %>% distinct() %>% sample_n(size = nrow(.), replace = T)
        bootstrap_sample = bootstrap_blocks %>% left_join(model_df, by = "block", relationship = "many-to-many")
        model_fit = eval(bquote(glm(formula = .(as.formula(formula_string)),                       
                                    family = "binomial",
                                    data = bootstrap_sample)))
        
        tr_string = model_params_row$x
        tr_contrast = bootstrap_sample %>% filter(!!sym(tr_string) != "Control") %>% pull(tr_string) %>% unique()
        p1 = mean(predict(model_fit, bootstrap_sample %>% mutate(!!sym(tr_string) := tr_contrast), type = "response"), na.rm = T)
        p0 = mean(predict(model_fit, bootstrap_sample %>% mutate(!!sym(tr_string) := "Control"), type = "response"), na.rm = T)
        PDs = c(PDs,  p1 - p0)
      }
      
      effect_est = mean(PDs)
      effect_ci = quantile(PDs, c(0.025, 0.975), na.rm = T) %>% as.numeric()
      effect_pval = NA
    } else {
      model_summary = model_fit %>% summary() %>% pluck(coefficients)
      effect_est = model_summary[2, 1]
      
      effect_covar_matrix = vcovCL(model_fit, cluster = model_df %>% pull(block), sandwich = TRUE) 
      effect_var = diag(effect_covar_matrix)[2] %>% as.numeric()
      effect_se = sqrt(effect_var)
      effect_pval = 2 * pnorm(-abs(effect_est/effect_se))
      
      effect_ci = effect_est + qnorm(c(0.025, 0.975)) * effect_se
      
      if (model_params_row$y_type == "categorical") {
        effect_est = exp(effect_est)
        effect_ci = exp(effect_ci)
      }
    }
    
    return(data.frame(x = model_params_row$x, y = model_params_row$y, 
                      est = effect_est, pval = effect_pval,
                      ci_lb = effect_ci[1], ci_ub = effect_ci[2]))
  }

screen_covariates = function(df, outcome_str, lr_pval_cutoff = 0.2, prev_cutoff = 0.05, print_res = FALSE) {
  excluded_covariates = c()
  
  for (covar_str in covariate_list) {
    # Exclude categorical covariates with prevalence less than cutoff
    filtered_df = df %>% select(all_of(outcome_str), all_of(covar_str)) %>% na.omit() 
    
    if (covar_str %in% categorical_covariates) {
      min_prev = min(df[covar_str] %>% table() %>% prop.table())
      if (min_prev < prev_cutoff) {
        excluded_covariates = c(excluded_covariates, covar_str)
      }
      null_model = glm(as.formula(glue::glue("{outcome_str} ~ 1")), family = poisson(link = "log"), data = filtered_df) 
      full_model = glm(as.formula(glue::glue("{outcome_str} ~ {covar_str}")), family = poisson(link = "log"), data = filtered_df) 
      lr_test = lrtest(null_model, full_model) 
    } else {
      null_model = glm(as.formula(glue::glue("{outcome_str} ~ 1")), data = filtered_df) 
      full_model = glm(as.formula(glue::glue("{outcome_str} ~ {covar_str}")), data = filtered_df) 
      lr_test = lrtest(null_model, full_model) 
    }
    
    if (lr_test$`Pr(>Chisq)`[2] > lr_pval_cutoff){
      excluded_covariates = c(excluded_covariates, covar_str)
    }
  }
  
  if (print_res) {
    print(glue::glue("Filtered out covariates: {paste(excluded_covariates, collapse = ', ')}"))
  }
  
  filtered_covariates = covariate_list[!(covariate_list %in% excluded_covariates)]
  
  return(filtered_covariates)
}

  
