rm(list = ls())
source(here::here("0-config.R"))

mediated_effects_RR = readRDS(paste0(mediated_effects_results_directory, "mediated_effects_RR.RDS"))

# Function to compute the E-value for RR > 1
compute_e_val_rr_gt_1 <- function(RR, LL, UL) {
  e_value <- RR + sqrt(RR * (RR - 1))
  e_value_bound <- ifelse(LL <= 1, 1, LL + sqrt(LL * (LL - 1)))
  return(list(E_value = e_value, E_value_bound = e_value_bound))
}

# Function to compute the E-value for RR < 1
compute_e_val_rr_lt_1 <- function(RR, LL, UL) {
  RR_star <- 1 / RR
  e_value <- RR_star + sqrt(RR_star * (RR_star - 1))
  e_value_bound <- ifelse(UL >= 1, 1, {
    UL_star <- 1 / UL
    UL_star + sqrt(UL_star * (UL_star - 1))
  })
  return(list(E_value = e_value, E_value_bound = e_value_bound))
}

add_mediational_e_value <- function(data) {
  data <- data %>%
    rowwise() %>%
    mutate(
      ACME_e_val_result = ifelse(ACME_average > 1,
                                 list(compute_e_val_rr_gt_1(ACME_average, ACME_average_ci_lb, ACME_average_ci_ub)),
                                 list(compute_e_val_rr_lt_1(ACME_average, ACME_average_ci_lb, ACME_average_ci_ub))),
      ADE_e_val_result = ifelse(ADE_average > 1,
                                list(compute_e_val_rr_gt_1(ADE_average, ADE_average_ci_lb, ADE_average_ci_ub)),
                                list(compute_e_val_rr_lt_1(ADE_average, ADE_average_ci_lb, ADE_average_ci_ub))),
      ACME_mediational_eval = ACME_e_val_result$E_value,
      ACME_mediational_eval_bound = ACME_e_val_result$E_value_bound,
      ADE_mediational_eval = ADE_e_val_result$E_value,
      ADE_mediational_eval_bound = ADE_e_val_result$E_value_bound
    ) %>%
    ungroup() %>%
    dplyr::select(-ACME_e_val_result, -ADE_e_val_result)
  
  return(data)
}

mediation_e_vals = add_mediational_e_value(mediated_effects_RR)

saveRDS(mediation_e_vals, here::here("results", "mediation_e_vals_RR.RDS"))

e_val_plt_df = mediation_e_vals %>% 
  select(ADE = ADE_mediational_eval, ACME = ACME_mediational_eval) %>% 
  pivot_longer(cols = c(ADE, ACME))
  
e_val_plt = ggplot(e_val_plt_df, aes(x = value)) + 
  geom_histogram() + 
  facet_wrap(~name, scales = "free") + 
  theme_minimal() +
  xlab("E-Value") + 
  ylab("Count") 

ggsave(here::here("figures", "e_val_plt.png"), e_val_plt, 
       width = 7, height = 3, bg = "white")
  




