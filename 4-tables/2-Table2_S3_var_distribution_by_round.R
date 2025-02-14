###############
# Baseline characteristics, stratified by # of measurements taken
##############
rm(list = ls())
source(here::here("0-config.R"))

merged_df = readRDS(merged_df_filepath)
missing <- merged_df %>% filter(if_any(starts_with("pos"), is.na) | if_any(ends_with("7d"), is.na))
merged_df <- merged_df %>% mutate(data_round = ifelse(round == 2, "data_14mo", "data_28mo"))

# Processing numeric variables
table2a <- merged_df %>%
  select(tr_pooled, data_round, agem, momage, momheight,
         Nlt18, Ncomp, watmin, n_virus, abdays, abtimes) %>%
  group_by(tr_pooled, data_round) %>%
  summarize(across(
    .cols = everything(),
    .fns = list(
      mean = ~ mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE)
    ),
    .names = "{col}_{fn}"
  )) %>%
  mutate(across(
    .cols = ends_with("_mean"),
    .fns = ~ paste0(sprintf("%.2f", .),
                    " (",
                    sprintf("%.2f", get(sub("_mean$", "_sd", cur_column()))),
                    ")"),
    .names = "{col}_value"
  )) %>%
  select(tr_pooled, data_round, ends_with("_value")) %>% pivot_longer(
    cols = ends_with("_value"),  # All columns where calculations were performed
    names_to = "variable", # The name of the new column that will hold the variable names
    values_to = "value"  # The name of the new column that will hold the corresponding values
  ) %>%
  mutate(variable = sub("_mean_value$", "", variable)) %>%
  pivot_wider(
    names_from = c(tr_pooled, data_round),
    names_sep = "_",
    values_from = value  
  ) %>% mutate(value = "")

# Processing binary numeric variables
table2b <- merged_df %>% 
  select(tr_pooled, data_round, roof, walls, floor, 
         diar7d, ari7d, ari_fever7d, fever7d, pos_virus, 
         any_infection_14mo, any_infection_14_28mo,
         ablastmo, abany, abmult) %>% 
  group_by(tr_pooled, data_round) %>% 
  summarize(across(
    .cols = everything(),
    .fns = list(
      sum = ~ sum(.x, na.rm = TRUE),
      perc = ~ round(mean(.x, na.rm = TRUE) * 100, 2)
    ),
    .names = "{col}_{fn}"
  )) %>%
  mutate(across(
    .cols = ends_with("_sum"),
    .fns = ~ paste0(.,
                    " (",
                    get(sub("_sum$", "_perc", cur_column())),
                    "%)"),
    .names = "{col}_value"
  )) %>%
  select(tr_pooled, data_round, ends_with("_value")) %>% pivot_longer(
    cols = ends_with("_value"),  # All columns where calculations were performed
    names_to = "variable", # The name of the new column that will hold the variable names
    values_to = "value"  # The name of the new column that will hold the corresponding values
  ) %>%
  mutate(variable = sub("_sum_value$", "", variable)) %>%
  pivot_wider(
    names_from = c(tr_pooled, data_round),
    names_sep = "_",
    values_from = value  
  ) %>% mutate(value = "")

# Processing non-numeric variables
table2c <- merged_df %>%
  select(tr_pooled, data_round, timeptsub, sex, birthord, momedu, hfiacatrev, HHwealth_quart) %>%
  pivot_longer(cols = c(-data_round, -tr_pooled), names_to = "variable", values_to = "value") %>%
  group_by(tr_pooled, data_round, variable, value) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(tr_pooled, data_round, variable) %>%
  mutate(perc = count / sum(count) * 100) %>%
  ungroup() %>%
  mutate(value_summary = paste0(count, " (", round(perc, 2), "%)")) %>%
  select(variable, value, tr_pooled, data_round, value_summary) %>%
  pivot_wider(names_from = c(tr_pooled, data_round), 
              names_sep = "_",
              values_from = value_summary)

# create rows for N, round, tr_pooled
N_row <- merged_df %>% 
  group_by(tr_pooled, data_round) %>% 
  count() %>% 
  pivot_wider(values_from = n,
              names_from = c(tr_pooled, data_round),
              names_sep = "_") %>% 
  mutate(value = "", variable = "N")

round_row <- merged_df %>% group_by(tr_pooled, data_round) %>% summarize() %>% 
  pivot_wider(values_from = data_round,
              names_from = c(tr_pooled, data_round),
              names_sep = "_") %>% 
  mutate(value = "", variable = "data_round")

tr_row <- merged_df %>% group_by(tr_pooled, data_round) %>% summarize() %>% 
  pivot_wider(values_from = tr_pooled,
              names_from = c(tr_pooled, data_round),
              names_sep = "_") %>% 
  mutate(value = "", variable = "tr_pooled")

# Combining numeric and non-numeric summaries
table2 <- rbind(tr_row, round_row, N_row, table2a, table2b, table2c) %>% 
  select(variable, value, 
         pooled_tr_data_14mo, pooled_tr_data_28mo,
         Control_data_14mo, Control_data_28mo)

order <- c("tr_pooled", "data_round", "N", "timeptsub", "agem", "sex", "birthord", "momage", "momheight", "momedu", "hfiacatrev", "Nlt18", "Ncomp", 
           "watmin", "roof", "walls", "floor", "HHwealth_quart", "any_infection_14mo", "any_infection_14_28mo", "diar7d", "ari7d", "ari_fever7d", "fever7d", "n_virus", "pos_virus", "ablastmo", "abany", "abdays", "abtimes", "abmult")

table2 <- table2 %>%
  mutate(variable = factor(variable, levels = order)) %>%
  arrange(variable)

table2 <- table2 %>%
  mutate(variable = case_when(
    variable == "tr_pooled" ~ "Intervention Pool",
    variable == "data_round" ~ "Data Collection Round",
    variable == "bruise7d" ~ "Bruising (Past 7 Days)",
    variable == "diar7d" ~ "Diarrhea (Past 7 Days)",
    variable == "diffbreathing7d" ~ "Difficulty Breathing (Past 7 Days)",
    variable == "ari7d" ~ "ARI (Past 7 Days)",
    variable == "ari_fever7d" ~ "ARI with Fever (Past 7 Days)",
    variable == "fever7d" ~ "Fever (Past 7 Days)",
    variable == "fever14d" ~ "Fever (Past 14 Days)",
    variable == "n_virus" ~ "Number of Enteric Viruses", 
    variable == "pos_virus" ~ "Any Enteric Viruses", 
    variable == "any_infection_14mo" ~ "Diarrhea, Fever, ARI, or Enteric Virus", 
    variable == "any_infection_14_28mo" ~ "Diarrhea Fever, or ARI",
    variable == "ablastmo" ~ "Any Antibiotics\n(Past Month)",
    variable == "abany" ~ "Any Antibiotics\n(Past 3 Months)",
    variable == "abdays" ~ "Days of Antibiotic Use\n(Past 3 Months)",
    variable == "abtimes" ~ "Episodes of Antibiotic Use\n(Past 3 Months)",
    variable == "abmult" ~ "Multiple Episodes of Antibiotic Use\n(Past 3 Months)",
    variable == "timeptsub" ~ "Measurement date",
    variable == "agem" ~ "Child's age",
    variable == "sex" ~ "Child's sex",
    variable == "birthord" ~ "Birth order",
    variable == "momage" ~ "Mother's age",
    variable == "momheight" ~ "Mother's height",
    variable == "momedu" ~ "Mother's education",
    variable == "hfiacatrev" ~ "Household food insecurity",
    variable == "Nlt18" ~ "Number of individuals in household under age 18",
    variable == "Ncomp" ~ "Number of individuals in compound",
    variable == "watmin" ~ "Distance to water source",
    variable == "roof" ~ "Household Roof Material",
    variable == "walls" ~ "Household Wall Material",
    variable == "floor" ~ "Household Floor Material",
    variable == "HHwealth_quart" ~ "Household Wealth Index",
    TRUE ~ variable  # Keep the original value if no match is found
  ))

write.csv(table2, here("tables", "Table2-variables_by_meas_round.csv"))
