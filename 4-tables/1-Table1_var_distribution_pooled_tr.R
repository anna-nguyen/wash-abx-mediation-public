###############
# Baseline characteristics, pooled treatment vs control
##############
rm(list = ls())
source(here::here("0-config.R"))

merged_df = readRDS(merged_df_filepath)

## Table 1
# Processing numeric variables
table1a <- merged_df %>%
  select(tr_pooled, agem, momage, momheight,
         Nlt18, Ncomp, watmin, n_virus, abdays, abtimes) %>%
  group_by(tr_pooled) %>%
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
  select(tr_pooled, ends_with("_value")) %>% pivot_longer(
    cols = -tr_pooled,  # All columns except `tr_pooled`
    names_to = "variable",  # The name of the new column that will hold the variable names
    values_to = "value"  # The name of the new column that will hold the corresponding values
  ) %>%
  mutate(variable = sub("_mean_value$", "", variable)) %>%  # Remove the "_value" suffix from the variable names
  pivot_wider(
    names_from = tr_pooled,  # Create new columns based on the unique values of `tr_pooled`
    values_from = value  # Fill these new columns with the corresponding `value`
  ) %>% mutate(value = "")

# Processing binary numeric variables
table1b <- merged_df %>% 
  select(tr_pooled, roof, walls, floor, 
         diar7d, ari7d, ari_fever7d, fever7d, pos_virus, 
         any_infection_14mo, any_infection_14_28mo,
         ablastmo, abany, abmult) %>% 
  group_by(tr_pooled) %>% 
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
  select(tr_pooled, ends_with("_value")) %>% pivot_longer(
    cols = -tr_pooled,  # All columns except `tr_pooled`
    names_to = "variable",  # The name of the new column that will hold the variable names
    values_to = "value"  # The name of the new column that will hold the corresponding values
  ) %>%
  mutate(variable = sub("_sum_value$", "", variable)) %>%  # Remove the "_value" suffix from the variable names
  pivot_wider(
    names_from = tr_pooled,  # Create new columns based on the unique values of `tr_pooled`
    values_from = value  # Fill these new columns with the corresponding `value`
  ) %>% mutate(value = "")

# Processing non-numeric variables
table1c <- merged_df %>%
  select(tr_pooled, timeptsub, sex, birthord, momedu, hfiacatrev, HHwealth_quart) %>%
  pivot_longer(cols = -tr_pooled, names_to = "variable", values_to = "value") %>%
  group_by(tr_pooled, variable, value) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(tr_pooled, variable) %>%
  mutate(perc = count / sum(count) * 100) %>%
  ungroup() %>%
  mutate(value_summary = paste0(count, " (", round(perc, 2), "%)")) %>%
  select(variable, value, tr_pooled, value_summary) %>%
  pivot_wider(names_from = tr_pooled, values_from = value_summary, names_sep = "_")

# Combining numeric and non-numeric summaries
N_table <- merged_df %>%
  select(childid, tr_pooled) %>%
  distinct() %>% 
  group_by(tr_pooled) %>% 
  count() %>%
  pivot_wider(values_from = n, names_from = tr_pooled) %>% 
  mutate(value = "", variable = "N")

table1 <- rbind(N_table, table1a, table1b, table1c) %>% select(variable, value, pooled_tr, Control)
colnames(table1) <- c("variable", "value", "Any WASH Intervention", "Control")

order <- c("N", "timeptsub", "agem", "sex", "birthord", "momage", "momheight", "momedu", "hfiacatrev", "Nlt18", "Ncomp", "watmin", "roof", "walls", "floor", "HHwealth_quart", "diar7d", "ari7d", "ari_fever7d", "fever7d", "n_virus", "pos_virus", "ablastmo", "abany", "abdays", "abtimes", "abmult")

table1 <- table1 %>%
  mutate(variable = factor(variable, levels = order)) %>%
  arrange(variable)

table1 <- table1 %>%
  mutate(variable = case_when(
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

write.csv(table1, here("tables", "Table1-variables_by_pooled_tr.csv"))
