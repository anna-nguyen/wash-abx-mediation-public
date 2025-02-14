###############
# Baseline characteristics, all treatment groups
# Nutrition, WSH, NWSH, Nutrition + NWSH, WSH + NWSH, Control
##############
rm(list = ls())
source(here::here("0-config.R"))

merged_df = readRDS(merged_df_filepath)

merged_df <- merged_df %>% mutate(tr = case_when(
  tr_n == "Nutrition" ~ "Nutrition",
  tr_wsh == "WSH" ~ "WSH",
  tr_n_wsh == "Nutrition + WSH" ~ "NWSH",
  TRUE ~ "Control"
))

merged_df2 <- merged_df %>% 
  mutate(tr = ifelse(tr_wsh_pooled == "pooled_WSH", "WSH_NWSH", "Control")) %>% 
  filter(tr == "WSH_NWSH")

merged_df3 <- merged_df %>% 
  mutate(tr = ifelse(tr_n_pooled == "pooled_N", "N_NWSH", "Control")) %>% 
  filter(tr == "N_NWSH")

merged_df <- rbind(merged_df, merged_df2, merged_df3)

## Table 2
# Processing numeric variables
tableS2a <- merged_df %>%
  select(tr, agem, momage, momheight,
         Nlt18, Ncomp, watmin, n_virus, abdays, abtimes) %>%
  group_by(tr) %>%
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
  select(tr, ends_with("_value")) %>% pivot_longer(
    cols = -tr,  # All columns except `tr`
    names_to = "variable",  # The name of the new column that will hold the variable names
    values_to = "value"  # The name of the new column that will hold the corresponding values
  ) %>%
  mutate(variable = sub("_mean_value$", "", variable)) %>%  # Remove the "_value" suffix from the variable names
  pivot_wider(
    names_from = tr,  # Create new columns based on the unique values of `tr`
    values_from = value  # Fill these new columns with the corresponding `value`
  ) %>% mutate(value = "")

# Processing binary numeric variables
tableS2b <- merged_df %>% 
  select(tr, roof, walls, floor, 
         diar7d, ari7d, ari_fever7d, fever7d, pos_virus, 
         any_infection_14mo, any_infection_14_28mo, 
         ablastmo, abany, abmult) %>% 
  group_by(tr) %>% 
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
  select(tr, ends_with("_value")) %>% pivot_longer(
    cols = -tr,  # All columns except `tr`
    names_to = "variable",  # The name of the new column that will hold the variable names
    values_to = "value"  # The name of the new column that will hold the corresponding values
  ) %>%
  mutate(variable = sub("_sum_value$", "", variable)) %>%  # Remove the "_value" suffix from the variable names
  pivot_wider(
    names_from = tr,  # Create new columns based on the unique values of `tr`
    values_from = value  # Fill these new columns with the corresponding `value`
  ) %>% mutate(value = "")

# Processing non-numeric variables
tableS2c <- merged_df %>%
  select(tr, timeptsub, sex, birthord, momedu, hfiacatrev, HHwealth_quart) %>%
  pivot_longer(cols = -tr, names_to = "variable", values_to = "value") %>%
  group_by(tr, variable, value) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(tr, variable) %>%
  mutate(perc = count / sum(count) * 100) %>%
  ungroup() %>%
  mutate(value_summary = paste0(count, " (", round(perc, 2), "%)")) %>%
  select(variable, value, tr, value_summary) %>%
  pivot_wider(names_from = tr, values_from = value_summary, names_sep = "_")

# Combining numeric and non-numeric summaries
N_table <- merged_df %>% 
  select(childid, tr) %>% 
  distinct() %>% 
  group_by(tr) %>% 
  count() %>% 
  pivot_wider(values_from = n, names_from = tr) %>% 
  mutate(value = "", variable = "N")

tableS2 <- rbind(N_table, tableS2a, tableS2b, tableS2c) %>% 
  select(variable, value, "Nutrition", "WSH", "NWSH", "N_NWSH", "WSH_NWSH", "Control")
colnames(tableS2) <- c("variable", "value", "Nutrition", "WSH", "NWSH", "Nutrition + NWSH", "WSH + NWSH", "Control")

order <- c("N", "timeptsub", "agem", "sex", "birthord", "momage", "momheight", "momedu", "hfiacatrev", "Nlt18", "Ncomp", "watmin", "roof", "walls", "floor", "HHwealth_quart", "any_infection_14mo", "any_infection_14_28mo", "diar7d", "ari7d", "ari_fever7d", "fever7d", "n_virus", "pos_virus", "ablastmo", "abany", "abdays", "abtimes", "abmult")

tableS2 <- tableS2 %>%
  mutate(variable = factor(variable, levels = order)) %>%
  arrange(variable)

tableS2 <- tableS2 %>%
  mutate(variable = case_when(
    variable == "bruise7d" ~ "Bruising (Past 7 Days)",
    variable == "diar7d" ~ "Diarrhea (Past 7 Days)",
    variable == "diffbreathing7d" ~ "Difficulty Breathing (Past 7 Days)",
    variable == "any_infection_14mo" ~ "Diarrhea, Fever, ARI, or Enteric Virus", 
    variable == "any_infection_14_28mo" ~ "Diarrhea Fever, or ARI",
    variable == "ari7d" ~ "ARI (Past 7 Days)",
    variable == "ari_fever7d" ~ "ARI with Fever (Past 7 Days)",
    variable == "fever7d" ~ "Fever (Past 7 Days)",
    variable == "fever14d" ~ "Fever (Past 14 Days)",
    variable == "n_virus" ~ "Number of Enteric Viruses", 
    variable == "pos_virus" ~ "Any Enteric Viruses", 
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

write.csv(tableS2, here("tables", "TableS2-variables_by_all_tr.csv"))
