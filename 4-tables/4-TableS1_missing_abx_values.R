rm(list = ls())
source(here::here("0-config.R"))

merged_df = readRDS(merged_df_filepath)

merged_df %>% 
  select(round, outcome_list) %>% 
  mutate(across(outcome_list, ~if_else(is.na(.), 1, 0))) %>% 
  mutate(n_missing = rowSums(select(., starts_with("ab")))) %>% 
  group_by(round) %>% 
  summarize(p_not_missing = mean(n_missing == 0) * 100, 
            n_not_missing = sum(n_missing == 0))

n_missing = merged_df %>% 
  select(round, tr = tr_label, all_of(outcome_list)) %>% 
  group_by(round, tr) %>% 
  summarize_all(~sum(is.na(.)))

colnames(n_missing) = str_replace(colnames(n_missing), "ab", "n_ab")

p_missing = merged_df %>% 
  select(round, tr = tr_label, all_of(outcome_list)) %>% 
  group_by(round, tr) %>% 
  summarize_all(~round(mean(is.na(.)) * 100, 2))

colnames(p_missing) = str_replace(colnames(p_missing), "ab", "p_ab")


missing_abx_tbl = n_missing %>% 
  left_join(p_missing, by = c("round", "tr")) 

for (outcome in outcome_list) {
  n_colname = paste0("n_", outcome)
  p_colname = paste0("p_", outcome)
  missing_abx_tbl = missing_abx_tbl %>% 
    mutate(!!(outcome) := paste0(!!sym(n_colname), " (", !!sym(p_colname), "%)"))
}

missing_abx_tbl = missing_abx_tbl %>% 
  select(-starts_with("n_"), -starts_with("p_")) %>% 
  rename("Intervention Group" = tr, 
         "Any Antibiotics (Past Month)" = ablastmo, 
         "Any Antibiotics (Past 3 Months)" = abany, 
         "Days of Antibiotic Use (Past 3 Months)" = abdays,
         "Episodes of Antibiotic Use (Past 3 Months)" = abtimes, 
         "Multiple Episodes of Antibiotic Use (Past 3 Months)" = abmult)

write_csv(missing_abx_tbl, here::here("tables", "TableS1-missing_abx_values.csv"))
