rm(list = ls())
source(here::here("0-config.R"))

merged_df = readRDS(merged_df_filepath)
pooled_age = merged_df %>% mutate(round = 0)
merged_df = merged_df %>% bind_rows(pooled_age)

round_n_size = merged_df %>% group_by(round, tr_pooled) %>% count() %>% rename("n_round" = "n")
round_n_size_wide = round_n_size %>% 
  mutate(tr_pooled = ifelse(tr_pooled == "Control", "Control", "Pooled Intervention"),
         tr_pooled = factor(tr_pooled, levels = c("Pooled Intervention", "Control")),
         n_round = glue("{n_round}")) %>%
  mutate(type = "N") %>% 
  pivot_wider(names_from = "tr_pooled", values_from = "n_round")  

factor_levels = c("N", "Diarrhea only", "ARI only", "Fever only", "Enteric Virus only", 
                  "Diarrhea + ARI", "Diarrhea + Fever", "Diarrhea + ARI + Fever", "ARI + Enteric Virus",
                  "Diarrhea + ARI + Fever + Enteric Virus")

table_s4 = 
  merged_df %>% 
  group_by(round, tr_pooled, diar7d, ari7d, fever7d, pos_virus) %>% 
  count() %>% 
  mutate(pos_virus = ifelse(is.na(pos_virus) & round == 3, 0, pos_virus),
         type = case_when(diar7d == 1 & ari7d == 0 & fever7d == 0 & pos_virus == 0 ~ "Diarrhea only",
                          diar7d == 0 & ari7d == 1 & fever7d == 0 & pos_virus == 0 ~ "ARI only",
                          diar7d == 0 & ari7d == 0 & fever7d == 1 & pos_virus == 0 ~ "Fever only",
                          diar7d == 0 & ari7d == 0 & fever7d == 0 & pos_virus == 1 ~ "Enteric Virus only",
                          diar7d == 1 & ari7d == 1 & fever7d == 0 & pos_virus == 0 ~ "Diarrhea + ARI", 
                          diar7d == 1 & ari7d == 0 & fever7d == 1 & pos_virus == 0 ~ "Diarrhea + Fever", 
                          diar7d == 1 & ari7d == 1 & fever7d == 1 & pos_virus == 0 ~ "Diarrhea + ARI + Fever",
                          diar7d == 0 & ari7d == 1 & fever7d == 0 & pos_virus == 1 ~ "ARI + Enteric Virus",
                          diar7d == 1 & ari7d == 1 & fever7d == 1 & pos_virus == 1 ~ "Diarrhea + ARI + Fever + Enteric Virus")) %>% 
  filter(!is.na(type)) %>% 
  ungroup() %>% 
  left_join(round_n_size, by = c("round", "tr_pooled")) %>% 
  mutate(p = n / n_round,
         p = format(round(p * 100, 1), nsmall = 1), 
         prev = glue::glue("{n} ({p}%)"),
         tr_pooled = ifelse(tr_pooled == "Control", "Control", "Pooled Intervention"),
         tr_pooled = factor(tr_pooled, levels = c("Pooled Intervention", "Control"))) %>% 
  select(round, tr_pooled, type, prev) %>%
  pivot_wider(names_from = "tr_pooled", values_from = "prev") %>% 
  bind_rows(round_n_size_wide) %>% 
  mutate(type = factor(type, levels = factor_levels),
         round = case_when(round == 0 ~ "Pooled Ages", 
                           round == 2 ~ "14 months", 
                           round == 3 ~ "28 months"),
         round = factor(round, levels = c("Pooled Ages", "14 months", "28 months"))) %>% 
  arrange(round, type)

write_csv(table_s4, here::here("tables", "TableS4-mediator-joint-prevalences.csv"))
