rm(list = ls())
source(here::here("0-config.R"))

###############################################
# Process antibiotic use data
###############################################
# Restrict to measurements at 14 months (round 2)
# Remove missing antibiotic measurements, treatment labels
ab_df = read_csv(ab_df_filepath) %>% 
  filter(round %in% c(2, 3), !is.na(abany), !is.na(tr_label))

ab_df = ab_df %>% 
  select(childid, round, cluster_id, block, tr_label, abtimes, abany, abmult, ablast2wks, ablastmo, abdays, any_of(covariate_list))
  
remaining_covars = covariate_list[!(covariate_list %in% colnames(ab_df))]

###############################################
# Process diarrhea, ARI data
###############################################

ages = read_csv(cleaned_wash_filepath) %>% 
  filter(str_detect(childid, "T")) %>% 
  mutate(childno = str_remove(childid, "T"),
         childid = str_remove(dataid, "^0+"),
         childid = paste0(childid, childno) %>% as.numeric(), 
         round = svy + 1, 
         agem = round(agedays/30, 1)) %>% 
  select(childid, round, agem)

midline_child_df = read_csv(midline_child_df_filepath) %>% 
  mutate(childid = str_remove(dataid, "^0+"),
         childid = paste0(childid, childno) %>% as.numeric(), 
         d3plus2d = case_when((q32a == 1 | q32b == 1 | q32c == 1) ~ 1,
                              is.na(q32a) & is.na(q32b) & is.na(q32c) ~ NA_real_,
                              TRUE ~ 0),
         d3plus7d = case_when(q32d == 1 ~ 1,
                              is.na(q32d) ~ NA_real_,
                              TRUE ~ 0), 
         dloose2d = case_when((q34a == 1 | q34b == 1 | q34c == 1) ~ 1,
                              is.na(q34a) & is.na(q34b) & is.na(q34c) ~ NA_real_,
                              TRUE ~ 0),
         dloose7d = case_when(q34d == 1 ~ 1,
                              is.na(q34d) ~ NA_real_,
                              TRUE ~ 0),
         dblood2d = case_when((q35a == 1 | q35b == 1 | q35c == 1) ~ 1,
                              is.na(q35a) & is.na(q35b) & is.na(q35c) ~ NA_real_,
                              TRUE ~ 0),
         dblood7d = case_when(q35d == 1 ~ 1,
                              is.na(q35d) ~ NA_real_,
                              TRUE ~ 0),
         diar2d = case_when((d3plus2d == 1 & dloose2d == 1) | (dblood2d == 1) ~ 1,
                            (is.na(d3plus2d) & is.na(dloose2d) & is.na(dblood2d)) ~ NA_real_,
                            (is.na(d3plus2d) | is.na(dloose2d)) & is.na(dblood2d) ~ NA_real_,
                            TRUE ~ 0),
         diar7d = case_when((d3plus7d == 1 & dloose7d == 1) | (dblood7d == 1) ~ 1,
                            diar2d == 1 ~ 1,
                            (is.na(d3plus7d) & is.na(dloose7d) & is.na(dblood7d) & is.na(diar2d)) ~ NA_real_,
                            (is.na(d3plus7d) | is.na(dloose7d)) & is.na(dblood7d) & is.na(diar2d) ~ NA_real_,
                            TRUE ~ 0), 
         cough7d = case_when((q37a == 1 | q37b == 1 | q37c == 1 | q37d == 1) ~ 1,
                             (is.na(q37a) & is.na(q37b) & is.na(q37c) & is.na(q37d)) ~ NA_real_,
                             TRUE ~ 0),
         diffbreathing7d = case_when((q39a == 1 | q39b == 1 | q39c == 1 | q39d == 1) ~ 1,
                                     (is.na(q39a) & is.na(q39b) & is.na(q39c) & is.na(q39d)) ~ NA_real_,
                                     TRUE ~ 0),
         fever7d = case_when((q30a == 1 | q30b == 1 | q30c == 1 | q30d == 1) ~ 1,
                             (is.na(q30a) & is.na(q30b) & is.na(q30c) & is.na(q30d)) ~ NA_real_,
                             TRUE ~ 0),
         fever14d = case_when((fever7d == 1 | q30e == 1) ~ 1,
                              (is.na(fever7d) & is.na(q30e)) ~ NA_real_,
                              TRUE ~ 0), 
         ari7d = case_when((cough7d == 1 | diffbreathing7d == 1) ~ 1,
                           (is.na(cough7d) & is.na(diffbreathing7d)) ~ NA_real_,
                           TRUE ~ 0), 
         ari_fever7d = case_when((ari7d == 1 & fever7d == 1) ~ 1,
                                 (is.na(ari7d) & is.na(fever7d)) ~ NA_real_,
                                 TRUE ~ 0),
         bruise7d = case_when((q40a == 1 | q40b == 1 | q40c == 1) ~ 1,
                              is.na(q40a) & is.na(q40b) & is.na(q40c) ~ NA_real_,
                              TRUE ~ 0),) %>% 
  select(childid, diar7d, cough7d, diffbreathing7d, ari7d, ari_fever7d, fever7d, fever14d, bruise7d) %>% 
  mutate(round = 2) 
  

endline_child_df = read_dta(endline_child_df_filepath) %>% 
  filter(str_detect(childid, "T")) %>% 
  mutate(childno = str_remove(childid, "T"),
         childid = str_remove(dataid, "^0+"),
         childid = paste0(childid, childno) %>% as.numeric(), 
         d3plus2d = case_when((q203a == 1 | q203b == 1 | q203c == 1) ~ 1,
                              is.na(q203a) & is.na(q203b) & is.na(q203c) ~ NA_real_,
                              TRUE ~ 0),
         d3plus7d = case_when(q203d == 1 ~ 1,
                              is.na(q203d) ~ NA_real_,
                              TRUE ~ 0), 
         dloose2d = case_when((q205a == 1 | q205b == 1 | q205c == 1) ~ 1,
                              is.na(q205a) & is.na(q205b) & is.na(q205c) ~ NA_real_,
                              TRUE ~ 0),
         dloose7d = case_when(q205d == 1 ~ 1,
                              is.na(q205d) ~ NA_real_,
                              TRUE ~ 0),
         dblood2d = case_when((q206a == 1 | q206b == 1 | q206c == 1) ~ 1,
                              is.na(q206a) & is.na(q206b) & is.na(q206c) ~ NA_real_,
                              TRUE ~ 0),
         dblood7d = case_when(q206d == 1 ~ 1,
                              is.na(q206d) ~ NA_real_,
                              TRUE ~ 0),
         diar2d = case_when((d3plus2d == 1 & dloose2d == 1) | (dblood2d == 1) ~ 1,
                            (is.na(d3plus2d) & is.na(dloose2d) & is.na(dblood2d)) ~ NA_real_,
                            (is.na(d3plus2d) | is.na(dloose2d)) & is.na(dblood2d) ~ NA_real_,
                            TRUE ~ 0),
         diar7d = case_when((d3plus7d == 1 & dloose7d == 1) | (dblood7d == 1) ~ 1,
                            diar2d == 1 ~ 1,
                            (is.na(d3plus7d) & is.na(dloose7d) & is.na(dblood7d) & is.na(diar2d)) ~ NA_real_,
                            (is.na(d3plus7d) | is.na(dloose7d)) & is.na(dblood7d) & is.na(diar2d) ~ NA_real_,
                            TRUE ~ 0), 
         cough7d = case_when((q208a == 1 | q208b == 1 | q208c == 1 | q208d == 1) ~ 1,
                             (is.na(q208a) & is.na(q208b) & is.na(q208c) & is.na(q208d)) ~ NA_real_,
                             TRUE ~ 0),
         diffbreathing7d = case_when((q210a == 1 | q210b == 1 | q210c == 1 | q210d == 1) ~ 1,
                                     (is.na(q210a) & is.na(q210b) & is.na(q210c) & is.na(q210d)) ~ NA_real_,
                                     TRUE ~ 0),
         fever7d = case_when((q201a == 1 | q201b == 1 | q201c == 1 | q201d == 1) ~ 1,
                             (is.na(q201a) & is.na(q201b) & is.na(q201c) & is.na(q201d)) ~ NA_real_,
                             TRUE ~ 0),
         fever14d = NA, 
         # fever14d = case_when((fever7d == 1 | q201e == 1) ~ 1,
         #                      (is.na(fever7d) & is.na(q201e)) ~ NA_real_,
         #                      TRUE ~ 0), 
         ari7d = case_when((cough7d == 1 | diffbreathing7d == 1) ~ 1,
                           (is.na(cough7d) & is.na(diffbreathing7d)) ~ NA_real_,
                           TRUE ~ 0), 
         ari_fever7d = case_when((ari7d == 1 & fever7d == 1) ~ 1,
                                 (is.na(ari7d) & is.na(fever7d)) ~ NA_real_,
                                 TRUE ~ 0),
         bruise7d = case_when((q211a == 1 | q211b == 1 | q211c == 1 | q211d == 1) ~ 1,
                              (is.na(q211a) & is.na(q211b) & is.na(q211c) & is.na(q211d)) ~ NA_real_,
                              TRUE ~ 0)) %>% 
  select(childid, diar7d, cough7d, diffbreathing7d, ari7d, ari_fever7d, fever7d, fever14d, bruise7d) %>% 
  unique() %>% 
  mutate(round = 3)

child_df = bind_rows(midline_child_df, endline_child_df) %>% 
  left_join(ages, by = c("childid", "round"))

remaining_covars = remaining_covars[!(remaining_covars %in% colnames(child_df))]

###############################################
# Process pathogen data
###############################################
# Ct.Cutoff <- data.frame(organism = c("Adenovirus40_41", "Sapovirus", "Astrovirus", "Campylobacter jejuni/coli", "ETEC_ST", "Norovirus GII", "Shigella_EIEC", "tEPEC", "Rotavirus", "Cryptosporidium", "Isospora", "Strongyloides", "E.histolytica", "V.cholerae", "H.pylori", "Salmonella", "Cyclospora"),
#                         TargetName = c(NA, NA, NA, "cdtA", "STh", NA, "ipaH", "bfpA", NA, "18S_Crypto", NA, NA, "18S_Ehist", "tcpA", "ureA", "invA", NA),
#                         MALED = c(24.0, 26.1, 23.7, 21.8, 23.5, 27.2, 28.8, 17.8, 31.7, 22.0, 33.8, 30.4, 30.0, 32.0, NA, NA, NA),
#                         GEMS = c(22.7, NA, 22.2, 15.4, 22.8,  23.4, 27.9, 16.0, 32.6, 24.0, NA, NA, 32.8, 33.8, 30.8, 30.7, 29.6)) %>%
#   mutate(OR_Ct = round(rowMeans(select(. , c("MALED", "GEMS")), na.rm = T), 1),
#          cq_cutoff = ((35 - OR_Ct)/3.322 ) + 3.699) %>%
#   select(organism, OR_Ct, cq_cutoff) 

Ct.Cutoff <- data.frame(organism = c("Adenovirus40_41", "Sapovirus", "Norovirus GII"),
                        OR_Ct = c(30.2, 28.2, 28.7)) %>%
  mutate(cq_cutoff = ((35 - OR_Ct)/3.322 ) + 3.699) %>%
  select(organism, OR_Ct, cq_cutoff) 


virus_list = c("Adenovirus40_41", "Norovirus GII", "Sapovirus")
virus_cutoffs = Ct.Cutoff %>%
  select(organism, cq_cutoff) %>%
  filter(organism %in% virus_list) %>%
  pivot_wider(names_from = "organism", values_from = "cq_cutoff")

colnames(virus_cutoffs) = paste0(colnames(virus_cutoffs), "_cutoff")

path_df = readRDS(path_df_filepath) %>%
  filter(childid != 68071) %>%  # This is a stool sample that we have for which we have no information on consent/collection. Likely mislabeled, but discrepancy never identified, so discard.
  bind_cols(virus_cutoffs) %>%
  mutate(pos_Adenovirus40_41_symp = (Adenovirus40_41 >= Adenovirus40_41_cutoff),
         pos_Adenovirus40_41_symp = ifelse(pos_Adenovirus40_41 == 0, 0, pos_Adenovirus40_41_symp),
         pos_Norovirus_GII = ifelse(`Norovirus GII` > 0, 1, 0), 
         pos_Norovirus_GII = ifelse(is.na(pos_Norovirus_GII), 0, pos_Norovirus_GII), 
         pos_Norovirus_GII_symp = (`Norovirus GII` >= `Norovirus GII_cutoff`),
         pos_Norovirus_GII_symp = ifelse(pos_Norovirus_GII == 0, 0, pos_Norovirus_GII_symp),
         pos_Sapovirus_symp = (Sapovirus >= Sapovirus_cutoff),
         pos_Sapovirus_symp = ifelse(pos_Sapovirus == 0, 0, pos_Sapovirus_symp),
         n_virus_symp = pos_Sapovirus_symp + pos_Norovirus_GII_symp + pos_Adenovirus40_41_symp, 
         pos_virus_symp = ifelse(n_virus_symp > 0, 1, 0)) %>% 
  select(childid, 
         pos_Adenovirus40_41, pos_Adenovirus40_41_symp, 
         pos_Norovirus_GII, pos_Norovirus_GII_symp,
         pos_Sapovirus, pos_Sapovirus_symp, 
         n_virus, pos_virus, n_virus_symp, pos_virus_symp) %>% 
  mutate(round = 2)

###############################################
# Process pathogen data
###############################################
path_covars = readRDS(path_df_filepath) %>% filter(childid != 68071) %>% select(childid, all_of(remaining_covars))
path_covars = bind_rows(path_covars %>% mutate(round = 2), path_covars %>% mutate(round = 3))

###############################################
# Merge datasets
###############################################

merged_df = ab_df %>% 
  left_join(path_df, by = c("childid", "round")) %>%
  left_join(path_covars, by = c("childid", "round")) %>%
  left_join(child_df, by = c("childid", "round")) %>% 
  filter(!is.na(tr_label), !is.na(abany)) %>% 
  mutate(symp_virus = ifelse((pos_virus == 1 & diar7d == 1), 1, 0), 
         any_infection_14mo = case_when(round != 2 ~ NA, 
                                        diar7d + ari7d + fever7d + pos_virus > 0 ~ 1, 
                                        diar7d + ari7d + fever7d + pos_virus == 0 ~ 0, 
                                        T ~ NA), 
         any_infection_14_28mo = case_when(diar7d + ari7d + fever7d > 0 ~ 1,
                                           diar7d + ari7d + fever7d == 0 ~ 0,
                                           T ~ NA), 
         tr_pooled = ifelse(tr_label == "Control", "Control", "pooled_tr"),
         tr_wsh = ifelse(tr_label %in% c("WSH", "Control"), tr_label, NA),
         tr_n = ifelse(tr_label %in% c("Nutrition", "Control"), tr_label, NA),
         tr_n_wsh = ifelse(tr_label %in% c("Nutrition + WSH", "Control"), tr_label, NA),
         tr_wsh_pooled = case_when(tr_label %in% c("WSH", "Nutrition + WSH") ~ "pooled_WSH",
                                   tr_label == "Control" ~ "Control"),
         tr_n_pooled = case_when(tr_label %in% c("Nutrition", "Nutrition + WSH") ~ "pooled_N",
                                 tr_label == "Control" ~ "Control")) %>% 
  select(-all_of(covariate_list), all_of(covariate_list)) %>% 
  select(childid, cluster_id, block, round, starts_with("tr"), timeptsub, everything()) 

sapply(colnames(merged_df), function(colname) sum(!is.na(merged_df[colname])))
sapply(colnames(merged_df), function(colname) sum(is.na(merged_df[colname])))
sapply(colnames(merged_df), function(colname) mean(is.na(merged_df[colname]))*100 %>% round(1))

saveRDS(merged_df, merged_df_filepath)

