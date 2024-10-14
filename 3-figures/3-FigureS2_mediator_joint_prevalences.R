rm(list = ls())
source(here::here("0-config.R"))
library(VennDiagram)

merged_df = readRDS(merged_df_filepath)

# Venn Diagrams
data_14months = merged_df %>% filter(round == 2)
prev_14months = 
  list(data_14months %>% filter(diar7d == 1) %>% pull(childid),
       data_14months %>% filter(ari7d == 1) %>% pull(childid),
       data_14months %>% filter(fever7d == 1) %>% pull(childid),
       data_14months %>% filter(pos_virus == 1) %>% pull(childid))

venn.diagram(
  x = prev_14months,
  category.names = c("Diarrhea" , "ARI" , "Fever", "Enteric Virus"), 
  filename = here::here("figures", "mediators_venn_diagram_14mo.png"))

data_28months = merged_df %>% filter(round == 3)
prev_28months = 
  list(data_28months %>% filter(diar7d == 1) %>% pull(childid),
       data_28months %>% filter(ari7d == 1) %>% pull(childid),
       data_28months %>% filter(fever7d == 1) %>% pull(childid))

venn.diagram(
  x = prev_28months,
  category.names = c("Diarrhea" , "ARI" , "Fever"), 
  filename = here::here("figures", "mediators_venn_diagram_28mo.png"))


