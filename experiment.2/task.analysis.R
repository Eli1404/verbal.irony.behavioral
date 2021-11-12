# setup -------------------------------------------------------------------
library(corrplot)
library(tidyverse)
library(data.table)
library(ggpubr)
library(rstatix)
library(webr)
library(ggstatsplot)
library(Hmisc)
setwd("/home/eli/Desktop/Neurobiologia/projects/ongoing/verbal.irony/facilitators/experiment.2/")

# data --------------------------------------------------------------------
results.participants <- read_csv("general/Results.by.participants.csv") 

# descriptives ------------------------------------------------------------
statistics.tasks <- results.participants %>% 
  select(2,7:27) %>%
  numSummary %>% 
  write_csv(., "results/descriptives.tasks.csv")

statistics.tasks.sex <- results.participants %>% 
  select(2,3,7:27) %>%
  dplyr::group_by(sex) %>% 
  numSummary %>% 
  write_csv(., "results/descriptives.tasks.sex.csv")

statistics.tasks.degree <- results.participants %>% 
  select(2,3,5,7:27) %>%
  dplyr::group_by(`type of degree`) %>% 
  numSummary () %>% 
  write_csv(., "results/descriptives.tasks.degree.csv")

# outliers ----------------------------------------------------------------
outliers.general <- results.participants %>%  
  dplyr::select(7,9,11,13,15,17) %>%
  gather(1:6, key = "variable", value = "value") %>% 
  separate(variable, into = c("expName", "condition")) %>% 
  drop_na(value) %>% 
  dplyr::group_by(expName, condition) %>% 
  identify_outliers(value) %>% 
  write_csv(., "results/outliers.csv")

# normality ---------------------------------------------------------------
shapiro.general <- results.participants %>%  
  dplyr::select(3,5,7:18) %>% 
  gather(3:14, key = "variable", value = "values") %>% 
  separate(variable, into = c("expName", "condition","statistics")) %>% 
  mutate_at(1:5, as.factor) %>% 
  group_by(expName, statistics, condition) %>% 
  shapiro_test(values) %>% 
  mutate(statistic = round(statistic, 3), p = round(p, 3)) %>% 
  write_csv(., "results/shapiro.csv")

# homogeneity of variances ------------------------------------------------
homogeneity.condition <- results.participants %>%  
  dplyr::select(3,5,7:18) %>% 
  gather(3:14, key = "variable", value = "value") %>% 
  separate(variable, into = c("expName", "condition","statistics")) %>% 
  drop_na(value) %>% 
  mutate_at(1:5, as.factor) %>% 
  select(-1,-2) %>% 
  group_by(statistics,expName) %>% 
  levene_test(value ~ condition) %>% 
  write_csv(., "results/levene.test.csv")

# comparatives no parametric ----------------------------------------------
Friedman_fe <- results.participants %>%
  dplyr::select(1,3,5,7,9,11) %>% 
  gather(4:6, key = "variable", value = "value") %>% 
  separate(variable, into = c("expName", "condition")) %>%
  mutate_at(1:5, as.factor) %>% 
  friedman_test(value ~ condition | id) %>% 
  adjust_pvalue(method = "bonferroni") %>% 
  mutate(p.adj = round(p.adj,3)) %>% 
  write_csv(., "results/Friedman.test.fe.csv")

Friedman_prosody <- results.participants %>%  
  dplyr::select(1,3,5,13,15,17) %>% 
  gather(4:6, key = "variable", value = "value") %>% 
  separate(variable, into = c("expName", "condition")) %>%
  friedman_test(value ~ condition | id) %>% 
  adjust_pvalue(method = "bonferroni") %>% 
  mutate(p.adj = round(p.adj,3)) %>% 
  write_csv(., "results/Friedman.test.prosody.csv")

Friedman_fe.rt <- results.participants %>%  
  dplyr::select(1,3,5,8,10,12) %>% 
  gather(4:6, key = "variable", value = "value") %>% 
  separate(variable, into = c("expName", "condition")) %>%
  mutate_at(1:5, as.factor) %>% 
  friedman_test(value ~ condition | id) %>% 
  adjust_pvalue(method = "bonferroni") %>% 
  mutate(p.adj = round(p.adj,3)) %>% 
  write_csv(., "results/Friedman.test.fe.rt.csv")

Friedman_prosody.rt <- results.participants %>%  
  dplyr::select(1,3,5,14,16,18) %>% 
  gather(4:6, key = "variable", value = "value") %>% 
  separate(variable, into = c("expName", "condition")) %>%
  friedman_test(value ~ condition | id) %>% 
  adjust_pvalue(method = "bonferroni") %>% 
  mutate(p.adj = round(p.adj,3)) %>% 
  write_csv(., "results/Friedman.test.prosody.rt.csv")

Mann_context.prosody <- results.participants %>%  
  dplyr::select(1,3,5,13,15,17) %>% 
  gather(4:6, key = "variable", value = "value") %>% 
  separate(variable, into = c("expName", "condition")) %>%
  mutate_at(1:5, as.factor) %>% 
  wilcox_test(value ~ condition, paired = T) %>% 
  mutate(p.adj = round(p.adj,3)) %>% 
  write_csv(., "results/Mann.test.prosody.csv")

Mann_context.fe <- results.participants %>%  
  dplyr::select(1,3,5,7,9,11) %>% 
  gather(4:6, key = "variable", value = "value") %>% 
  separate(variable, into = c("expName", "condition")) %>%
  mutate_at(1:5, as.factor) %>% 
  wilcox_test(value ~ condition, paired = T) %>% 
  mutate(p.adj = round(p.adj,3)) %>% 
  write_csv(., "results/Mann.test.fe.csv")

Mann_context.prosody.rt <- results.participants %>%  
  dplyr::select(1,3,5,14,16,18) %>% 
  gather(4:6, key = "variable", value = "value") %>% 
  separate(variable, into = c("expName", "condition")) %>%
  mutate_at(1:5, as.factor) %>% 
  wilcox_test(value ~ condition, paired = T) %>% 
  mutate(p.adj = round(p.adj,3)) %>% 
  write_csv(., "results/Mann.test.prosody.rt.csv")

Mann_context.fe.rt <- results.participants %>%  
  dplyr::select(1,3,5,8,10,12) %>% 
  gather(4:6, key = "variable", value = "value") %>% 
  separate(variable, into = c("expName", "condition")) %>%
  mutate_at(1:5, as.factor) %>% 
  wilcox_test(value ~ condition, paired = T) %>% 
  mutate(p.adj = round(p.adj,3)) %>% 
  write_csv(., "results/Mann.test.fe.rt.csv")

# inferential -------------------------------------------------------------
correlation.psychometric <- results.participants %>% 
  dplyr::select(7:27) %>% 
  cor_mat() %>% 
  cor_gather() %>% 
  adjust_pvalue(method = "fdr") %>% 
  dplyr::mutate(p.adj = round(p.adj, 3)) %>% 
  filter(p.adj <=0.05 | cor == 1) %>% 
  arrange(cor) %>% 
  write_csv(.,"results/correlations.csv")