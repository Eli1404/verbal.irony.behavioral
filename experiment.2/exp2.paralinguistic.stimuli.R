# set up ------------------------------------------------------------------
#rm(list = ls())
library(tidyverse)
library(webr)
library(rstatix)
library(ggstatsplot)
setwd("/home/eli/Desktop/Neurobiologia/projects/ongoing/verbal.irony/facilitators/experiment.2/")

# data --------------------------------------------------------------------
experiment.2 <- read_csv("general/exp2.paralinguistic.full_stimuli_data.csv") %>% 
  drop_na(stimuli) %>% 
  mutate(expName = fct_relevel(expName, "Prosody", "Facial expression"))

# descriptives ------------------------------------------------------------
descriptives.score <- experiment.2 %>% 
  dplyr::group_by(expName,condition) %>% 
  numSummary(score) %>% 
  mutate(vars = "score")

descriptives.condition.vars.correctans <- experiment.2 %>% 
  dplyr::group_by(expName,condition) %>% 
  dplyr::filter(score == 1) %>% 
  numSummary(rt) %>% 
  mutate(vars = "rt") %>% 
  full_join(descriptives.score) %>% 
  write_csv(., "results/descriptives.condition_correctans.csv")

descriptives.stimuli.score <- experiment.2 %>% 
  dplyr::group_by(expName,condition,stimuli) %>% 
  numSummary(score) %>%
  mutate(mean = round(mean *100,0), sd = round(sd *100,0), vars = "score") 

descriptives.stimuli.vars <- experiment.2 %>% 
  filter(score == 1) %>% 
  dplyr::group_by(expName,condition,stimuli) %>% 
  numSummary() %>%
  mutate(vars = "rt") %>% 
  full_join(descriptives.stimuli.score) %>% 
  arrange(vars) %>% 
  write_csv(.,"results/descriptives.condition.phrase.csv")

# outliers ----------------------------------------------------------------
outliers.general <- descriptives.stimuli.vars %>% 
  select(vars, condition, mean) %>% 
  dplyr::group_by(vars, condition) %>% 
  identify_outliers(mean) %>% 
  write_csv(., "results/outliers.csv")

# normality ---------------------------------------------------------------
shapiro.general <- descriptives.stimuli.vars %>% 
  select(vars, condition, mean) %>% 
  dplyr::group_by(vars, condition) %>% 
  shapiro_test(mean) %>% 
  dplyr::mutate(p = round(p,3)) %>% 
  write_csv(., "results/shapiro.csv")

# homogeneity of variances ------------------------------------------------
homogeneity.condition <- descriptives.stimuli.vars %>% 
  select(vars, condition, mean) %>% 
  mutate_at(1:3, as.factor) %>% 
  dplyr::group_by(vars) %>% 
  levene_test(mean ~ condition) %>%
  dplyr::mutate(p = round(p,4)) 

# plot -------------------------------------------------------------------
descriptives.stimuli.score %>% 
  mutate(condition = recode(condition, "Irony" = "Ironic")) %>% 
  mutate(expName = factor(expName, levels = c("Prosody", "Facial expression"))) %>%
  filter(condition != "") %>% 
  filter(expName != "") %>%
  ggplot(aes(x= stimuli, y = mean, 
             ymax = (mean + sd), 
             ymin = (mean - sd))) + 
  theme_bw() +
  facet_grid(expName~condition) +
  geom_point(pch=1, fill = "black") +
  geom_errorbar(width=0.6) +
  geom_hline(yintercept = 65,color = "grey", alpha = 0.6) +
  xlab("Item") +
  ylab("Mean+SD") +
  theme(text = element_text(size = 25),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_flip() +
  scale_y_continuous(n.breaks = 6)
ggsave("plots/error.bar.plot.stimuli.exp2.jpg", width = 11, height = 8)

experiment.2 %>% 
  grouped_ggpiestats(x = condition, y = answer, grouping.var = "expName", 
                     bf.message = F, type = "parametric", paired = F,
                     package      = "wesanderson",
                     palette      = "Royal1",
                     label.repel  = TRUE,
                     p.adjust.methods = "bonferroni", 
                     results.subtitle = F,
                     pairwaise.comparisons = F)
ggsave("plots/exp2.pieplot.answer.jpg", width = 20, height = 4)

# spanish plots -----------------------------------------------------------
descriptives.stimuli.score %>% 
  mutate(condition = recode(condition, "Irony" = "Ironía", 
                            "Unrelated" = "Sin relación")) %>%
  mutate(expName = factor(expName, levels = c("Facial expression","Prosody"))) %>%
  mutate(expName = recode(expName, "Facial expression" = "Expresión facial", 
                            "Prosody" = "Prosodia")) %>%
  filter(condition != "") %>% 
  filter(expName != "") %>%
  ggplot(aes(x= stimuli, y = mean, 
             ymax = (mean + sd), 
             ymin = (mean - sd))) + 
  theme_bw() +
  facet_grid(expName~condition) +
  geom_point(pch=1, fill = "black") +
  geom_errorbar(width=0.6) +
  geom_hline(yintercept = 65,color = "grey", alpha = 0.6) +
  xlab("Estímulo") +
  ylab("Media + DE") +
  theme(text = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_flip() +
  scale_y_continuous(n.breaks = 6)
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/error.bar.plot.stimuli.exp2.jpg", width = 11, height = 8)

descriptives.stimuli.score %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Irony"  = "Ironía", "Unrelated" = "Sin relación")) %>%
  dplyr::mutate(expName = dplyr::recode(expName,"Prosody"  = "b. Prosodia",
                                          "Facial expression" = "a. Expresión facial")) %>%
  mutate(expName = factor(expName, levels = c("a. Expresión facial", "b. Prosodia"))) %>% 
  grouped_ggbetweenstats(condition, mean, grouping.var = "expName",  
                 p.adjust.method = "bonferroni",
                 bf.message = F, results.subtitle = F,
                 ylab = "Porcentaje de clasificación",
                 xlab = "Condición")
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp2.violin.condiciones.spanish.jpg", width = 9, height = 5)
ggsave("plots/exp2.violin.condiciones.spanish.jpg", width = 9, height = 5)
