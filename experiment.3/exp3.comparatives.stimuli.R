# set up ------------------------------------------------------------------
#rm(list = ls())
library(tidyverse)
library(webr)
library(rstatix)
library(ggstatsplot)
setwd("/home/eli/Desktop/Neurobiologia/projects/ongoing/verbal.irony/facilitators/experiment.3/")

# data --------------------------------------------------------------------
experiment.3 <- read_csv("general/exp3.paralinguistic.full_stimuli_data.csv") %>% 
  drop_na(stimuli) %>% 
  mutate_at(c(1:5,8:15), as.factor)

# descriptives ------------------------------------------------------------
descriptives.score <- experiment.3 %>% 
  group_by(expName, condition) %>% 
  numSummary(score) %>% 
  mutate(vars = "score") 

descriptives.condition.vars.correctans <- experiment.3 %>% 
  dplyr::group_by(expName,condition) %>% 
  dplyr::filter(score == 1) %>% 
  numSummary(rt) %>% 
  mutate(vars = "rt") %>% 
  full_join(descriptives.score) %>% 
  write_csv(., "results/descriptives.condition_correctans.csv")

descriptives.stimuli.score <- experiment.3 %>% 
  dplyr::group_by(expName,condition,stimuli) %>% 
  numSummary(score) %>%
  mutate(mean = round(mean *100,0), sd = round(sd *100,0), vars = "score") 

descriptives.stimuli.vars <- experiment.3 %>% 
  filter(score == 1) %>% 
  dplyr::group_by(expName,condition,stimuli) %>% 
  numSummary(rt) %>%
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
  mutate(expName = factor(expName, levels = c("Contextual discrepancy","Prosody", "Facial expression"))) %>%
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
  ylab("Mean+DE") +
  theme(text = element_text(size = 25),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_flip() +
  scale_y_continuous(n.breaks = 6)
ggsave("plot/error.bar.plot.stimuli.exp3.jpg", width = 11, height = 8)

descriptives.stimuli.score %>% 
  mutate(expName = factor(expName, levels = c("Contextual discrepancy","Facial expression","Prosody"))) %>%
  grouped_ggbetweenstats(expName, mean, grouping.var = "condition",  
                         p.adjust.method = "bonferroni",
                         bf.message = F, results.subtitle = F,
                         ylab = "Score",
                         xlab = "Condition")
ggsave("plot/exp3.violin.condiciones.jpg", width = 9, height = 5)

experiment.3 %>% 
  grouped_ggpiestats(x = condition, y = answer, grouping.var = "expName", 
           bf.message = F, type = "parametric", paired = F,
           package      = "wesanderson",
           palette      = "Royal1",
           p.adjust.methods = "bonferroni", 
           results.subtitle = F,
           pairwaise.comparisons = F,
           label.repel  = TRUE)
ggsave("plot/exp3.pieplot.answer.jpg", width = 30, height = 4)
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp3.pieplot.answer.jpg", width = 30, height = 4)

# spanish plots -----------------------------------------------------------
descriptives.stimuli.score %>% 
  mutate(condition = recode(condition, "Irony" = "Ironía", 
                            "Unrelated" = "Sin relación")) %>%
  mutate(expName = factor(expName, levels = c("Contextual discrepancy","Facial expression","Prosody"))) %>%
  mutate(expName = recode(expName,"Contextual discrepancy" =  "Discrepancia contextual", "Facial expression" = "Expresión facial", 
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
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/error.bar.plot.stimuli.exp3.jpg", width = 11, height = 8)

descriptives.stimuli.score %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Ironic"  = "a. Ironía","Literal" = "b. Literal" , "Unrelated" = "c. Sin relación")) %>%
  mutate(expName = factor(expName, levels = c("Contextual discrepancy","Facial expression","Prosody"))) %>%
  mutate(expName = recode(expName,"Contextual discrepancy" =  "Discrepancia contextual", "Facial expression" = "Expresión facial", 
                          "Prosody" = "Prosodia")) %>%
  grouped_ggbetweenstats(expName, mean, grouping.var = "condition",  
                         p.adjust.method = "bonferroni",
                         bf.message = F, results.subtitle = F,
                         ylab = "Porcentaje de clasificación",
                         xlab = "Condición")
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp3.violin.condiciones.score.spanish.jpg", width = 13, height = 5)

descriptives.stimuli.vars %>% 
  dplyr::filter(vars == "rt") %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Ironic"  = "a. Ironía","Literal" = "b. Literal" , "Unrelated" = "c. Sin relación")) %>%
  mutate(expName = factor(expName, levels = c("Contextual discrepancy","Facial expression","Prosody"))) %>%
  mutate(expName = recode(expName,"Contextual discrepancy" =  "Discrepancia contextual", "Facial expression" = "Expresión facial", 
                          "Prosody" = "Prosodia")) %>%
  grouped_ggbetweenstats(expName, mean, grouping.var = "condition",  
                         p.adjust.method = "bonferroni",
                         bf.message = F, results.subtitle = F,
                         ylab = "Tiempo de clasificación",
                         xlab = "Condición")
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp3.violin.condiciones.rt.spanish.jpg", width = 13, height = 5)

descriptives.stimuli.score %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Ironic"  = "Ironía","Literal" = "Literal" , "Unrelated" = "Sin relación")) %>%
  mutate(expName = factor(expName, levels = c("Contextual discrepancy","Facial expression","Prosody"))) %>%
  mutate(expName = recode(expName,"Contextual discrepancy" =  "a. Discrepancia contextual", "Facial expression" = "b. Expresión facial", 
                          "Prosody" = "c. Prosodia")) %>%
  grouped_ggbetweenstats(condition, mean, grouping.var = "expName",  
                         p.adjust.method = "bonferroni",
                         bf.message = F, results.subtitle = F,
                         ylab = "Porcentaje de clasificación")
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp3.violin.expName.spanish.jpg", width = 12, height = 5)
