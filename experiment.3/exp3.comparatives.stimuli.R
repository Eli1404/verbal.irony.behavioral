# set up ------------------------------------------------------------------
#rm(list = ls())
library(tidyverse)
library(webr)
library(rstatix)
library(ggstatsplot)
library(ggpubr)
setwd("/home/eli/Desktop/Neurobiologia/projects/ongoing/verbal.irony/facilitators/experiment.3/")

# data --------------------------------------------------------------------
experiment.3 <- read_csv("general/exp3.paralinguistic.full_stimuli_data.csv") %>% 
  drop_na(stimuli) %>% 
  mutate_at(c(1:5,8:15), as.factor)

# descriptives ------------------------------------------------------------
descriptives.score <- experiment.3 %>% 
  group_by(expName, condition) %>% 
  numSummary(score) %>% 
  mutate(mean = round(mean *100,0), sd = round(sd *100,0), vars = "score") 

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

# Friedman ----------------------------------------------------------------
Friedman.fe <- experiment.3 %>%
  dplyr::select(3,6,9,8) %>% 
  filter(expName == "Facial expression") %>% 
  group_by(condition, id) %>% 
  dplyr::summarise(score = mean(score, na.rm = T)) %>% 
  ungroup() %>% 
  friedman_test(score ~ condition | id) %>% 
  adjust_pvalue(method = "bonferroni") %>% 
  mutate(p.adj = round(p.adj,3)) 


fridman <- experiment.3.by.participant %>% 
  gather(c(4,6,8), key = "variables", value = "mean") %>% 
  separate(variables, into = c("expName", "condition")) %>% 
  friedman_test(mean ~ condition | id) %>% 
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") %>% 
  mutate(p.adj = round(p.adj, 3))

fridman.fe <- experiment.3.by.participant %>% 
  gather(c(4,6,8,10,12,14,16,18,20), key = "variables", value = "mean") %>% 
  separate(variables, into = c("expName", "condition")) %>%
  #  group_by(condition) %>% 
  dplyr::filter(condition == "Unrelated") %>% 
  dplyr::filter(id != "lilia.sarai943@gmail.com") %>% 
  friedman_test(mean ~ expName | id) %>% 
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") %>% 
  mutate(p.adj = round(p.adj, 3)) 

# welch anova -------------------------------------------------------------
anova.test.condition <- descriptives.stimuli.vars %>% 
  group_by(vars) %>% 
  welch_anova_test(mean ~ condition) %>% 
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") %>% 
  mutate(p.adj = round(p.adj, 3)) %>% 
  write_csv(.,"results/anova-welch.csv")

fridman.context <- experiment.3.by.participant %>% 
  gather(c(4,6,8), key = "variables", value = "mean") %>% 
  separate(variables, into = c("expName", "condition")) %>% 
  friedman_test(mean ~ condition | id) %>% 
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") %>% 
  mutate(p.adj = round(p.adj, 3))

fridman.fe <- experiment.3.by.participant %>% 
  gather(c(4,6,8,10,12,14,16,18,20), key = "variables", value = "mean") %>% 
  separate(variables, into = c("expName", "condition")) %>%
#  group_by(condition) %>% 
  dplyr::filter(condition == "Unrelated") %>% 
  dplyr::filter(id != "lilia.sarai943@gmail.com") %>% 
  friedman_test(mean ~ expName | id) %>% 
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") %>% 
  mutate(p.adj = round(p.adj, 3)) 

games.test.accuaracy <- descriptives.stimuli.vars %>% 
  group_by(vars) %>%
  games_howell_test(mean ~ condition) %>%
  adjust_pvalue(method = "bonferroni") %>%
  mutate(p.adj = round(p.adj,3)) %>% 
  arrange(p.adj) %>% 
  write_csv(.,"results/games-test.csv")

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

# ggwithinstats score -----------------------------------------------------------
ggstat.score.irony <- experiment.3 %>% 
  dplyr::select(3,6,8,9) %>% 
  filter(condition == "Ironic") %>% 
  group_by(expName,id) %>% 
  dplyr::summarise(score = mean(score, na.rm = T)*100) %>% 
  ungroup()  %>% 
  mutate(expName = factor(expName, levels = c("Contextual discrepancy","Facial expression","Prosody"))) %>%
  mutate(expName = recode(expName,"Contextual discrepancy" =  "Discrepancia contextual", "Facial expression" = "Expresión facial", 
                          "Prosody" = "Prosodia")) %>%
  ggwithinstats(expName, score,
                p.adjust.method = "bonferroni",
                bf.message = F, 
                results.subtitle = T,
                type = "np",
                centrality.path = F,
                ylab = "Porcentaje de clasificación",
                xlab = "Condición",
                title = "a. Ironía")

ggstat.score.literal <- experiment.3 %>% 
  dplyr::select(3,6,8,9) %>% 
  filter(condition == "Literal") %>% 
  group_by(expName,id) %>% 
  dplyr::summarise(score = mean(score, na.rm = T)*100) %>% 
  ungroup()  %>% 0
  mutate(expName = factor(expName, levels = c("Contextual discrepancy","Facial expression","Prosody"))) %>%
  mutate(expName = recode(expName,"Contextual discrepancy" =  "Discrepancia contextual", "Facial expression" = "Expresión facial", 
                          "Prosody" = "Prosodia")) %>%
  ggwithinstats(expName, score,
                p.adjust.method = "bonferroni",
                bf.message = F, 
                results.subtitle = T,
                type = "np",
                centrality.path = F,
                ylab = "Porcentaje de clasificación",
                xlab = "Condición",
                title = "b. Literal")

ggstat.score.unrelated <- experiment.3 %>% 
  dplyr::select(3,6,8,9) %>% 
  filter(condition == "Unrelated") %>% 
  group_by(expName,id) %>% 
  dplyr::summarise(score = mean(score, na.rm = T)*100) %>% 
  ungroup()  %>% 
  mutate(expName = factor(expName, levels = c("Contextual discrepancy","Facial expression","Prosody"))) %>%
  mutate(expName = recode(expName,"Contextual discrepancy" =  "Discrepancia contextual", "Facial expression" = "Expresión facial", 
                          "Prosody" = "Prosodia")) %>%
  ggwithinstats(expName, score,
                p.adjust.method = "bonferroni",
                bf.message = F, 
                results.subtitle = T,
                type = "np",
                centrality.path = F,
                ylab = "Porcentaje de clasificación",
                xlab = "Condición",
                title = "c. Sin relación")
ggarrange(ggstat.score.irony, ggstat.score.literal, ggstat.score.unrelated, ncol = 3)
ggsave("plot/exp3.violin.condiciones.jpg", width = 20, height = 6)
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp3.violin.condiciones.jpg", width = 20, height = 6)

# ggwithinstats rt -----------------------------------------------------------
ggstat.score.irony.rt <- experiment.3 %>% 
  dplyr::select(3,7,8,9) %>% 
  filter(condition == "Ironic") %>% 
  group_by(expName,id) %>% 
  dplyr::summarise(rt = mean(rt, na.rm = T)) %>% 
  ungroup()  %>% 
  mutate(expName = factor(expName, levels = c("Contextual discrepancy","Facial expression","Prosody"))) %>%
  mutate(expName = recode(expName,"Contextual discrepancy" =  "Discrepancia contextual", "Facial expression" = "Expresión facial", 
                          "Prosody" = "Prosodia")) %>%
  ggwithinstats(expName, rt,
                p.adjust.method = "bonferroni",
                bf.message = F, 
                results.subtitle = T,
                type = "np",
                centrality.path = F,
                ylab = "Tiempo de clasificación",
                xlab = "Condición",
                title = "a. Ironía")

ggstat.score.literal.rt <- experiment.3 %>% 
  dplyr::select(3,7,8,9) %>% 
  filter(condition == "Literal") %>% 
  group_by(expName,id) %>% 
  dplyr::summarise(rt = mean(rt, na.rm = T)) %>% 
  ungroup()  %>% 
  mutate(expName = factor(expName, levels = c("Contextual discrepancy","Facial expression","Prosody"))) %>%
  mutate(expName = recode(expName,"Contextual discrepancy" =  "Discrepancia contextual", "Facial expression" = "Expresión facial", 
                          "Prosody" = "Prosodia")) %>%
  ggwithinstats(expName, rt,
                p.adjust.method = "bonferroni",
                bf.message = F, 
                results.subtitle = T,
                type = "np",
                centrality.path = F,
                ylab = "Tiempo de clasificación",
                xlab = "Condición",
                title = "b. Literal")

ggstat.score.unrelated.rt <- experiment.3 %>% 
  dplyr::select(3,7,8,9) %>% 
  filter(condition == "Unrelated") %>% 
  group_by(expName,id) %>% 
  dplyr::summarise(rt = mean(rt, na.rm = T)) %>% 
  ungroup()  %>% 
  mutate(expName = factor(expName, levels = c("Contextual discrepancy","Facial expression","Prosody"))) %>%
  mutate(expName = recode(expName,"Contextual discrepancy" =  "Discrepancia contextual", "Facial expression" = "Expresión facial", 
                          "Prosody" = "Prosodia")) %>%
  ggwithinstats(expName, rt,
                p.adjust.method = "bonferroni",
                bf.message = F, 
                results.subtitle = T,
                type = "np",
                centrality.path = F,
                ylab = "Tiempo de clasificación",
                xlab = "Condición",
                title = "c. Sin relación")
ggpubr::ggarrange(ggstat.score.irony.rt, ggstat.score.literal.rt, ggstat.score.unrelated.rt, ncol = 3)
ggsave("plot/exp3.violin.condiciones.rt.jpg", width = 20, height = 6)
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp3.violin.condiciones.rt.jpg", width = 20, height = 6)

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
