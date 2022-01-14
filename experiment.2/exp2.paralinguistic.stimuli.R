# set up ------------------------------------------------------------------
#rm(list = ls())
library(tidyverse)
library(webr)
library(rstatix)
library(ggstatsplot)
library(ggpubr)
setwd("/home/eli/Desktop/Neurobiologia/projects/ongoing/verbal.irony/facilitators/experiment.2/")

# data --------------------------------------------------------------------
experiment.2 <- read_csv("general/exp2.paralinguistic.full_stimuli_data.filter_selected.csv") %>% 
  drop_na(stimuli) %>% 
  mutate(expName = fct_relevel(expName, "Prosody", "Facial expression"))

# descriptives ------------------------------------------------------------
descriptives.stimuli.score <- experiment.2 %>% 
  dplyr::group_by(expName,condition,stimuli) %>% 
  numSummary(score) %>%
  mutate(mean = round(mean *100,0), sd = round(sd *100,0), vars = "score")

descriptives.stimuli.vars <- experiment.2 %>% 
  filter(score == 1) %>% 
  select(3,5,8,9) %>% 
  dplyr::group_by(expName,condition,stimuli) %>% 
  numSummary() %>%
  mutate(vars = "rt") %>% 
  full_join(descriptives.stimuli.score) %>% 
  arrange(vars) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(1,2,3,16,4:15) %>% 
  write_csv(.,"results/descriptives.condition.phrase.csv")

descriptives.condition.vars.correctans <- descriptives.stimuli.vars %>%
  ungroup() %>% 
  select(1,2,4,6) %>% 
  dplyr::group_by(expName,condition,vars) %>% 
  numSummary() %>% 
  mutate_if(is.numeric, round, 2) %>% 
  write_csv(., "results/descriptives.condition_correctans.csv")

# outliers ----------------------------------------------------------------
outliers.general <- descriptives.stimuli.vars %>% 
  select(vars, condition, mean) %>% 
  dplyr::group_by(vars, condition) %>% 
  identify_outliers(mean) %>% 
  write_csv(., "results/outliers.csv")

ggplot(descriptives.stimuli.score,aes(sample = mean,color = condition)) +
  stat_qq()+
  stat_qq_line()+
  facet_wrap(~expName)

# normality ---------------------------------------------------------------
shapiro.general <- descriptives.stimuli.vars %>% 
  filter(vars == "score") %>% 
  select(expName, condition, mean) %>% 
  ungroup() %>% 
  dplyr::group_by(expName, condition) %>% 
  shapiro_test(mean) %>% 
  dplyr::mutate(p = round(p,3)) %>% 
  write_csv(., "results/shapiro.csv")

# homogeneity of variances ------------------------------------------------
homogeneity.condition <- descriptives.stimuli.vars %>% 
  dplyr::select(expName, vars, condition, mean) %>% 
  mutate_at(1:3, as.factor) %>% 
  dplyr::group_by(expName,vars) %>% 
  rstatix::levene_test(mean ~ condition) %>%
  dplyr::mutate(p = round(p,4))  %>% 
  write_csv(., "results/homogenity.csv")

# ggwithinstat-Friedman-Durbin ------------------------------------------------------------
ggstat.score.FE <- experiment.2.filted_selected %>%
  dplyr::select(2,5,7,8) %>% 
  filter(expName == "Facial expression") %>% 
  group_by(condition, id) %>% 
  dplyr::summarise(score = mean(score, na.rm = T)*100) %>% 
  ungroup()  %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Irony"  = "Ironía", "Unrelated" = "Sin relación")) %>%
  ggwithinstats(condition, score,
                p.adjust.method = "bonferroni",
                type = "np",
                bf.message = F, 
                results.subtitle = T,
                centrality.path = F,
                labs(title = "a. Expresión facial"),
                ylab = "Porcentaje de clasificación",
                xlab = "Condición")

ggstat.score.Prosody <- experiment.2.filted_selected %>%
  dplyr::select(2,5,7,8) %>% 
  filter(expName == "Prosody") %>% 
  group_by(condition, id) %>% 
  dplyr::summarise(score = mean(score, na.rm = T)*100) %>% 
  ungroup()  %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Irony"  = "Ironía", "Unrelated" = "Sin relación")) %>%
  ggwithinstats(condition, score,
                p.adjust.method = "bonferroni",
                bf.message = F, 
                results.subtitle = T,
                type = "np",
                centrality.path = F,
                ylab = "Porcentaje de clasificación",
                xlab = "Condición")
ggarrange(ggstat.score.FE, ggstat.score.Prosody)
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp2.violin.condiciones.spanish.jpg", width = 15, height = 5)

# ggwithinstat rt-Friedman-Durbin------------------------------------------------------------
ggstat.score.FE.rt <- experiment.2.filted_selected %>%
  dplyr::select(3,5,7,8) %>% 
  filter(expName == "Facial expression") %>% 
  group_by(condition, id) %>% 
  dplyr::summarise(rt = mean(rt, na.rm = T)) %>% 
  ungroup()  %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Irony"  = "Ironía", "Unrelated" = "Sin relación")) %>%
  ggwithinstats(condition, rt,
                p.adjust.method = "bonferroni",
                type = "np",
                bf.message = F, 
                results.subtitle = T,
                centrality.path = F,
                labs(title = "a. Expresión facial"),
                ylab = "Tiempo de clasificación",
                xlab = "Condición")

ggstat.score.Prosody.rt <- experiment.2.filted_selected %>%
  dplyr::select(3,5,7,8) %>% 
  filter(expName == "Prosody") %>% 
  group_by(condition, id) %>% 
  dplyr::summarise(rt = mean(rt, na.rm = T)) %>% 
  ungroup()  %>%
  dplyr::mutate(condition = dplyr::recode(condition,"Irony"  = "Ironía", "Unrelated" = "Sin relación")) %>%
  ggwithinstats(condition, rt,
                p.adjust.method = "bonferroni",
                bf.message = F, 
                results.subtitle = T,
                type = "np",
                centrality.path = F,
                ylab = "Tiempo de clasificación",
                xlab = "Condición")
ggarrange(ggstat.score.FE.rt, ggstat.score.Prosody.rt)
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp2.violin.condiciones.rt.spanish.jpg", width = 15, height = 5)

# plot -------------------------------------------------------------------
read_csv("general/exp2.paralinguistic.full_stimuli_data.csv") %>% 
  dplyr::group_by(expName,condition,stimuli) %>% 
  dplyr::mutate(mean = round(mean(score)*100,2), sd = round(sd(score)*100,2)) %>% 
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
  xlab("Stimuli") +
  ylab("Mean + SD") +
  theme(text = element_text(size = 14),
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
read_csv("general/exp2.paralinguistic.full_stimuli_data.csv") %>%   dplyr::group_by(expName,condition,stimuli) %>% 
  mutate(mean = round(mean(score)*100,2), sd = round(sd(score)*100,2)) %>% 
  mutate(condition = dplyr::recode(condition, "Irony" = "Ironía", 
                                   "Unrelated" = "Sin relación")) %>%
  mutate(expName = factor(expName, levels = c("Facial expression","Prosody"))) %>%
  mutate(expName = dplyr::recode(expName, "Facial expression" = "Expresión facial", 
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

# pie plot ---------------------------------------------------------------
experiment.2 %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Irony"  = "Ironía", "Unrelated" = "Sin relación")) %>%
  dplyr::mutate(expName = dplyr::recode(expName,"Prosody"  = "b. Prosodia",
                                        "Facial expression" = "a. Expresión facial")) %>%
  mutate(expName = factor(expName, levels = c("a. Expresión facial", "b. Prosodia"))) %>% 
  grouped_ggpiestats(x = condition, y = answer, grouping.var = "expName", 
                     bf.message = F, 
                     type = "np", paired = F,
                     package      = "wesanderson",
                     palette      = "Royal1",
                     label.repel  = TRUE,
                     p.adjust.methods = "bonferroni", 
                     results.subtitle = F,
                     pairwaise.comparisons = F)
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp2.pieplot.answer.jpg", width = 20, height = 4)

# alpha ------------------------------------------------------------------
# alpha: medida de la consistencia interna de una escala, 
# que sus ítems apuntan en la misma dirección
library(psych)
borrar <- experiment.2.filted_selected %>% 
  dplyr::filter(expName == "Prosody") %>% 
  dplyr::filter(condition == "Literal") %>% 
  dplyr::select(2,7,9,10) %>% 
#  mutate_at(1:3, as.factor) %>% 
  unite("stimuli", 3:4) %>% 
  spread(stimuli, score) %>%
  filter(id != "samrojo19@gmail.com") %>% 
  dplyr::select(-1)

alfa <- alpha(borrar)
alfa$total
15


