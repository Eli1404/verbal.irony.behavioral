# setup -------------------------------------------------------------------
library(tidyverse)
library(rstatix)
library(webr)
library(ggstatsplot)
setwd("~/Desktop/Neurobiologia/projects/ongoing/verbal.irony/discrepancy/experiment.1/")

# reading data ------------------------------------------------------------
experiment.1 <- read_csv("general/exp1.context.full_stimuli_data.csv") 

# descriptives ------------------------------------------------------------
descriptives.stimuli.score <- experiment.1 %>% 
  dplyr::group_by(stimuli,condition) %>%
  numSummary(score) %>% 
  dplyr::mutate(mean = mean*100, sd = sd*100) %>% 
  mutate(vars = "score")

descriptives.stimuli.rt <- experiment.1 %>% 
  dplyr::group_by(stimuli,condition) %>%
  filter(score == 1) %>% 
  numSummary(rt) %>% 
  mutate(vars = "rt")

descriptives.stimuli.vars <- experiment.1 %>% 
  dplyr::select(2,5,6,10) %>% 
  dplyr::group_by(condition, stimuli) %>% 
  numSummary() %>%
  full_join(descriptives.stimuli.score) %>% 
  full_join(descriptives.stimuli.rt) %>% 
  mutate_if(is.numeric, round,2) %>% 
  arrange(vars) %>% 
  write_csv(.,"results/descriptives.condition.phrase.csv")

descriptives.condition.vars.correctans <- descriptives.stimuli.vars %>% 
  ungroup() %>% 
  select(1,3,5) %>% 
  dplyr::group_by(vars, condition) %>%
  numSummary() %>%
  mutate_if(is.numeric, round, 2) %>% 
  write_csv(.,"results/descriptives.condition_correctans.csv")

# outliers ----------------------------------------------------------------
outliers.general <- descriptives.stimuli.vars %>% 
  select(vars, condition, mean) %>% 
  dplyr::group_by(vars, condition) %>% 
  identify_outliers(mean) %>% 
  write_csv(., "results/outliers.csv")

# normality ---------------------------------------------------------------
shapiro.general <- descriptives.stimuli.vars %>% 
  select(vars, condition, mean) %>% 
  mutate(mean = log10(mean)) %>% 
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

# ggwithinstats-Friedman  -------------------------------------------------
ggstat.score <- experiment.1 %>% 
  mutate_at(1:2, as.factor) %>% 
  group_by(condition, participant) %>% 
  dplyr::summarise(score = mean(score*100)) %>% 
  ungroup() %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Ironic"  = "Ironía", "Unrelated" = "Sin relación", 
                                          "White lie" = "Mentira")) %>%
  ggwithinstats(condition, score,
                p.adjust.method = "bonferroni",
                bf.message = F, 
                results.subtitle = F,
                type = "np",
                centrality.path = F,
                ylab = "Porcentaje de clasificación",
                xlab = "Condición",
                title = "a")

ggstat.rt <- experiment.1 %>% 
  mutate_at(1:2, as.factor) %>% 
  group_by(condition, participant) %>% 
  dplyr::summarise(rt = mean(rt)) %>% 
  ungroup() %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Ironic"  = "Ironía", "Unrelated" = "Sin relación", 
                                          "White lie" = "Mentira")) %>%
  ggwithinstats(condition, rt,  
                p.adjust.method = "bonferroni",
                bf.message = F,
                results.subtitle = F,
                type = "np",
                centrality.path = F,
                ylab = "Tiempo de clasificación (s)",
                xlab = "Condición",
                title = "b")
ggpubr::ggarrange(ggstat.score, ggstat.rt)
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp1.violin.condiciones.spanish.jpg", width = 9, height = 5)
ggsave("plots/exp1.violin.condiciones.spanish.jpg", width = 9, height = 5)

experiment.1 %>% 
  mutate_at(1:2, as.factor) %>% 
  group_by(condition, participant) %>% 
  dplyr::summarise(context.rt = mean(context.rt)) %>% 
  ungroup() %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Ironic"  = "Ironía", "Unrelated" = "Sin relación", 
                                          "White lie" = "Mentira")) %>%
  ggwithinstats(condition, context.rt,  
                p.adjust.method = "bonferroni",
                bf.message = F,
                results.subtitle = T,
                type = "np",
                centrality.path = F,
                ylab = "Tiempo de lectura de contextos (s)",
                xlab = "Condición")
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp1.violin.condiciones.spanish.context.rt.jpg", width = 9, height = 6)

experiment.1 %>% 
  mutate_at(1:2, as.factor) %>% 
  group_by(condition, participant) %>% 
  dplyr::summarise(sentence.rt = mean(sentence.rt)) %>% 
  ungroup() %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Ironic"  = "Ironía", "Unrelated" = "Sin relación", 
                                          "White lie" = "Mentira")) %>%
  ggwithinstats(condition, sentence.rt,  
                p.adjust.method = "bonferroni",
                bf.message = F,
                results.subtitle = T,
                type = "np",
                centrality.path = F,
                ylab = "Tiempo de lectura de enunciados (s)",
                xlab = "Condición")
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp1.violin.condiciones.spanish.sentence.rt.jpg", width = 9, height = 6)

# plot -------------------------------------------------------------------
ggplot(descriptives.stimuli.score, aes(x= stimuli, y = mean, 
                                       ymax = (mean + sd), 
                                       ymin = (mean - sd))) + 
  theme_bw() +
  facet_wrap(~condition,nrow = 1) +
  geom_point(pch=1, fill = "black") +
  geom_errorbar(width=0.65) +
  geom_hline(yintercept = 60, color = "grey", alpha = 0.8) +
  ggtitle("Experiment 1. Contextual discrepancy item scores") +
  xlab("Item") +
  ylab("Mean + SD") +
  theme(text = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_flip() +
  scale_y_continuous(n.breaks = 6)
ggsave("plots/Figure 8.jpg", width = 12, height = 8)

# spanish plots ----------------------------------------------------------
descriptives.stimuli.score %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Ironic"  = "Ironía", "Unrelated" = "Sin relación",
                                          "White lie" = "Mentira")) %>% 
  ggplot(aes(x= stimuli, y = mean, 
             ymax = (mean + sd), 
             ymin = (mean - sd))) + 
  theme_bw() +
  facet_wrap(~condition,nrow = 1) +
  geom_point(pch=1, fill = "black") +
  geom_errorbar(width=0.65) +
  geom_hline(yintercept = 60, color = "grey", alpha = 0.8) +
  xlab("Estímulo") +
  ylab("Media + DE") +
  theme(text = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_flip() +
  scale_y_continuous(n.breaks = 6)
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/error.bar.plot.stimuli.exp1.spanish.jpg", width = 11, height = 8)

