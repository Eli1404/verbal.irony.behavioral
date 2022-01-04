# setup -------------------------------------------------------------------
library(tidyverse)
library(rstatix)
library(webr)
library(ggstatsplot)
setwd("~/Desktop/Neurobiologia/projects/ongoing/verbal.irony/discrepancy/experiment.1/")

# reading data ------------------------------------------------------------
experiment.1 <- read_csv("general/exp1.context.full_stimuli_data.csv")

# descriptives ------------------------------------------------------------
descriptives.score <- experiment.1 %>% 
  select(2,5,6,8) %>% 
  dplyr::group_by(condition) %>% 
  numSummary() 

descriptives.condition.vars.correctans <- experiment.1 %>% 
  filter(score == 1) %>% 
  select(2,9) %>% 
  dplyr::group_by(condition) %>% 
  numSummary() %>%
  mutate(vars = "rt") %>% 
  full_join(descriptives.score) %>% 
  select(1,14,2:13) %>% 
  write_csv(.,"results/descriptives.condition_correctans.csv")

descriptives.score.without.filter <- experiment.1 %>% 
  select(2,8,5,6,9) %>% 
  dplyr::group_by(condition) %>% 
  numSummary() %>%
  write_csv(.,"results/descriptives.condition_without.filter.csv")

descriptives.stimuli.score <- experiment.1 %>% 
  dplyr::group_by(stimuli,condition) %>%
  numSummary(score) %>% 
  dplyr::mutate(mean = round(mean*100,2), sd = round(sd*100,2)) %>% 
  mutate(vars = "score")

descriptives.stimuli.vars <- experiment.1 %>% 
  filter(score == 1) %>% 
  select(2,5,6,9,10) %>% 
  dplyr::group_by(condition, stimuli) %>% 
  numSummary() %>%
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

# welch anova -------------------------------------------------------------
anova.test.condition <- descriptives.stimuli.vars %>% 
  group_by(vars) %>% 
  welch_anova_test(mean ~ condition) %>% 
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") %>% 
  mutate(p.adj = round(p.adj, 3)) %>% 
  write_csv(.,"results/anova-welch.csv")

games.test.accuaracy <- descriptives.stimuli.vars %>% 
  group_by(vars) %>%
  games_howell_test(mean ~ condition) %>%
  adjust_pvalue(method = "bonferroni") %>%
  mutate(p.adj = round(p.adj,3)) %>% 
  arrange(p.adj) %>% 
  write_csv(.,"results/games-test.csv")

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


ggstat.score <- descriptives.stimuli.vars %>% 
  filter(vars == "score") %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Ironic"  = "Ironía", "Unrelated" = "Sin relación",                                          "White lie" = "Mentira")) %>%
  ggbetweenstats(condition, mean,
                 p.adjust.method = "bonferroni",
                 bf.message = F, results.subtitle = F,
                 ylab = "Porcentaje de clasificación",
                 xlab = "Condición", 
                 title = "a")

ggstat.rt <- descriptives.stimuli.vars %>% 
  filter(vars == "rt") %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Ironic"  = "Ironía", "Unrelated" = "Sin relación",
                                          "White lie" = "Mentira")) %>%
  ggbetweenstats(condition, mean,  
                 p.adjust.method = "bonferroni",
                 bf.message = F, results.subtitle = F,
                 ylab = "Tiempo de clasificación (s)",
                 xlab = "Condición", 
                 title = "b")
ggpubr::ggarrange(ggstat.score, ggstat.rt)
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp1.violin.condiciones.spanish.jpg", width = 9, height = 5)
ggsave("plots/exp1.violin.condiciones.spanish.jpg", width = 9, height = 5)