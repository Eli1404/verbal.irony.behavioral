# setup -------------------------------------------------------------------
rm (list = ls())
library(data.table)
library(tidyverse)
library(psych)
library(rstatix)
library(webr)

# data reading ------------------------------------------------------------
setwd("/home/eli/Desktop/Neurobiologia/projects/ongoing/verbal.irony/discrepancy/experiment.1/data/")
base <- read_csv("../general/Base.De.Datos.Participantes.csv") %>% 
  mutate_at(c(1:2,4:6), as.factor)
phrase_stimuli <- read_csv("../../stimulus.design/stimulus.construction/general/stimuli_phrase.csv") %>% 
  mutate_at(1:2, as.factor)

temp = list.files(pattern="*.csv")
experiment.1 = rbindlist(lapply(temp, fread), fill = TRUE) %>%
  mutate(condicion = recode(condicion,"Ironía" = "Ironia", "Iron??a" = "Ironia")) %>% 
  filter(estimulo != "") %>% 
  dplyr::select(27,4,2,7,17,19:22) %>%
  dplyr::rename("condition" = "condicion", 
         "answer" = "key_resp_5.keys", "rt" = "key_resp_5.rt",
         "score" = "key_resp_5.corr", "context.rt" = "key_resp_3.rt",
         "sentence.rt" = "key_resp_4.rt") %>% 
  mutate(condition = recode(condition, "Absurdo" = "Unrelated",  "Ironia" = "Irony",
                            "Mentira" = "White lie")) %>% 
  mutate_at(c(1:4,7), as.factor) %>% 
  full_join(phrase_stimuli) %>% 
  ungroup()

experiment.1.complete <- experiment.1 %>% 
  dplyr::select(1,2,5,6,8,9) %>% 
  group_by(participant,condition) %>% 
  summarise_all(list(mean)) %>% 
  mutate(score = round(score*100),
         context.rt = round(context.rt,2),
         sentence.rt = round(sentence.rt,2),
         rt = round(rt,2)) %>% 
  gather(3:6, key = "key", value = "value") %>% 
  unite("variables", 2:3) %>% 
  spread(variables, value) %>% 
  full_join(base, by = "participant") %>% 
  dplyr::select(1,18:22,2:17,23:32) %>% 
  write_csv(.,"../general/Data.Base.Experiment-1.csv")

# descriptives ------------------------------------------------------------
descriptives.condition <- experiment.1 %>% 
  dplyr::group_by(condition) %>% 
  numSummary(score,rt)

descriptives.condition.phrase <- experiment.1 %>% 
  dplyr::group_by(condition,stimuli) %>% 
  numSummary(score)

descriptives.participantes <- experiment.1 %>% 
  dplyr::group_by(participant, condition) %>% 
  numSummary(score,rt)

# outliers ----------------------------------------------------------------
outliers.general <- experiment.1 %>%  
  dplyr::select(2,8) %>% 
  dplyr::group_by(condition) %>% 
  identify_outliers(score) %>% 
  write_csv(., "../results/outliers.csv")

# normality ---------------------------------------------------------------
shapiro.general <- experiment.1 %>%  
    dplyr::group_by(condition) %>% 
    shapiro_test(score)  %>% 
    dplyr::mutate(p = round(p,4)) %>% 
    write_csv(., "../results/shapiro.csv")

# homogeneity of variances ------------------------------------------------
homogeneity.condition <- experiment.1 %>%  
  levene_test(score ~ condition) %>%
  dplyr::mutate(p = round(p,4)) %>% 
  write_csv(., "../results/levene.csv")
  
# welch anova -------------------------------------------------------------
attach(experiment.1)
condition
anova.test.welch <- experiment.1 %>% 
  welch_anova_test(score ~ condition) %>% 
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") %>% 
  mutate(p.adj = round(p.adj, 3)) %>% 
  write_csv(.,"../results/anova-welch.csv")
  
games.test.accuaracy <- descriptives.condition.phrase %>% 
  games_howell_test(mean ~ condition) %>%
  adjust_pvalue(method = "bonferroni") %>%
  mutate(p.adj = round(p.adj,3)) %>% 
  arrange(p.adj) %>% 
  write_csv(.,"../results/games-test.csv")

# plots -------------------------------------------------------------------
ggplot(descriptives.condition.phrase, aes(x= stimuli, y = mean*100, 
                                          ymax = (mean + sd)*100, 
                                          ymin = (mean - sd)*100)) + 
  theme_bw() +
  geom_line()+
  facet_wrap(~condition,nrow = 1) +
  geom_point(pch=1, fill = "black") +
  geom_errorbar(width=0.6) +
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
ggsave("../../../images/error.bar.plot.stimuli.exp1.jpg", width = 12, height = 8)