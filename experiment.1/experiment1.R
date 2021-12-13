# setup -------------------------------------------------------------------
rm (list = ls())
library(data.table)
library(tidyverse)
library(psych)
library(rstatix)
library(webr)
library(corrplot)
library(Hmisc)
library(RColorBrewer)
library(expss)

# data reading ------------------------------------------------------------
setwd("/home/eli/Desktop/Neurobiologia/projects/ongoing/verbal.irony/discrepancy/experiment.1/data/")
base <- read_csv("../general/Base.De.Datos.Participantes.csv") %>% 
  mutate_at(c(1:2,4:6), as.factor)
phrase_stimuli <- read_csv("../../stimulus.design/stimulus.construction/general/stimuli_phrase.csv") %>% 
  mutate_at(1:2, as.factor)

temp = list.files(pattern="*.csv")
experiment.1 = rbindlist(lapply(temp, fread), fill = TRUE) %>%
  mutate(condicion = dplyr::recode(condicion,"Ironía" = "Ironia", "Iron??a" = "Ironia")) %>% 
  filter(estimulo != "") %>% 
  dplyr::select(27,4,2,7,17,19:22) %>%
  dplyr::rename("condition" = "condicion", 
         "answer" = "key_resp_5.keys", "rt" = "key_resp_5.rt",
         "score" = "key_resp_5.corr", "context.rt" = "key_resp_3.rt",
         "sentence.rt" = "key_resp_4.rt") %>% 
  dplyr::mutate(condition = dplyr::recode(condition, "Absurdo" = "Unrelated",  "Ironia" = "Ironic",
                            "Mentira" = "White lie")) %>% 
  mutate_at(c(1:4,7), as.factor) %>% 
  full_join(phrase_stimuli) %>% 
  ungroup() 

experiment.1.by.participant <- experiment.1 %>% 
  dplyr::select(1,2,5,6,8,9) %>% 
  group_by(participant,condition) %>% 
  summarise_all(list(mean)) %>% 
  dplyr::mutate(score = round(score*100),
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
  select(2,5:9) %>% 
  dplyr::group_by(condition) %>% 
  numSummary() %>%
  write_csv(.,"../results/descriptives.condition.csv")

descriptives.condition.phrase <- experiment.1 %>% 
  dplyr::group_by(condition,stimuli) %>% 
  numSummary(score) %>%
  write_csv(.,"../results/descriptives.condition.phrase.csv")

descriptives.participantes <- experiment.1 %>% 
  dplyr::group_by(participant, condition) %>% 
  numSummary(score,rt) %>%
  write_csv(.,"../results/descriptives.participants.csv")

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
anova.test.welch <- descriptives.condition.phrase %>% 
  welch_anova_test(mean ~ condition) %>% 
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

# correlations ------------------------------------------------------------
# inferencial -------------------------------------------------------------
correlation.psychometric <- experiment.1.complete %>% 
  dplyr::select(1,7:32) %>% 
  cor_mat() %>% 
  cor_gather() %>% 
  adjust_pvalue(method = "fdr") %>% 
  dplyr::mutate(p.adj = round(p.adj, 3)) %>% 
  filter(p.adj <=0.05 | p.adj == 1) %>% 
  arrange(cor) %>% 
  write_csv(.,"../results/correlations.csv")

cor <- rcorr(as.matrix(experiment.1.complete[c(7:32)], type = "pearson")) %>%
  adjust_pvalue(method = "fdr")

png("../plots/cor.psychometric.png", width=1280,height=800)
corrplot(cor$r, method = "color",type = "lower", tl.col = "black", tl.cex = 0.8, 
         cl.ratio = 0.1, p.mat = cor$P, insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = .8, pch.col = "white",
         col = brewer.pal(n = 10, name = "BrBG"), order= "hclust", hclust.method = "average")
dev.off()

# plots -------------------------------------------------------------------
ggplot(descriptives.condition.phrase, aes(x= stimuli, y = mean*100, 
                                          ymax = (mean + sd)*100, 
                                          ymin = (mean - sd)*100)) + 
  theme_bw() +
  geom_line()+
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
ggsave("../plots/Figure 8.jpg", width = 12, height = 8)
