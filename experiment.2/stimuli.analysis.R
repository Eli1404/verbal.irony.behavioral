# set up ------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(plyr)
library(webr)
library(rstatix)
setwd("/home/eli/Desktop/Neurobiologia/projects/ongoing/verbal.irony/facilitators/experiment.2/")

# data --------------------------------------------------------------------
facilitators <- read_csv("general/experiment.2.facilitators.pavlovia.csv") %>% 
  drop_na(stimuli)

# descriptives ------------------------------------------------------------
descriptives.condition.phrase <- facilitators %>% 
  dplyr::group_by(expName,condition,stimuli) %>% 
  numSummary(score)

statistics.tasks <- facilitators %>% 
  select(8,9,11,12) %>%
  group_by(expName,condition) %>% 
  numSummary %>% 
  write_csv(., "results/descriptives.tasks.csv")

statistics.tasks.sex <- facilitators %>% 
  select(3,8,9,11,12) %>%
  dplyr::group_by(expName,condition,sex) %>% 
  numSummary %>% 
  write_csv(., "results/descriptives.tasks.sex.csv")

statistics.tasks.degree <- facilitators %>% 
  select(5,8,9,11,12) %>%
  dplyr::group_by(expName,condition,`type of degree`) %>% 
  numSummary () %>% 
  write_csv(., "results/descriptives.tasks.degree.csv")

# selected stimuli --------------------------------------------------------
selected_stimuli <- facilitators %>% 
  dplyr::group_by(expName, condition, stimuli) %>% 
  dplyr::summarise(score = mean(score)) %>% 
  filter(score >= 0.60) %>% 
  write_csv(.,"general/selected_stimuli.csv")

# plot -------------------------------------------------------------------
descriptives.condition.phrase %>% 
  filter(condition != "") %>% 
  filter(expName != "") %>%
  ggplot(aes(x= stimuli, y = mean*100, 
             ymax = (mean + sd)*100, 
             ymin = (mean - sd)*100)) + 
  theme_bw() +
  geom_line()+
  facet_grid(expName~condition) +
  geom_point(pch=1, fill = "black") +
  geom_errorbar(width=0.6) +
  geom_hline(yintercept = 60,color = "grey", alpha = 0.6) +
  ggtitle("Experiment 2. Facial expression and Prosody item scores") +
  xlab("Item") +
  ylab("Mean + SD") +
  theme(text = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_flip() +
  scale_y_continuous(n.breaks = 6)
ggsave("plots/error.bar.plot.stimuli.exp2.jpg", width = 16, height = 16)