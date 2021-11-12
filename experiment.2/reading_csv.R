# set up ------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(plyr)
library(webr)
library(rstatix)
setwd("/home/eli/Desktop/Neurobiologia/projects/ongoing/verbal.irony/facilitators/experiment.2/")

# general -----------------------------------------------------------------
general.data.pavlovia <- read_csv("tasks/google.forms/Datos generales.csv") %>% 
  dplyr::rename(id = "Nombre de usuario", sex = "Sexo", age = "Edad",
                degree = "Profesión",
                last.degree = "Último grado de estudio", "type of degree" = "Group") %>% 
  select(-1,-8:-10) %>% 
  mutate_at(1,as.factor)
phrases <- read_csv("../stimulus.design/Rostros/Piloto3-pavlovian/stimulus.csv")

# facilitators ------------------------------------------------------------
setwd("tasks/facial.expression/data/")
temp.fe = list.files(pattern="*.csv", recursive = T)
facial.expression = rbindlist(lapply(temp.fe, fread), fill = TRUE) %>% 
  mutate_at(17, as.factor) %>% 
  full_join(phrases) %>% 
  mutate_at(c(16,24,43), as.factor)

setwd("../../prosody/data/")
temp.p = list.files(pattern="*.csv", recursive = T)
prosody = rbindlist(lapply(temp.p, fread), fill = TRUE) %>% 
  mutate_at(21, as.factor) %>% 
  separate(Audio, into = c("stimuli", "voice")) %>% 
  mutate_at(16, as.factor) 

# join data ---------------------------------------------------------------
facilitators <- facial.expression %>% 
  full_join(prosody) %>%
  drop_na(stimuli) %>% 
  select(1:3,18,19,24,27,43) %>% 
  rename_at(6,~"id") %>% 
  dplyr::rename(answer = key_resp_1.keys, score = key_resp_1.corr, rt = key_resp_1.rt) %>% 
  mutate(answer = recode(answer, "1" = "Irony", "2" = "Literal", "3" = "Unrelated")) %>% 
  full_join(general.data.pavlovia) %>% 
  select(6,9:13,8,2,3,1,5,8,7) %>%
  mutate(sex = recode(sex, "Másculino" = "male", "Femenino" = "female")) %>% 
  mutate(presentation = "psychopy-pavlovia") %>% 
#  mutate(expName = recode(expName, "Expresion facial" = "facial.expression", "Audios_prosodia" = "prosody")) %>% 
  mutate(modality.of.presentation = ifelse(expName == "prosody", 1, 0)) %>% 
  mutate(modality.of.presentation = recode(modality.of.presentation,"1" = "audio", "0" = "text and image")) %>% 
  mutate(`type of degree` = recode(`type of degree`, "No.sciencia" = "No.science")) %>% 
  arrange(expName,id) %>% 
  drop_na(sex) %>%  
  drop_na(condition) %>% 
  drop_na(stimuli) %>% 
  mutate(expName = recode(expName, "Expresion facial" = "Facial expression",
                          "Audios_prosodia" = "Prosody")) %>% 
  write_csv(., "../../../general/experiment.2.facilitators.pavlovia.csv")

# psychometrical test -----------------------------------------------------
# rmet --------------------------------------------------------------------
setwd("../../rmet/data/")
temp.rmet = list.files(pattern="*.csv", recursive = T)
rmet = rbindlist(lapply(temp.rmet, fread), fill = TRUE) %>%
  dplyr::select(4,5,20)

RMET <- rmet %>%
  filter(key_resp_3.corr != "NA") %>% 
  rename_at(3,~"id") %>% 
  group_by(id) %>%
  dplyr::summarise(rmet = sum(key_resp_3.corr), rmet.rt= round(mean(key_resp_3.rt),2))

# AQ ----------------------------------------------------------------------
AQ <- read_csv("../../../general/AQ Prueba.csv") %>% 
  dplyr::select(1,3,4) %>% 
  mutate_at(1,as.character)

AQ_Task <- read_csv("../../google.forms/AQ.csv") %>%
  gather(4:53, key = "stimuli", value = "answer") %>%
  separate(stimuli, into = c("item")) %>%
  full_join(AQ, by = "item") %>%
  separate(correctans, into = c("ans", "ans1", "ans2")) %>%
  dplyr::select(-2,-7) %>%
  mutate_at(c(6:7), as.numeric) %>%
  mutate(score = ifelse(answer == ans1 | answer == ans2, 1, 0)) %>%
  mutate_at(c(1:3), as.factor) %>%
  rename_at(2, ~"id") %>% 
  select(-1,-3,-4,-6,-7) %>% 
  group_by(id, type) %>%
  dplyr::summarise(AQ = sum(score)) %>%
  spread(type, AQ) %>%
  ungroup() %>% 
  mutate(AQ = rowSums(.[2:6])) %>% 
  drop_na(id) %>% 
  arrange(AQ)
  
# SSS ---------------------------------------------------------------------
SSS <- read_csv("../../google.forms/SSS.csv") %>%  
  gather(3:18, key = "stimuli", value = "answer") %>%
  filter(answer != "NA") %>%
  separate(stimuli, into = c("item")) %>%
  select(-1) %>% 
  rename_at(1, ~"id") %>% 
  mutate_at(2, as.factor) %>%
  group_by(id) %>%
  dplyr::summarise(SSS = round(mean(answer), 2))

# complete tasks ----------------------------------------------------------
pavlovia <- facilitators %>% 
  dplyr::group_by(expName,id, condition) %>% 
  dplyr::summarise(score = round(mean(score, na.rm = T),2), rt = round(mean(rt,na.rm = T),2)) %>% 
  ungroup() %>% 
  filter(score!="NaN") %>% 
  mutate_at(1:2,as.factor) %>% 
  drop_na(expName) %>%
  unite("values", 4:5) %>%
  unite("value",c(1,3)) %>% 
  spread(value, values) %>% 
  separate("Facial expression_Irony", into = c("FE.Irony.score", "FE.Irony.rt"), sep = "_") %>%
  separate("Facial expression_Literal", into = c("FE.Literal.score", "FE.Literal.rt"), sep = "_") %>%
  separate("Facial expression_Unrelated", into = c("FE.Unrelated.score", "FE.Unrelated.rt"), sep = "_") %>%
  separate("Prosody_Irony", into = c("prosody.Irony.score", "prosody.Irony.rt"), sep = "_") %>%
  separate("Prosody_Literal", into = c("prosody.Literal.score", "prosody.Literal.rt"), sep = "_") %>%
  separate("Prosody_Unrelated", into = c("prosody.Unrelated.score", "prosody.Unrelated.rt"), sep = "_") %>% 
  mutate_at(2:13, as.numeric) %>% 
  full_join(RMET) %>% 
  full_join(AQ_Task) %>% 
  full_join(SSS) %>% 
  mutate_at(1, as.factor) %>% 
  full_join(general.data.pavlovia) %>% 
  dplyr::select(1,23:27,2:22) %>% 
  drop_na(FE.Irony.score) %>% 
  write_csv(.,"../../../general/Results.by.participants.csv")

