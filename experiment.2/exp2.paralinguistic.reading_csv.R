# set up ------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(plyr)
setwd("/home/eli/Desktop/Neurobiologia/projects/ongoing/verbal.irony/facilitators/experiment.2/")

# general -----------------------------------------------------------------
general.data.pavlovia <- read_csv("tasks/google.forms/Datos generales.csv") %>% 
  dplyr::rename(id = "Nombre de usuario", sex = "Sexo", age = "Edad",
                degree = "Profesión",
                last.degree = "Último grado de estudio", "type of degree" = "Group") %>% 
  select(-1,-8:-10) %>% 
  mutate_at(1,as.factor)
phrases <- read_csv("../stimulus.design/Rostros/Piloto3-pavlovian/stimulus.csv")

# facial expression -------------------------------------------------------
setwd("tasks/facial.expression/data/")
temp.fe = list.files(pattern="*.csv", recursive = T)
facial.expression = rbindlist(lapply(temp.fe, fread), fill = TRUE) %>% 
  mutate_at(17, as.factor) %>% 
  full_join(phrases) %>% 
  mutate_at(c(16,24,43), as.factor)

# prosody -----------------------------------------------------------------
setwd("../../prosody/data/")
temp.p = list.files(pattern="*.csv", recursive = T)
prosody = rbindlist(lapply(temp.p, fread), fill = TRUE) %>% 
  mutate_at(21, as.factor) %>% 
  separate(Audio, into = c("stimuli", "voice")) %>% 
  mutate_at(16, as.factor) 

# join data ---------------------------------------------------------------
experiment.2 <- facial.expression %>% 
  full_join(prosody) %>%
  drop_na(stimuli) %>% 
  select(1:3,18,19,20,24,27,43,44) %>% 
  rename_at(7,~"id") %>% 
  dplyr::rename(answer = key_resp_1.keys, score = key_resp_1.corr, rt = key_resp_1.rt) %>% 
  dplyr::mutate(answer = dplyr::recode(answer, "1" = "Irony", "2" = "Literal", "3" = "Unrelated")) %>% 
  full_join(general.data.pavlovia) %>% 
  mutate(sex = dplyr::recode(sex, "Másculino" = "male", "Femenino" = "female")) %>% 
  mutate(presentation = "psychopy-pavlovia") %>% 
  mutate(modality.of.presentation = ifelse(expName == "prosody", 1, 0)) %>% 
  mutate(modality.of.presentation = dplyr::recode(modality.of.presentation,"1" = "audio", "0" = "text and image")) %>% 
  mutate(actor.sex = dplyr::recode(actor,"019.jpg" = "M","184.jpg" = "M",
                        "075.jpg" = "F", "078.jpg" = "F")) %>% 
  arrange(expName,id) %>% 
  drop_na(sex) %>%  
  drop_na(condition) %>% 
  mutate(expName = dplyr::recode(expName, "Expresion facial" = "Facial expression",
                          "Audios_prosodia" = "Prosody")) %>% 
  write_csv(.,"../../../general/exp2.paralinguistic.full_stimuli_data.csv")
  
# selecting stimuli --------------------------------------------------------
selected_stimuli.FE <- experiment.2 %>%
  filter(expName == "Facial expression") %>% 
  dplyr::group_by(expName, condition, stimuli, actor, actor.sex) %>% 
  dplyr::summarise(score = mean(score)*100) %>% 
  dplyr::filter(score >= 65) %>% 
  mutate(selected = "selected") %>% 
  write_csv(.,"../../../general/selected_stimuli_FE.csv")

selected_stimuli.prosody <- experiment.2 %>% 
  dplyr::group_by(expName, condition, stimuli, voice) %>% 
  dplyr::summarise(score = mean(score)) %>% 
  filter(score >= 0.65) %>% 
  filter(expName == "Prosody") %>% 
  mutate(selected = "selected") %>% 
  write_csv(.,"../../../general/selected_stimuli_prosody.csv")

selected_stimuli.all <- full_join(selected_stimuli.prosody, selected_stimuli.FE) %>% 
  select(-5) %>% 
  write_csv(.,"../../../general/selected_stimuli_all.csv")

experiment.2.filted_selected <- experiment.2 %>% 
  full_join(selected_stimuli.all) %>% 
  drop_na(selected) %>% 
  write_csv(.,"../../../general/exp2.paralinguistic.full_stimuli_data.filter_selected.csv")

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

psychometrics <- full_join(RMET,AQ_Task) %>% 
  full_join(SSS) %>% 
  mutate_at(1, as.factor) %>% 
  full_join(general.data.pavlovia) %>% 
  dplyr::select(1,11:15,2:10) %>% 
  rename_at(5,~"group") %>% 
  write_csv(.,"../../../general/exp2.psychometrics.csv")
# data by participants ----------------------------------------------------
experiment.2.by.participant.rt <- experiment.2.filted_selected %>% 
  dplyr::group_by(expName,id, condition) %>% 
  filter(score == 1) %>% 
  dplyr::summarise(rt = round(mean(rt,na.rm = T),2)) %>% 
  ungroup()

experiment.2.by.participant <- experiment.2.filted_selected %>% 
  dplyr::group_by(expName,id, condition) %>% 
  dplyr::summarise(score = round(mean(score, na.rm = T),2)*100) %>%
  full_join(experiment.2.by.participant.rt) %>% 
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
  separate("Prosody_Irony", into = c("Prosody.Irony.score", "Prosody.Irony.rt"), sep = "_") %>%
  separate("Prosody_Literal", into = c("Prosody.Literal.score", "Prosody.Literal.rt"), sep = "_") %>%
  separate("Prosody_Unrelated", into = c("Prosody.Unrelated.score", "Prosody.Unrelated.rt"), sep = "_") %>% 
  mutate_at(2:13, as.numeric) %>% 
  full_join(RMET) %>% 
  full_join(AQ_Task) %>% 
  full_join(SSS) %>% 
  mutate_at(1, as.factor) %>% 
  full_join(general.data.pavlovia) %>% 
  dplyr::select(1,24,23,25:27,2:22) %>% 
  rename_at(5,~"group") %>% 
  drop_na(FE.Irony.score) %>% 
  write_csv(.,"../../../general/exp2.paralinguistic.results_by_participant.csv")

# data by participant spanish ---------------------------------------------
experiment.2.by.participant.spanish <- experiment.2.by.participant %>% 
  dplyr::rename("sexo" = "sex", "edad" = "age", "grado" = "degree", "grupo" = "group", 
                "EF Ironía puntuación" = "FE.Irony.score", "EF Ironía tr" = "FE.Irony.rt",
                "EF Literal puntuación" = "FE.Literal.score", "EF Literal tr" = "FE.Literal.rt",
                "EF Sin relación puntuación" = "FE.Unrelated.score", "EF Sin relación tr" = "FE.Unrelated.rt",
                "Prosody Ironía puntuación" = "Prosody.Irony.score", "Prosodia Ironía tr" = "Prosody.Irony.rt",
                "Prosody Literal puntuación" = "Prosody.Literal.score", "Prosodia Literal tr" = "Prosody.Literal.rt",
                "Prosody Sin relación puntuación" = "Prosody.Unrelated.score", "Prosodia Sin relación tr" = "Prosody.Unrelated.rt",
                "RMET" = "rmet","RMET.tr" = "rmet.rt", "cambio atencional" = "attention switching","atención a detalles" = "attention to detail",
                "comunicación"  = "communication", "imaginación" = "imagination", "habilidades sociales" = "social skill") %>% 
  write_csv(.,"../../../general/exp2.context.results_by_participant.spanish.csv")
