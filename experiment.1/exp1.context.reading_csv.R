# setup -------------------------------------------------------------------
rm (list = ls())
library(data.table)
library(tidyverse)
setwd("~/Desktop/Neurobiologia/projects/ongoing/verbal.irony/discrepancy/experiment.1/task/data/")
#setwd("discrepancy/experiment.1/task/data/")

# general -----------------------------------------------------------------
base <- read_csv("../../general/Base.De.Datos.Participantes.csv") %>% 
  mutate_at(c(1:2,4:6), as.factor)

phrase_stimuli <- read_csv("../../../stimulus.design/stimulus.construction/general/stimuli_phrase.csv") %>% 
  mutate_at(1:2, as.factor)

# contextual discrepancy task ---------------------------------------------
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
  ungroup() %>% 
  full_join(phrase_stimuli) %>% 
  write_csv(., "../../general/exp1.context.full_stimuli_data.csv")

# data by participant --------------------------------------------------
experiment.1.by.participant.rt <- experiment.1 %>% 
  dplyr::select(1,2,5,6,8,9) %>% 
  filter(score == 1) %>% 
  group_by(participant,condition) %>% 
  summarise(rt = round(mean(rt, na.rm = T),2)) %>% 
  mutate(rt1 = "rt") %>% 
  unite(variables, c("condition", "rt1")) %>% 
  spread(variables, rt) 
  
experiment.1.by.participant <- experiment.1 %>% 
  dplyr::select(1,2,5,6,8) %>% 
  dplyr::group_by(participant,condition) %>% 
  dplyr::summarise(score = round(mean(score*100)),
                context.rt = round(mean(context.rt,2)),
                sentence.rt = round(mean(sentence.rt,2))) %>% 
  gather(3:5, key = "key", value = "value") %>% 
  unite("variables", 2:3) %>% 
  spread(variables, value) %>% 
  full_join(experiment.1.by.participant.rt) %>% 
  full_join(base, by = "participant") %>% 
  dplyr::select(1,18:22,2:17,23:33) %>% 
  write_csv(.,"../../general/exp1.context.results_by_participant.csv")

# data by participant spanish ---------------------------------------------
experiment.1.by.participant.spanish <- experiment.1.by.participant %>% 
  rename("Ironía contexto tiempo" = "Ironic_context.rt", "Ironía clasificación tiempo" = "Ironic_rt", "Ironía puntuación" = "Ironic_score","Ironía enunciado tiempo" = "Ironic_sentence.rt",
         "Literal contexto tiempo" = "Literal_context.rt", "Literal clasificación tiempo" = "Literal_rt", "Literal puntuación" = "Literal_score","Literal enunciado tiempo" = "Literal_sentence.rt",
         "Sin relación contexto tiempo" = "Unrelated_context.rt", "Sin relación clasificación tiempo" = "Unrelated_rt", "Sin relación puntuación" = "Unrelated_score","Sin relación enunciado tiempo" = "Unrelated_sentence.rt",
         "Mentira contexto tiempo" = "White lie_context.rt", "Mentira clasificación tiempo" = "White lie_rt", "Mentira puntuación" = "White lie_score","Mentira enunciado tiempo" = "White lie_sentence.rt",
         "Fluidez verbal" = "VF", "Retención de dígitos en progresión"= "DSF", "Retención de dígitos en regresión"= "DSB",
         "Retención de dígitos"= "DS", "Retención de dígitos tiempo"= "DS.Time","Diseño de cubos" = "BDT",
         "Historia corta espontánea" = "SST.S","Historia corta explícita" = "SST.E", "Historia corta comprensión" = "SST.C",
         "Historia corta total" = "SST", "Matrices progresivas de Raven" = "Raven") %>% 
  write_csv(.,"../../general/exp1.context.results_by_participant.spanish.csv")

