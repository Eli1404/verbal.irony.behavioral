# setup -------------------------------------------------------------------
rm(list = ls())
library(data.table)
library(tidyverse)
library(plyr)
setwd("/home/eli/Desktop/Neurobiologia/projects/ongoing/verbal.irony/facilitators/experiment.3/tasks/google.forms/")

# general csv reading -------------------------------------------------------------
temp.g = list.files(pattern="*.csv")
read_csv_filename <- function(temp){
  ret <- read_csv(temp, locale = locale(encoding = "Latin1"))
  ret$csv <- temp 
  ret}

formularios <- ldply(temp.g, read_csv_filename) %>%
  rename_at(2, ~"id") %>% 
  filter(!id %in% c("cdpolis1980@gmail.com","xfdzciencias@gmail.com",
                 "palmaleo30@gmail.com", "josue.jovannni.re@gmail.com"))

# general ------------------------------------------------------------
general.data <- formularios %>% 
  select(2,53,86:99) %>% 
  filter(csv == "Datos Generales-Estudio Resonancia Magnética.csv") %>% 
  select(1,4,5)

# additional data --------------------------------------------------------------------
setwd("/home/eli/Desktop/Neurobiologia/projects/ongoing/verbal.irony/facilitators/experiment.3/")

phrases.facilitators <- read_csv("../stimulus.design/Rostros/Piloto3-pavlovian/stimulus.csv")
phrase.context <- read_csv("../../discrepancy/stimulus.design/stimulus.construction/general/stimuli_phrase.csv")

# selected stimuli from experiment 2 --------------------------------------
selected_stimuli.p <- read_csv("../experiment.2/general/selected_stimuli_prosody.csv") %>% 
  dplyr::select(1:4) %>% 
  mutate_at(1:4, as.factor) %>% 
  mutate(selected = "selected")

selected_stimuli.fe <- read_csv("../experiment.2/general/selected_stimuli_FE.csv") %>% 
  dplyr::select(1:4) %>% 
  mutate_at(1:4, as.factor) %>% 
  mutate(selected = "selected")

# contextual discrepancy --------------------------------------------------
setwd("tasks/contextual.discrepancy/data/")
temp.c = list.files(pattern="*.csv", recursive = T)
contextual = rbindlist(lapply(temp.c, fread), fill = TRUE) %>% 
  mutate_at(23, as.factor) %>% 
  full_join(phrase.context) %>% 
  mutate_at(31, as.factor) %>% 
  dplyr::select(-2,-4,-5,-8:-19,-24:-30) %>% 
  mutate(expName = "Contextual discrepancy") %>% 
  drop_na(participant) %>%
  rename_at(3,~"condition") %>% 
  filter(condition != "") %>% 
  rename_at(5,~"key_resp_1.keys") %>% 
  rename_at(6,~"key_resp_1.corr") %>% 
  rename_at(7,~"key_resp_1.rt")

# facial expression -------------------------------------------------------
setwd("../../facial.expresion/data/")
temp.fe = list.files(pattern="*.csv", recursive = T)
facial.expression = rbindlist(lapply(temp.fe, fread), fill = TRUE) %>%
  full_join(phrases.facilitators) %>%
  mutate_at(c(4,24,35), as.factor) %>%
  dplyr::select(-6:-24,-29,-30,-32:-34) %>%
  mutate(expName = recode(expName, "Expresion facial" = "Facial expression")) %>% 
  filter(condition != "") %>% 
  mutate(participant = recode(participant,  "vianney" = "vianneymendoza00@gmail.com")) %>% 
  mutate(actor.sex = recode(actor,"019.jpg" = "M","184.jpg" = "M",
                        "075.jpg" = "F", "078.jpg" = "F")) %>% 
  full_join(selected_stimuli.fe) %>% 
  drop_na(selected) %>% 
  select(-13)

# prosody -----------------------------------------------------------------
setwd("../../prosody/data/")
temp.p = list.files(pattern="*.csv", recursive = T)
prosody = rbindlist(lapply(temp.p, fread), fill = TRUE) %>%
  mutate_at(21, as.factor) %>%
  separate(Audio, into = c("stimuli", "voice")) %>%
  mutate_at(c(1,16,35), as.factor) %>%
  dplyr::select(-4:-30,-34:-37,-39,-40,-42:-57) %>% 
  filter(condition != "") %>% 
  mutate(expName = recode(expName,"Audios_prosodia" = "Prosody")) %>% 
  full_join(selected_stimuli.p) %>% 
  drop_na(selected) %>%
  mutate_at(2, as.factor) %>% 
  select(-9)

# join data ---------------------------------------------------------------
experiment.3 <- full_join(facial.expression, prosody) %>% 
  full_join(contextual) %>% 
  dplyr::rename("phrase" = "frase", "answer" = "key_resp_1.keys", 
         "rt" ="key_resp_1.rt", "score" = "key_resp_1.corr",
         "id" = "participant") %>% 
  dplyr::select(-1) %>% 
  mutate(answer = recode(answer, "1" = "Ironic", "2" = "Literal", "3" = "Unrelated")) %>% 
  mutate(condition = recode(condition, "Absurdo" = "Unrelated", "Ironia" = "Ironic", 
                            "Sin relacion" = "Unrelated", "Sincera" = "Literal",
                            "Irony" = "Ironic")) %>% 
  full_join(general.data) %>% 
  filter(id != "aniavj@gmail.com")%>%
  mutate(id = recode(id, "chokkoss@gmail.com" = "cokkoss@gmail.com")) %>%
  write_csv(.,"../../../general/exp3.paralinguistic.full_stimuli_data.csv")

# AQ ----------------------------------------------------------------------
AQ <- read_csv("../../../general/AQ Prueba.csv") %>% 
  dplyr::select(1,3,4) %>% 
  mutate_at(1,as.character) 

AQ_Task <- formularios %>%
  filter(grepl("AQ", csv)) %>%
  select(2:52) %>%
  gather(2:51, key = "stimuli", value = "answer") %>%
  separate(stimuli, into = c("item")) %>%
  full_join(AQ, by = "item") %>%
  separate(correctans, into = c("ans", "ans1", "ans2")) %>%
  dplyr::select(-5) %>%
  mutate_at(c(5:6), as.numeric) %>%
  mutate(score = ifelse(answer == ans1 | answer == ans2, 1, 0)) %>%
  mutate(id= sub("dzoarae@gmai.com", "dzoarae@gmail.com", id)) %>% 
  filter(id != "direccion@caadd.org.mx") %>% 
  mutate_at(1:4, as.factor) %>%
  dplyr::select(-6,-5) %>%
  group_by(id, type) %>%
  dplyr::summarise(AQ_score = sum(score)) %>%
  spread(type, AQ_score) %>%
  ungroup() %>% 
  mutate(AQ.score = rowSums(.[2:6])) 

# rmet --------------------------------------------------------------------
setwd("../../rmet/data/")
temp.rmet = list.files(pattern="*.csv", recursive = T)
rmet = rbindlist(lapply(temp.rmet, fread), fill = TRUE) %>%
  dplyr::select(55,50,51)

RMET <- rmet %>%
  dplyr::filter(key_resp_3.corr != "NA") %>% 
  rename_at(1,~"id") %>% 
  group_by(id) %>%
  dplyr::summarise(rmet = sum(key_resp_3.corr), rmet.rt= round(mean(key_resp_3.rt),2))

# SSS ---------------------------------------------------------------------
SSS <- formularios %>%  
  filter(grepl("SSS", csv)) %>% 
  select(2,53,112:127) %>% 
  gather(3:17, key = "stimuli", value = "answer") %>%
  filter(answer != "NA") %>%
  separate(stimuli, into = c("item")) %>%
  mutate_at(2, as.factor) %>%
  group_by(id) %>%
  dplyr::summarise(SSS = round(mean(answer), 2))

# SST ---------------------------------------------------------------------
SST <- read_csv("../../../general/Tests-results.csv")

# data by participant -----------------------------------------------------
experiment.3.by.participant <- experiment.3 %>%
  group_by(id,condition,expName) %>% 
  dplyr::summarise(mean= round(mean(score*100),1), rt = round(mean(rt),2)) %>% 
  drop_na(expName) %>% 
  unite("variables", c(3,2)) %>% 
  unite("values", c(3,4)) %>% 
  spread(variables,values) %>% 
  separate("Contextual discrepancy_Ironic", into = c("Context.Irony.score", "Context.Irony.rt"), sep = "_") %>%
  separate("Contextual discrepancy_Literal", into = c("Context.Literal.score", "Context.Literal.rt"), sep = "_") %>%
  separate("Contextual discrepancy_Unrelated", into = c("Context.Unrelated.score", "Context.Unrelated.rt"), sep = "_") %>%
  separate("Facial expression_Ironic", into = c("FE.Irony.score", "FE.Irony.rt"), sep = "_") %>%
  separate("Facial expression_Literal", into = c("FE.Literal.score", "FE.Literal.rt"), sep = "_") %>%
  separate("Facial expression_Unrelated", into = c("FE.Unrelated.score", "FE.Unrelated.rt"), sep = "_") %>%
  separate("Prosody_Ironic", into = c("Prosody.Irony.score", "Prosody.Irony.rt"), sep = "_") %>%
  separate("Prosody_Literal", into = c("Prosody.Literal.score", "Prosody.Literal.rt"), sep = "_") %>%
  separate("Prosody_Unrelated", into = c("Prosody.Unrelated.score", "Prosody.Unrelated.rt"), sep = "_") %>% 
  full_join(RMET) %>% 
  full_join(AQ_Task) %>% 
  full_join(SSS) %>% 
  full_join(SST) %>% 
  full_join(general.data) %>% 
  filter(id != "aniavj@gmail.com") %>% 
  select (1,33,34,2:32) %>% 
  write_csv(.,"../../../general/exp3.paralinguistic.results_by_participant.csv")

# data by participant spanish ---------------------------------------------
experiment.3.by.participant.spanish <- experiment.3.by.participant %>% 
  dplyr::rename("sexo" = "GÃ©nero", "edad" = "Edad", 
                "Contexto Ironía puntuación" = "Context.Irony.score", "Contexto Ironía tr" = "Context.Irony.rt",
                "Contexto Literal puntuación" = "Context.Literal.score", "Contexto Literal tr" = "Context.Literal.rt",
                "Contexto Sin relación puntuación" = "Context.Unrelated.score", "Contexto Sin relación tr" = "Context.Unrelated.rt",                
                "EF Ironía puntuación" = "FE.Irony.score", "EF Ironía tr" = "FE.Irony.rt",
                "EF Literal puntuación" = "FE.Literal.score", "EF Literal tr" = "FE.Literal.rt",
                "EF Sin relación puntuación" = "FE.Unrelated.score", "EF Sin relación tr" = "FE.Unrelated.rt",
                "Prosody Ironía puntuación" = "Prosody.Irony.score", "Prosody Ironía tr" = "Prosody.Irony.rt",
                "Prosody Literal puntuación" = "Prosody.Literal.score", "Prosody Literal tr" = "Prosody.Literal.rt",
                "Prosody Sin relación puntuación" = "Prosody.Unrelated.score", "Prosody Sin relación tr" = "Prosody.Unrelated.rt",
                "RMET" = "rmet","RMET.tr" = "rmet.rt", "cambio atencional" = "attention switching","atención a detalles" = "attention to detail",
                "comunicación"  = "communication", "imaginación" = "imagination", "habilidades sociales" = "social skill",
                "Historia corta espontánea" = "SST.Spontaneous","Historia corta explícita" = "SST.Explicit", "Historia corta comprensión" = "SST.Comprehension",
                "Historia corta total" = "SST") %>% 
  write_csv(.,"../../../general/exp3.context.results_by_participant.spanish.csv")