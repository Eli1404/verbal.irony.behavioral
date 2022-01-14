# setup -------------------------------------------------------------------
library(tidyverse)
library(rstatix)
library(corrplot)
library(formattable)
library(webshot)
library(htmltools)
library(ggstatsplot)
library(webr)
setwd("/home/eli/Desktop/Neurobiologia/projects/ongoing/verbal.irony/facilitators/experiment.3/")

# data --------------------------------------------------------------------
experiment.3.by.participant <- read_csv("general/exp3.paralinguistic.results_by_participant.csv") 

# descriptives ------------------------------------------------------------
descriptives.participantes <- experiment.3.by.participant %>% 
  ungroup() %>% 
  dplyr::select(4:34) %>% 
  numSummary(.) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  write_csv(., "results/descriptives.participants.csv")

# differences by sex ------------------------------------------------------
sex <- experiment.3.by.participant %>% 
  filter(rmet != "NA") %>% 
  rename_at(2, ~"sex") %>% 
  mutate(sex= recode(sex, `MÃ¡sculino` = "Masculino")) %>% 
  gather(4:34, key = "task", value = "value") %>% 
  group_by(task) %>% 
  t_test(value ~ sex) %>% 
  write_csv(., "results/t_test-sex.csv") 

# plots -------------------------------------------------------------------
correlations <- experiment.3.by.participant %>% 
  dplyr::select(4:34) %>% 
  cor_mat(method = "pearson") %>% 
  cor_gather() %>% 
  adjust_pvalue(method = "fdr") %>% 
  mutate(p = round(p, 2), p.adj = round(p.adj, 2)) %>% 
  write_csv(.,"results/correlations.csv")

jpeg("plot/exp3.corrplot.jpeg", width=1280,height=1000)
if(require("ggpubr")){
  my.palette <- get_palette("BrBG", k = 10)
  experiment.3.by.participant %>% 
    dplyr::select(7:27) %>% 
    cor_mat(method = "pearson") %>% 
    cor_plot(method = "color", type = "upper", 
             p.mat = p.mat.c$p.adj,
             palette = my.palette)}
dev.off()

# spanish corrplot --------------------------------------------------------
experiment.3.by.participant %>% 
  gather(c(4,6,8,10,12,14,16,18,20), key = "key", value = "score") %>%
  separate(key, into = c("expName", "condition")) %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Irony"  = "Ironía", "Unrelated" = "Sin relación")) %>%
  dplyr::mutate(expName = dplyr::recode(expName,"prosody"  = "Prosodia",
                                        "FE" = "Expresión facial")) %>%
  grouped_ggbetweenstats(condition, score, grouping.var = "expName",  
                         p.adjust.method = "bonferroni",
                         bf.message = F, results.subtitle = F,
                         ylab = "Porcentaje de clasificación",
                         xlab = "Condición")
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp3.violin.participante.spanish.jpg", width = 9, height = 5)
ggsave("plot/exp3.violin.participante.spanish.jpg", width = 9, height = 5)

spanish <- read_csv("general/exp3.context.results_by_participant.spanish.csv")
correlations.spanish <- spanish %>% 
  dplyr::select(4:34) %>% 
  cor_mat(method = "pearson") %>% 
  cor_gather() %>% 
  adjust_pvalue(method = "fdr") %>% 
  mutate(p = round(p, 2), p.adj = round(p.adj, 2)) 

jpeg("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp3.corrplot.spanish.borrar.jpeg", width=1280,height=1000)
if(require("ggpubr")){
  my.palette <- get_palette("BrBG", k = 10)
  spanish %>% 
    dplyr::select(4:34) %>% 
    cor_mat(method = "pearson") %>% 
    cor_plot(method = "color", type = "upper", 
             palette = my.palette, font.label = list(size = 4))}
dev.off()

# formattable -------------------------------------------------------------
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
  }

# tabla -------------------------------------------------------------------
tabla <- spanish %>%
  gather(22:34, key = "Prueba", value = "score") %>% 
  select(22:23) %>% 
  group_by(Prueba) %>% 
  numSummary() %>% 
  mutate(Prueba = factor(Prueba, levels = c("RMET","RMET.tr", "cambio atencional","atención a detalles",
                                            "comunicación", "imaginación", "habilidades sociales","AQ" = "AQ.score",
                                            "Historia corta espontánea","Historia corta explícita", 
                                            "Historia corta comprensión","Historia corta total",
                                            "SSS"))) %>% 
  arrange(Prueba) %>% 
  select(-2,-7,-6,-13) %>% 
  dplyr::rename(media = mean, DE = sd, mediana = median, rango = range)

table.formattable <- formattable(tabla, digits = 2)
export_formattable(table.formattable, "plot/exp3.resultados.psicometricos.png")
export_formattable(table.formattable, "/home/eli/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp3.resultados.psicometricos.png")
