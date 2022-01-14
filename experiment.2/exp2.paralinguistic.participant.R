# setup -------------------------------------------------------------------
library(tidyverse)
library(rstatix)
library(corrplot)
library(formattable)
library(webshot)
library(htmltools)
library(ggstatsplot)
library(webr)
setwd("/home/eli/Desktop/Neurobiologia/projects/ongoing/verbal.irony/facilitators/experiment.2/")

# data --------------------------------------------------------------------
experiment.2.by.participant <- read_csv("general/exp2.paralinguistic.results_by_participant.csv")

# descriptives ------------------------------------------------------------
descriptives.participantes <- experiment.2.by.participant %>% 
  dplyr::select(7:27) %>% 
  numSummary() %>% 
  mutate_if(is.numeric, round, 2) %>% 
  write_csv(., "results/descriptives.participants.csv")

# differences by sex ------------------------------------------------------
sex <- experiment.2.by.participant %>% 
  gather(7:27, key = "task", value = "value") %>% 
  dplyr::group_by(task) %>% 
  t_test(value ~ sex) %>% 
  adjust_pvalue(method = "bonferroni") %>% 
  mutate(p.adj = round(p.adj,3)) %>% 
  write_csv(., "results/t_test-sex.csv") 

degree_sex <- experiment.2.by.participant %>% 
  gather(7:27, key = "task", value = "value") %>% 
  dplyr::group_by(task) %>% 
  anova_test(value ~ group*sex) %>% 
  adjust_pvalue(method = "bonferroni") %>% 
  mutate(p.adj = round(p.adj,3)) %>% 
  write_csv(., "results/anova-degree*sex.csv") 

# plots -------------------------------------------------------------------
correlations <- experiment.2.by.participant %>% 
  dplyr::select(7:27) %>% 
  cor_mat(method = "pearson") %>% 
  cor_gather() %>% 
  adjust_pvalue(method = "fdr") %>% 
  mutate(p = round(p, 2), p.adj = round(p.adj, 2)) %>% 
  write_csv(.,"results/correlations.csv")

jpeg("plots/exp2.corrplot.jpeg", width=1280,height=1000)
if(require("ggpubr")){
  my.palette <- get_palette("BrBG", k = 10)
  experiment.2.by.participant %>% 
    dplyr::select(7:27) %>% 
    cor_mat(method = "pearson") %>% 
    cor_plot(method = "color", type = "upper", 
             p.mat = p.mat.c$p.adj,
             palette = my.palette)}
dev.off()

experiment.2.by.participant %>% 
  dplyr::select(7:27) %>% 
  ggcorrmat(matrix.type = "lower")
ggsave("plots/exp2.pieplot.answer.ggstatsplot.jpg", width = 15, height = 15)

# spanish corrplot --------------------------------------------------------
experiment.2.by.participant %>% 
  gather(c(7,9,11,13,15,17), key = "key", value = "score") %>%
  separate(key, into = c("expName", "condition")) %>% 
  dplyr::mutate(condition = dplyr::recode(condition,"Irony"  = "Ironía", "Unrelated" = "Sin relación")) %>%
  dplyr::mutate(expName = dplyr::recode(expName,"prosody"  = "Prosodia",
                                        "FE" = "Expresión facial")) %>%
  grouped_ggbetweenstats(condition, score, grouping.var = "expName",  
                         p.adjust.method = "bonferroni",
                         bf.message = F, results.subtitle = F,
                         ylab = "Porcentaje de clasificación",
                         xlab = "Condición")
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp2.violin.participante.spanish.jpg", width = 9, height = 5)
ggsave("plots/exp2.violin.participante.spanish.jpg", width = 9, height = 5)

spanish <- read_csv("general/exp2.context.results_by_participant.spanish.csv")
correlations.spanish <- spanish %>% 
  dplyr::select(7:27) %>% 
  cor_mat(method = "pearson") %>% 
  cor_gather() %>% 
  adjust_pvalue(method = "fdr") %>% 
  mutate(p = round(p, 2), p.adj = round(p.adj, 2)) 

jpeg("plots/exp2.corrplot.spanish.jpeg", width=1280,height=1000)
if(require("ggpubr")){
  my.palette <- get_palette("BrBG", k = 10)
  spanish %>% 
    dplyr::select(7:27) %>% 
    cor_mat(method = "pearson") %>% 
    cor_plot(method = "color", type = "upper", 
             p.mat = p.mat.c$p.adj,
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
  gather(19:27, key = "Prueba", value = "score") %>% 
  select(19:20) %>% 
  dplyr::group_by(Prueba) %>% 
  numSummary() %>%
  mutate(Prueba = factor(Prueba, levels = c("RMET","RMET.tr", "cambio atencional","atención a detalles",
                                            "comunicación", "imaginación", "habilidades sociales", "AQ","SSS"))) %>% 
  arrange(Prueba) %>% 
  select(-2,-7,-6,-13) %>% 
  dplyr::rename(media = mean, DE = sd, mediana = median, rango = range)

table.formattable <- formattable(tabla, digits = 2)
export_formattable(table.formattable, "plots/exp2.resultados.psicometricos.png")
export_formattable(table.formattable, "/home/eli/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp2.resultados.psicometricos.png")
