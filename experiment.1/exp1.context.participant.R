# setup -------------------------------------------------------------------
library(tidyverse)
library(rstatix)
library(webr)
library(ggstatsplot)
library(ggcorrplot)
library(formattable)
library(webshot)
library(htmltools)
setwd("~/Desktop/Neurobiologia/projects/ongoing/verbal.irony/discrepancy/experiment.1/")

# reading data ------------------------------------------------------------
experiment.1.by.participant <- read_csv("general/exp1.context.results_by_participant.csv")

# descriptives ------------------------------------------------------------
descriptives.participantes <- experiment.1.by.participant %>% 
  ungroup() %>% 
  dplyr::select(7:33) %>% 
  numSummary(.) %>% 
  write_csv(., "results/descriptives.participants.csv")

# inference ---------------------------------------------------------------
sex <- experiment.1.by.participant %>% 
  gather(7:33, key = "task", value = "value") %>% 
  dplyr::group_by(task) %>% 
  t_test(value ~ sex) %>% 
  adjust_pvalue(method = "bonferroni") %>% 
  mutate(p.adj = round(p.adj,3)) %>% 
  write_csv(., "results/t_test-sex.csv") 

degree_sex <- experiment.1.by.participant %>% 
  gather(7:33, key = "task", value = "value") %>% 
  dplyr::group_by(task) %>% 
  anova_test(value ~ group*sex) %>% 
  adjust_pvalue(method = "bonferroni") %>% 
  mutate(p.adj = round(p.adj,3)) %>% 
  write_csv(., "results/anova-degree*sex.csv") 

# correlation -------------------------------------------------------------
correlations <- experiment.1.by.participant %>% 
  dplyr::select(7:33) %>% 
  cor_mat(method = "pearson") %>% 
  cor_gather() %>% 
  adjust_pvalue(method = "fdr") %>% 
  mutate(p = round(p, 2), p.adj = round(p.adj, 2)) %>% 
  write_csv(.,"results/correlations.csv")

jpeg("plots/exp1.corrplot.jpeg", width=1280,height=800)
if(require("ggpubr")){
  my.palette <- get_palette("BrBG", k = 10)
  experiment.1.by.participant %>% 
    dplyr::select(7:33) %>% 
    cor_mat(method = "pearson") %>% 
    cor_plot(method = "color", type = "upper", 
             p.mat = correlations$p.adj,
             palette = my.palette)
  }
dev.off()

# spanish correlation -----------------------------------------------------
spanish <- read_csv("general/exp1.context.results_by_participant.spanish.csv")

correlations.spanish <- spanish %>% 
  dplyr::select(7:33) %>% 
  cor_mat(method = "pearson") %>% 
  cor_gather() %>% 
  adjust_pvalue(method = "fdr") %>% 
  mutate(p = round(p, 2), p.adj = round(p.adj, 2))

jpeg("/home/eli/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp1.corrplot.spanish.jpeg", width=1280,height=1000)
if(require("ggpubr")){
  my.palette <- get_palette("BrBG", k = 10)
  spanish %>% 
    dplyr::select(7:33) %>% 
    cor_mat(method = "pearson") %>% 
    cor_plot(method = "color", type = "upper", 
             p.mat = correlations.spanish$p.adj,
             palette = my.palette)}
dev.off()

score.violin <-experiment.1.by.participant.spanish %>% 
  gather(c(9,13,17,21), key = "key", value = "puntuación") %>%
  separate(key, into = c("condición"), sep = "_") %>% 
  mutate(condición = recode(condición, "Ironía puntuación" = "Ironía" ,
                            "Literal puntuación" = "Literal",
                            "Sin relación puntuación" = "Sin relación",
                            "Mentira puntuación" = "Mentira")) %>%
  mutate(condición = fct_relevel(condición, c("Ironía", "Literal", "Sin relación","Mentira"))) %>%
  ggbetweenstats(condición, puntuación,   
                         p.adjust.method = "bonferroni",
                         bf.message = F, results.subtitle = F,
                         ylab = "Porcentaje de clasificación",
                         xlab = "Condición", title = "a.")

rt.violin <- experiment.1.by.participant.spanish %>% 
  gather(c(8,12,16,20), key = "key", value = "puntuación") %>%
  separate(key, into = c("condición"), sep = "_") %>%
  mutate(condición = recode(condición, "Ironía clasificación tiempo" = "Ironía" ,
                            "Literal clasificación tiempo" = "Literal",
                            "Sin relación clasificación tiempo" = "Sin relación",
                            "Mentira clasificación tiempo" = "Mentira")) %>%
  mutate(condición = fct_relevel(condición, c("Ironía", "Literal", "Sin relación","Mentira"))) %>%
  ggbetweenstats(condición, puntuación,   
                 p.adjust.method = "bonferroni",
                 bf.message = F, results.subtitle = F,
                 ylab = "Tiempo de clasificación",
                 xlab = "Condición", title = "b.")

ggarrange(score.violin, rt.violin)
ggsave("~/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp1.violin.participante.spanish.jpg", width = 13, height = 5)
ggsave("plots/exp1.violin.participante.spanish.jpg", width = 13, height = 5)

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
  gather(c(8,9,12,13,16,17,20,21,23:33), key = "Prueba", value = "score") %>% 
  select(15,16) %>% 
  group_by(Prueba) %>% 
  numSummary() %>% 
  mutate(Prueba = factor(Prueba, levels = c("Ironía puntuación", "Literal puntuación", 
                                            "Sin relación puntuación","Mentira puntuación",
                                            "Ironía clasificación tiempo","Literal clasificación tiempo",
                                            "Sin relación clasificación tiempo","Mentira clasificación tiempo",
                                            "Matrices progresivas de Raven", "Fluidez verbal",
                                            "Retención de dígitos en progresión", "Retención de dígitos en regresión",
                                            "Retención de dígitos", "Retención de dígitos tiempo",
                                            "Historia corta espontánea" ,"Historia corta explícita",                                            
                                            "Historia corta comprensión", "Historia corta total",
                                            "Diseño de cubos"))) %>% 
  arrange(Prueba) %>% 
  select(-2,-7,-6,-13) %>% 
  rename(media = mean, DE = sd, mediana = median, rango = range)

table.formattable <- formattable(tabla, digits = 1)
export_formattable(table.formattable, "plots/exp1.resultados.psicometricos.png")
export_formattable(table.formattable, "/home/eli/Desktop/Neurobiologia/DoctoradoCienciasBiomedicas/Tesis/plot/exp1.resultados.psicometricos.png")