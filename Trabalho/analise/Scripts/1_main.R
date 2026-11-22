# ALASKA AIRLINES

setwd("C:/Users/Richard Amarante/Documents/AnaliseDeSeriesTemporais/Código do Projeto Final")

options(scipen = 999) # Tirar notação científica

library(jsonlite)
library(ggplot2)
library(scales)
library(urca)
library(writexl)

parametros <- fromJSON("parametros.json")

source("Scripts/2_dados.R", encoding = "UTF-8")
source("Scripts/3_estatisticas.R", encoding = "UTF-8", print.eval = TRUE)
source("Scripts/4_analise_descritiva.R", encoding = "UTF-8", print.eval = TRUE)
source("Scripts/SARIMA/1_analise_descritiva.R", encoding = "UTF-8", print.eval = TRUE)
source("Scripts/SARIMA/2_sarima.R", encoding = "UTF-8", print.eval = TRUE)
source("Scripts/AR/1_analise_descritiva.R", encoding = "UTF-8", print.eval = TRUE)
source("Scripts/AR/2_ar.R", encoding = "UTF-8", print.eval = TRUE)
source("Scripts/SARIMA/3_analise_descritiva_passageiros.R", encoding = "UTF-8", print.eval = TRUE)
source("Scripts/SARIMA/4_airline.R", encoding = "UTF-8", print.eval = TRUE)
source("Scripts/5_resultados.R", encoding = "UTF-8", print.eval = TRUE)
