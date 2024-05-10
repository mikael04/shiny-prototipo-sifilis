#' Script principal
#'
#' @description Script para donwload de dados e geração de gráficos iniciais
#'
#' @return
#'
#' @author 'Mikael M. Coletto'
#'
#' @date: 10/05/2024

# Library ----
library(dplyr) ## Manipulação de dados
library(dbplyr) ## Manipulação de dados (db)
library(bigrquery) ## Comunicação com o banco de dados
library(ggplot2) ## Gráficos

# Source ----
source("fct/fct_download_data.R")
source("fct/fct_graf_iniciais.R")

# Download dos dados ----
func_download_data()

# Gerando gráficos iniciais ----

df_sifilis <- readRDS("data-raw/df_sifilis.RDS")
## Função salvando com problemas de salvar com tamanhos muito maiores
## Não utilizar a função
# func_graf_iniciais(df_sifilis)
