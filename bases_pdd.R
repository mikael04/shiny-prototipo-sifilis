#' @title Avaliacao de Bases PDD
#'
#' @description Script para manipular as tabelas do gdrive e verificar inconsistências e problemas
#'
#' @return
#'
#' @author 'Mikael M. Coletto'
#'
#' @date: 09/07/2024
#'

library(dplyr)
## Lendo do google sheets
library(googlesheets4)
## Lendo dados do bigquery
library(bigrquery)
library(dbplyr)
library(DBI)


# PDD - Google Sheets ----
relacao_bq_pdd_dv <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/13Z_PfIVhxJY_9f2TIVIwL3ip1NYM0Almg_6kQoa8hto", sheet = "Relacao_BQ_PDD_DataVerse_All")

relacao_bq_pdd_dv <- janitor::clean_names(relacao_bq_pdd_dv) |>
  tidyr::as_tibble() |>
  dplyr::mutate(bigquery_tabelas_views = as.character(bigquery_tabelas_views),
                replication = as.character(replication))

  ## Número de datasets no BigQuery (contando os "válidos")
length(unique(relacao_bq_pdd_dv$bigquery_dataset))-1 ## -1 para desconsiderar o valor NA

## Número de tabelas e views no BigQuery
length(unique(relacao_bq_pdd_dv$bigquery_tabelas_views))

## Verificar pelos nomes e bases que estão na PDD (possuem link)
length(unique(relacao_bq_pdd_dv$pdd))-2 ## -2 para desconsiderar os valores NA e -
length(unique(relacao_bq_pdd_dv$nome))-4 ## -4 para desconsiderar os valores NA, "-", Não encontrado e "Verificar se deveriam estar na PDD"

## Verificar se tem tabelas que estão na PDD e não estão no BigQuery
pdd_naoBq_dv <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/13Z_PfIVhxJY_9f2TIVIwL3ip1NYM0Almg_6kQoa8hto", sheet = "PDD_nãoBQ_DV")
