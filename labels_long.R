#' @title labels_long
#'
#' @description Script para manipular csv de dicionários do cidacs e gerar arquivo de labels no formato csv longo
#'
#' @return T se ok, F se teve algum problema
#'
#' @author 'Mikael M. Coletto'
#'
#' @date: 20/06/2024
#'

library(dplyr)
## Lendo do google sheets
library(googlesheets4)
## Lendo dados do bigquery
library(bigrquery)
library(dbplyr)
library(DBI)

# PDD - Google Sheets ----
## SINAN (Sífilis gestacional) ----
### Bigrquery ----
con <- dbConnect(
  bigrquery::bigquery(),
  project = "pdi-covid-basededados",
  dataset = "sifilis_gestantes"
) # gestantes / n de nasc vivos no município
df_sifilis <- tbl(con, "view_sifilis_gestantes_a_partir_2007") |>
  dplyr::collect()

### Dicionário SINAN gdrive ----
vars_no_bq <- colnames(df_sifilis)
# Carrega o arquivo de dicionário
dic_sinan <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WUo0z1FnH84n8u8F2uI6BpqNtAFz02FXUrd4GP5SEgg", sheet = "sinan_sifilis_gen_2007_a_2018_n") |>
  dplyr::rename(variavel = variable, descricao = description,
                tipo = type, categorias = categories, notas = external_comment)

vars_no_dic <- dic_sinan$variavel

## Verificando variáveis que estão no bq e não estão no dicionário
vars_nao_encontradas <- setdiff(vars_no_bq, vars_no_dic)

## Selecionando variáveis que estão no bq e não foram criadas
vars_nao_encontradas <- vars_nao_encontradas[!startsWith(vars_nao_encontradas, "_")]

## Buscando variáveis no dicionário de labels que estão na base de dados
vars_no_bq_labels <- intersect(vars_no_bq, vars_no_dic)

## Organizando no formato wide o dicionário de labels
dic_sinan_labels <- dic_sinan %>%
  dplyr::filter(variavel %in% vars_no_bq_labels) |>
  dplyr::select(variavel, categorias)

dic_sinan_labels <- dic_sinan_labels |>
  dplyr::mutate(categorias = gsub("\n", ";", categorias)) |>
  dplyr::mutate(categorias = gsub("  ", " ", categorias)) |>
  dplyr::mutate(categorias = gsub("; ", ";", categorias)) |>
  dplyr::mutate(categorias = gsub(" ;", ";", categorias))


separate_labels <- function(variavel, categorias) {
  categorias_df <- data.frame(
    categorias = stringr::str_split(categorias, ";")[[1]],
    stringsAsFactors = FALSE
  )
  return(categorias_df)
}

long_df <- lapply(seq_len(nrow(dic_sinan_labels)), function(i) {
  categorias_df <- separate_labels(dic_sinan_labels[i, "variavel"], dic_sinan_labels[i, "categorias"])
  return(cbind(variavel = dic_sinan_labels[i, "variavel"], categorias_df))
})
long_df <- do.call(rbind, long_df)  # Combine results from each row

### Dicionário resultante ----
labels_sinan_cong <- long_df |>
  dplyr::filter(categorias != "None")

#### Variáveis não encontradas ----
vars_nao_encontradas

#### Variáveis no dicionário mas não na base ----
setdiff(vars_no_dic, vars_no_bq)

# PDD - Dataverse ----
## SINASC ----

## Sinasc 2010-2019
con <- dbConnect(
  bigrquery::bigquery(),
  project = "pdi-covid-basededados",
  dataset = "sinasc"
)
df_sinasc_2010_2019 <- tbl(con, "view_sinasc_2010_2019")

variaveis_sinasc_2010_2019 <- colnames(df_sinasc_2010_2019)
## Selecionando variáveis que estão no bq e não foram criadas
variaveis_sinasc_2010_2019 <- variaveis_sinasc_2010_2019[!startsWith(variaveis_sinasc_2010_2019, "_")]

library(tabulapdf)
library(dplyr)

## Dicionário SINASC 2010-2019 ----
pdf_file <- "data-raw/dicionario_dados_SINASC.pdf.pdf"

get_page_dims(pdf_file)

region <- c(20, 0, 1190.25, 841.50)

mat <- extract_tables(file = pdf_file,
                      pages = 1,
                      guess = FALSE,
                      area = list(region)
)[[1]]

region <- c(180, 40, 841.50, 1190.25)
pag1 <- extract_tables(file = pdf_file,
                      pages = 1,
                      guess = FALSE,
                      area = list(region)
)[[1]]

pag1 <- pag1 |>
  janitor::clean_names() |>
  dplyr::slice(3:nrow(pag1)) |>
  # dplyr::select(-`..5`)
  dplyr::mutate(desc_aux = ifelse(is.na(lead(posicao)), paste0(`x5`, " ", lead(`x5`)), `x5`)) |>
  dplyr::filter(!is.na(posicao)) |>
  dplyr::mutate(descricao = desc_aux) |>
  dplyr::select(-x5, -desc_aux)

colnames <- c("posicao", "nome_campo", "tipo", "tamanho_final", "descricao", "ano_inicial",
              "tipo_inicial", "tamanho_inicial",
              "ano_alteracao_1", "tipo_alteracao_1", "tamanho_alteracao_1",
              "ano_alteracao_2", "tipo_alteracao_2", "tamanho_alteracao_2",
              "ano_alteracao_3", "tipo_alteracao_3", "tamanho_alteracao_3")

colnames(pag1) <- colnames

region <- c(0, 0, 841.50, 1190.25)
pag2 <- extract_tables(file = pdf_file,
                       pages = 2,
                       guess = FALSE,
                       area = list(region)
)[[1]]


region <- c(0, 0 , 1190.25, 841.50)
extract_tables(file = pdf_file,
                       pages = 2,
                       guess = FALSE,
                       area = list(region)
)[[1]]
