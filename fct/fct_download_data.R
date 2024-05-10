#' func_donwload_data
#'
#' @description Download dos dados do bigquery para trabalhar localmente
#'
#' @return T se ok, F se teve algum problema
#'
#' @author 'Mikael M. Coletto'
#'
#' @date: 08/05/2024
#'
func_download_data <- function(){
  # Library ----
  library(dplyr)
  library(dbplyr)
  library(tidyr)
  ## Comunicação com o banco de dados
  library(bigrquery)
  library(DBI)

  # Bigrquery ----
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "pdi-covid-basededados",
    dataset = "sifilis_gestantes"
  )
  df_sifilis <- tbl(con, "view_agregado_sifilis_gestantes_a_partir_2007_mikael") |>
    dplyr::collect() |>
    dplyr::rename(uf_res = `_UF_RESI`, mun_res = ID_MN_RESI,
                  ano_diag = `_ANO_DIAG`, mes_diag = `_MES_DIAG`,
                  faixa_etaria = `_FX_ETARIA`, raca = `_CS_RACA`, escolaridade = `_ESCOLARIDADE`,
                  n_casos = CASOS_CONF)

  saveRDS(df_sifilis, "data-raw/df_sifilis.RDS")
}

