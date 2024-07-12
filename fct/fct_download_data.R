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
  ) # gestantes / n de nasc vivos no município
  df_sifilis <- tbl(con, "view_agregado_sifilis_gestantes_a_partir_2007") |>
    # dplyr::select(`UF_RESI`, ID_MN_RESI,
    #               `CASOS_SIFG`, `NASCIDOS_VIVOS`, `NASCIDOS_COM_ANOMALIA`,
    #               `ANO`,
    #               `FX_ETARIA`, `CS_RACA`, `ESCOLARIDADE`, CASOS_SIFG,
    #               ## Indicadores de desigualdade
    #               IBP_QUINTIL, IDHM_2010) |>
    # dplyr::rename(uf_res = `UF_RESI`, mun_res = ID_MN_RESI,
    #               casos_sifg = `CASOS_SIFG`, nasc_viv =  `NASCIDOS_VIVOS`,
    #               nasc_c_anom = `NASCIDOS_COM_ANOMALIA`,
    #               ano_diag = `ANO`,
    #               faixa_etaria = `FX_ETARIA`, raca = `CS_RACA`, escolaridade = `ESCOLARIDADE`,
    #               n_casos_sifg = CASOS_SIFG) |>
    dplyr::collect()

  # data.table::fwrite(df_sifilis, "data-raw/df_sifilis_all.csv")

  ## Calculando taxa de sífilis por nascidos vivos
  df_sifilis <- df_sifilis |>
    dplyr::mutate(taxa_sifg_nv = CASOS_SIFG / NASCIDOS_VIVOS)

  # colnames(df_sifilis)
  saveRDS(df_sifilis, "data-raw/df_sifilis_all_agreg.RDS")

  # df_sifilis_ <- readRDS("data-raw/df_sifilis.RDS")

  ## Dado não agregado

  df_sifilis <- tbl(con, "view_sifilis_gestantes_a_partir_2007") |>
    dplyr::collect()


  saveRDS(df_sifilis, "data-raw/df_sifilis_all_n_agreg.RDS")

}

#município de residência, ano do diagnóstico, mês de diagnóstico, faixa etária, raça/cor e escolaridade  (variável CASOS_SIFG);

