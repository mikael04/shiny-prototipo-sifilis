#' graf_iniciais
#'
#' @description Uma função que gera os gráficos iniciais do painel
#'
#' @return T se ok, F se teve algum problema
#'
#' @author 'Mikael M. Coletto'
#'
#' @date: 10/05/2024

func_graf_iniciais <- function(df_sifilis){
  # Dados iniciais ----
  ## Dados RDS ----
  df_sifilis <- readRDS("data-raw/df_sifilis_mun_ano_fx_et_racacor_esc.RDS")
  ## Eixos para faixa etária, raça/cor e escolaridade ----
  f_fx_et_limits <- c("10-14", "15-19", "20-24", "25-29", "30-39",
                      "40-59", "60+", "Em branco/Inválido")
  racacor_limits <- c("Amarela", "Branca", "Indigena", "Parda", "Preta", "Em branco/Inválido")
  esc_limits <- c("Analfabeto", "1ª a 4ª série incompleta do EF", "4ª série completa do EF",
                  "5ª à 8ª série incompleta do EF",
                  "Ensino fundamental completo",
                  "Ensino médio incompleto", "Ensino médio completo",
                  "Educação superior incompleta",
                  "Educação superior completa",
                  "Em branco/Inválido")

  ## Dados espaciais UFs ----
  uf_sf <- sf::read_sf(here::here("data-raw/dados-espaciais/uf_sf.shp")) |>
    dplyr::select(cod_stt, geometry)  |>
    dplyr::mutate(cod_stt = as.character(cod_stt))


  df_ufs <- data.table::fread("data-raw/ibge-ufs-pop-2022-est.csv") |>
    dplyr::mutate(uf_cod = as.character(uf_cod))

  df_ufs <- left_join(df_ufs, uf_sf, by = c("uf_cod" = "cod_stt")) |>
    sf::st_as_sf()

  df_ufs <- sf::st_transform(df_ufs, crs = '+proj=longlat
+datum=WGS84')

  # Gráficos iniciais ----
  ## Bivariada ----
  ### Mapa de distribuição dos casos por UF por raça/cor ----
  tabela_uf_racacor <- df_sifilis |>
    dplyr::group_by(uf_res, raca) |>
    dplyr::summarise(n_casos_sifg = sum(n_casos_sifg)) |>
    dplyr::ungroup()

  ## Adicionando linha com "Todas" somadas ao dataframe tabela_uf_racacor

  tabela_uf_racacor <- tabela_uf_racacor |>
    dplyr::group_by(uf_res) |>
    dplyr::summarise(n_casos_sifg = sum(n_casos_sifg)) |>
    dplyr::mutate(raca = "Todas") |>
    dplyr::select(uf_res, raca, n_casos_sifg) |>
    dplyr::bind_rows(tabela_uf_racacor)

  df_uf_racacor <- tabela_uf_racacor |>
    dplyr::left_join(df_ufs, by = c("uf_res" = "uf_cod")) |>
    sf::st_as_sf()

  df_uf_racacor <- sf::st_transform(df_uf_racacor, crs = '+proj=longlat
+datum=WGS84')

  uf_racacor_map <- df_uf_racacor |>
    dplyr::filter(raca == "Todas") |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = n_casos_sifg)) +
    # scale_fill_viridis_c() +
    ggplot2::labs(title = "Distribuição de casos de Sífilis por UF e Raça/cor: Todas",
                  fill = "Número de casos") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_gradient2() +
    ggplot2::guides(fill = guide_legend(theme = theme(
      legend.title = element_text(size = 15, face = "bold", colour = "black")
    )))

  uf_racacor_map

  saveRDS(df_uf_racacor, "data/graf_i_biv_df_mapa_uf_racacor.RDS")
}
