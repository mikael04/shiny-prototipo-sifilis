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
  ## Eixos para faixa etária, raça/cor e escolaridade ----
  f_fx_et_limits <- c("10-14", "15-19", "20-39", "40-59", "60-64",
                      "65-69", "70-79", "80+", "Em branco/Inválido")
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
  ## Univariada ----

  ### Mapa de distribuição dos casos por UF ----
  tabela_uf <- df_sifilis |>
    dplyr::group_by(uf_res) |>
    dplyr::summarise(n_casos = sum(n_casos))

  df_uf_sel <- tabela_uf |>
    dplyr::left_join(df_ufs, by = c("uf_res" = "uf_cod")) |>
    sf::st_as_sf()

  df_uf_sel <- sf::st_transform(df_uf_sel, crs = '+proj=longlat
+datum=WGS84')

  uf_map <- df_uf_sel |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = n_casos)) +
    # scale_fill_viridis_c() +
    ggplot2::labs(title = "Distribuição de casos de Sífilis por UF",
         fill = "Número de casos") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_gradient2() +
    ggplot2::guides(fill = guide_legend(theme = theme(
      legend.title = element_text(size = 15, face = "bold", colour = "black")
    )))

  # uf_map

  saveRDS(df_uf_sel, "data/graf_i_uni_df_mapa.RDS")
  save(df_uf_sel, file = "data/graf_i_uni_df_mapa.RData")

  ### Distribuição de casos por ano ----
  tabela_anos <- df_sifilis |>
    dplyr::group_by(ano_diag) |>
    dplyr::summarise(n_casos = sum(n_casos)) |>
    dplyr::filter(ano_diag != "****") |>
    dplyr::ungroup()

  graf_ano <- tabela_anos |>
    ggplot2::ggplot() +
    ggplot2::geom_col(aes(x = ano_diag, y = n_casos), fill = "#ABA2D1") +
    ggplot2::labs(title = "Distribuição de casos de Sífilis por ano",
         x = "Ano",
         y = "Número de casos") +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous(labels = scales::number)

  # graf_ano

  saveRDS(graf_ano, "data/graf_i_uni_ano.RDS")

  ### Distribuição de casos por mês ----
  tabela_ano_mes <- df_sifilis |>
    dplyr::group_by(ano_diag, mes_diag) |>
    dplyr::summarise(n_casos = sum(n_casos)) |>
    dplyr::filter(mes_diag != "**", ano_diag != "****") |>
    dplyr::mutate(ano_mes = as.Date(paste0(ano_diag, "-", mes_diag, "-01"))) |>
    dplyr::ungroup()

  axis_x <- seq.Date(from = min(tabela_ano_mes$ano_mes), to = max(tabela_ano_mes$ano_mes),
                     by = "years")

  graf_ano_mes <- tabela_ano_mes |>
    ggplot2::ggplot() +
    ggplot2::geom_line(aes(x = ano_mes, y = n_casos), color = "#ABA2D1") +
    ggplot2::labs(title = "Distribuição de casos de Sífilis por mês",
         x = "Mês",
         y = "Número de casos") +
    ggplot2::scale_x_date(breaks = axis_x, labels = scales::date_format("%Y")) +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous(labels = scales::number)
  # scale_x_date(date_labels = "%b %Y")

  # graf_ano_mes

  saveRDS(graf_ano_mes, "data/graf_i_uni_ano_mes.RDS")

  ## Raça/cor ----

  ### Mapa ----
  tabela_racacor_mapa <- df_sifilis |>
    dplyr::group_by(raca, uf_res) |>
    dplyr::summarise(n_casos = sum(n_casos)) |>
    dplyr::ungroup()

  df_racacor_mapa <- tabela_racacor_mapa |>
    dplyr::left_join(df_ufs, by = c("uf_res" = "uf_cod")) |>
    sf::st_as_sf()

  df_racacor_mapa <- sf::st_transform(df_racacor_mapa, crs = '+proj=longlat
+datum=WGS84')

  saveRDS(df_racacor_mapa, "data/df_racacor_mapa.RDS")

  racacor_mapa <- df_racacor_mapa |>
    dplyr::filter(raca == "Parda") |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = n_casos)) +
    ggplot2::labs(title = "Distribuição de casos de Sífilis por raça/cor: Parda",
         fill = "Número de casos") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_gradient2() +
    ggplot2::guides(fill = guide_legend(theme = theme(
      legend.title = element_text(size = 15, face = "bold", colour = "black")
    )))

  # racacor_mapa

  ### Distribuição de casos por raça/cor ao longo dos anos ----

  tabela_racacor_ano <- df_sifilis |>
    dplyr::group_by(raca, ano_diag) |>
    dplyr::summarise(n_casos = sum(n_casos)) |>
    dplyr::ungroup()

  graf_racacor_ano <- tabela_racacor_ano |>
    dplyr::filter(raca == "Parda") |>
    ggplot2::ggplot() +
    ggplot2::geom_col(aes(x = ano_diag, y = n_casos, fill = raca)) +
    ggplot2::labs(title = "Distribuição de casos de Sífilis por raça/cor (Parda) ao longo dos anos",
         x = "Ano",
         y = "Número de casos",
         fill = "Raça/cor") +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous(labels = scales::number) +
    ggplot2::scale_fill_manual(values = c("Amarela" = "#FFD700", "Branca" = "#FFFFFF",
                                 "Indigena" = "#8B4513", "Parda" = "#8B4513",
                                 "Preta" = "#000000", "Em branco/Inválido" = "#D3D3D3"))

  graf_racacor_ano

  saveRDS(graf_racacor_ano, "data/graf_i_racacor_ano.RDS")

  ### Distribuição de casos por raça/cor ao longo dos meses ----

  tabela_racacor_mes <- df_sifilis |>
    dplyr::group_by(raca, ano_diag, mes_diag) |>
    dplyr::summarise(n_casos = sum(n_casos)) |>
    dplyr::ungroup()

  tabela_racacor_mes <- tabela_racacor_mes |>
    dplyr::filter(raca == "Parda", mes_diag != "**", ano_diag != "****") |>
    dplyr::mutate(ano_mes = as.Date(paste0(ano_diag, "-", mes_diag, "-01")))

  axis_x <- seq.Date(from = min(tabela_racacor_mes$ano_mes), to = max(tabela_racacor_mes$ano_mes),
                     by = "years")

  graf_racacor_mes <- tabela_racacor_mes |>
    ggplot2::ggplot() +
    ggplot2::geom_line(aes(x = ano_mes, y = n_casos, color = raca)) +
    ggplot2::labs(title = "Distribuição de casos de Sífilis por raça/cor (Parda) ao longo dos meses",
         x = "Mês",
         y = "Número de casos",
         color = "Raça/cor") +
    ggplot2::scale_x_date(breaks = axis_x, labels = scales::date_format("%Y")) +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous(labels = scales::number) +
    ggplot2::scale_color_manual(values = c("Amarela" = "#FFD700", "Branca" = "#FFFFFF",
                                  "Indigena" = "#8B4513", "Parda" = "#8B4513",
                                  "Preta" = "#000000", "Em branco/Inválido" = "#D3D3D3"))

  # graf_racacor_mes

  saveRDS(graf_racacor_mes, "data/graf_i_racacor_mes.RDS")
}
