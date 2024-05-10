# Library ----
## Gráficos
library(ggplot2)
## Manipulação de dados
library(dplyr)
library(dbplyr)
library(tidyr)
## Comunicação com o banco de dados
library(bigrquery)
library(DBI)

# con <- dbConnect(
#   bigrquery::bigquery(),
#   project = "pdi-covid-basededados",
#   dataset = "sifilis_gestantes"
# )
# df_sifilis <- tbl(con, "view_agregado_sifilis_gestantes_a_partir_2007") |>
#   dplyr::collect() |>
#   dplyr::rename(uf_res = `_UF_RESI`, mun_res = ID_MN_RESI,
#                 ano_diag = `_ANO_DIAG`, mes_diag = `_MES_DIAG`,
#                 faixa_etaria = `_FX_ETARIA`, raca = `_CS_RACA`, escolaridade = `_ESCOLARIDADE`,
#                 n_casos = CASOS_CONF)

# Leitura de dados e configurações iniciais ----

## Dados RDS ----
df_sifilis <- readRDS("data-raw/df_sifilis.RDS")

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
                "Não se aplica", "Em branco/Inválido")

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

# Filtros iniciais ----

f_anos <- df_sifilis |>
  dplyr::select(ano_diag) |>
  dplyr::distinct(ano_diag) |>
  dplyr::filter(ano_diag != "****")

f_meses <- df_sifilis |>
  dplyr::select(mes_diag) |>
  dplyr::distinct(mes_diag) |>
  dplyr::filter(mes_diag != "**")

# Exploração de dados ----
## Distribuição por faixa etária ----
tabela_fx_etaria <- df_sifilis |>
  dplyr::group_by(faixa_etaria) |>
  dplyr::summarise(n_casos = sum(n_casos))

f_fx_et_limits <- c("10-14", "15-19", "20-39", "40-59", "60-64",
                    "65-69", "70-79", "80+", "Em branco/Inválido")

graf_fx_etaria <- tabela_fx_etaria |>
  ggplot(aes(x = faixa_etaria, y = n_casos)) +
  geom_col() +
  labs(title = "Distribuição de casos de Sífilis por faixa etária",
       x = "Faixa etária",
       y = "Número de casos") +
  theme_minimal() +
  scale_y_continuous(labels = scales::number) +
  scale_x_discrete(limits = f_fx_et_limits)

graf_fx_etaria

## Refazer as classificações estárias, possivelmente dividindo o grupo de 20-39 em dois ou quatro subgrupos
## Agrupar idades 60+ em um único grupo

## Distribuição por raça/cor ----
tabela_raca <- df_sifilis |>
  dplyr::group_by(raca) |>
  dplyr::summarise(n_casos = sum(n_casos))

racacor_limits <- c("Amarela", "Branca", "Indigena", "Parda", "Preta", "Em branco/Inválido")

graf_raca <- tabela_raca |>
  ggplot(aes(x = raca, y = n_casos)) +
  geom_col() +
  labs(title = "Distribuição de casos de Sífilis por raça/cor",
       x = "Raça/cor",
       y = "Número de casos") +
  theme_minimal() +
  scale_y_continuous(labels = scales::number) +
  scale_x_discrete(limits = racacor_limits)

graf_raca

## Distribuição por escolaridade ----
tabela_esc <- df_sifilis |>
  dplyr::group_by(escolaridade) |>
  dplyr::summarise(n_casos = sum(n_casos))

esc_limits <- c("Analfabeto", "1ª a 4ª série incompleta do EF", "4ª série completa do EF",
                "5ª à 8ª série incompleta do EF",
                "Ensino fundamental completo",
                "Ensino médio incompleto", "Ensino médio completo",
                "Educação superior incompleta",
                "Educação superior completa",
                "Não se aplica", "Em branco/Inválido")

graf_esc <- tabela_esc |>
  ggplot(aes(x = escolaridade, y = n_casos)) +
  geom_col() +
  labs(title = "Distribuição de casos de Sífilis por escolaridade",
       x = "Escolaridade",
       y = "Número de casos") +
  theme_minimal() +
  scale_y_continuous(labels = scales::number) +
  scale_x_discrete(limits = esc_limits)

graf_esc

## Agrupar superior completo/incompleto, "Não se aplica" e "Em branco/Inválido"


