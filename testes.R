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
#                 n_casos_sifg_sifg = CASOS_CONF)


# Leitura de dados e configurações iniciais ----

## Dados RDS ----
df_sifilis <- readRDS("data-raw/df_sifilis_all.RDS")

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
  dplyr::summarise(n_casos_sifg = sum(n_casos_sifg_sifg))

f_fx_et_limits <- c("10-14", "15-19", "20-39", "40-59", "60-64",
                    "65-69", "70-79", "80+", "Em branco/Inválido")

graf_fx_etaria <- tabela_fx_etaria |>
  ggplot(aes(x = faixa_etaria, y = n_casos_sifg)) +
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
  dplyr::summarise(n_casos_sifg = sum(n_casos_sifg))

racacor_limits <- c("Amarela", "Branca", "Indigena", "Parda", "Preta", "Em branco/Inválido")

graf_raca <- tabela_raca |>
  ggplot(aes(x = raca, y = n_casos_sifg)) +
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
  dplyr::summarise(n_casos_sifg = sum(n_casos_sifg))

esc_limits <- c("Analfabeto", "1ª a 4ª série incompleta do EF", "4ª série completa do EF",
                "5ª à 8ª série incompleta do EF",
                "Ensino fundamental completo",
                "Ensino médio incompleto", "Ensino médio completo",
                "Educação superior incompleta",
                "Educação superior completa",
                "Não se aplica", "Em branco/Inválido")

graf_esc <- tabela_esc |>
  ggplot(aes(x = escolaridade, y = n_casos_sifg)) +
  geom_col() +
  labs(title = "Distribuição de casos de Sífilis por escolaridade",
       x = "Escolaridade",
       y = "Número de casos") +
  theme_minimal() +
  scale_y_continuous(labels = scales::number) +
  scale_x_discrete(limits = esc_limits)

graf_esc

## Agrupar superior completo/incompleto, "Não se aplica" e "Em branco/Inválido"

# Índices de desigualdades Socioeconômicos ----
## IBP Decil e Quintil
typeof(df_sifilis$IBP_DECIL)
hist(df_sifilis$IBP_DECIL)
summary(df_sifilis$IBP_DECIL)
unique(df_sifilis$IBP_DECIL)



typeof(df_sifilis$IBP_QUINTIL)
hist(df_sifilis$IBP_QUINTIL)
summary(df_sifilis$IBP_QUINTIL)
unique(df_sifilis$IBP_QUINTIL)

IBP_QUINTIL_limits <- c("1", "2", "3", "4", "5", "Em branco/Inválido")

## IDHM (2010)
unique(df_sifilis$IDHM_2010)
hist(df_sifilis$IDHM_2010)
summary(df_sifilis$IDHM_2010)

IDHM_2010_limits <- c("Baixo", "Médio", "Alto", "Muito Alto", "Em branco/Inválido")
## Baixo < 0,5; Médio 0,5 - 0,799; Alto 0,8 - 0,899; Muito Alto >= 0,9

## IDHM Renda (2010)
unique(df_sifilis$IDHM_RENDA_2010)
hist(df_sifilis$IDHM_RENDA_2010)
summary(df_sifilis$IDHM_RENDA_2010)

IDHM_RENDA_2010_limits <- c("Baixo", "Médio", "Alto", "Muito Alto", "Em branco/Inválido")
## Baixo < 0,5; Médio 0,5 - 0,799; Alto 0,8 - 0,899; Muito Alto >= 0,9

## IDHM Longevidade (2010)

IDHM_RENDA_LONG_limits <- c("Baixo", "Médio", "Alto", "Muito Alto", "Em branco/Inválido")
## Baixo < 0,5; Médio 0,5 - 0,799; Alto 0,8 - 0,899; Muito Alto >= 0,9

## IDHM Educação (2010)
IDHM_RENDA_ED_limits <- c("Baixo", "Médio", "Alto", "Muito Alto", "Em branco/Inválido")
## Baixo < 0,5; Médio 0,5 - 0,799; Alto 0,8 - 0,899; Muito Alto >= 0,9

## PIB percapita (2010)
typeof(df_sifilis$PIBPERCAPITA_MIL_2010)
hist(df_sifilis$PIBPERCAPITA_MIL_2010)
summary(df_sifilis$PIBPERCAPITA_MIL_2010)

## PIB milhão (2010)
typeof(df_sifilis$PIB_MILHAO_2010)
hist(df_sifilis$PIB_MILHAO_2010)
summary(df_sifilis$PIB_MILHAO_2010)

## Renda Gini (2010)
typeof(df_sifilis$RENDA_GINI_2010)
hist(df_sifilis$RENDA_GINI_2010)
summary(df_sifilis$RENDA_GINI_2010)

## Gráficos iniciais bivariada ----
### Mapa de distribuição dos casos por UF por raça/cor ----
tabela_uf_racacor <- df_sifilis |>
  dplyr::group_by(uf_res, raca) |>
  dplyr::summarise(n_casos_sifg = sum(n_casos_sifg)) |>
  dplyr::ungroup()

## Adicionando linha com "Todas" somadas ao dataframe tabela_uf_racacor

tabela_uf_racacor <- tabela_uf_racacor |>
  dplyr::filter(raca != "Em branco/Inválido") |> # Remover Em branco/Inválido para contagem
  dplyr::group_by(uf_res) |>
  dplyr::summarise(n_casos_sifg = sum(n_casos_sifg)) |>
  dplyr::mutate(raca = "Todas válidas") |>
  dplyr::select(uf_res, raca, n_casos_sifg) |>
  dplyr::ungroup() |>
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

### Mapa de distribuição dos casos por UF por racacor ao longo dos anos ----

tabela_uf_racacor_ano <- df_sifilis |>
  dplyr::group_by(uf_res, raca, ano_diag) |>
  dplyr::summarise(n_casos_sifg = sum(n_casos_sifg)) |>
  dplyr::ungroup()

## Adicionando linha com "Todas" somadas ao dataframe tabela_uf_racacor_ano ----

tabela_uf_racacor_ano <- tabela_uf_racacor_ano |>
  dplyr::filter(raca != "Em branco/Inválido") |> # Remover Em branco/Inválido para contagem
  dplyr::group_by(uf_res, ano_diag) |>
  dplyr::summarise(n_casos_sifg = sum(n_casos_sifg)) |>
  dplyr::mutate(raca = "Todas válidas") |>
  dplyr::select(uf_res, raca, ano_diag, n_casos_sifg) |>
  dplyr::ungroup() |>
  dplyr::bind_rows(tabela_uf_racacor_ano)

graf_uf_racacor_ano <- tabela_uf_racacor_ano |>
  dplyr::filter(raca != "Todas válidas" & ano_diag != "****" ) |> # Remover todas válidas, manter apenas classes individuais
  ggplot(aes(x = ano_diag, y = n_casos_sifg, fill = raca)) +
  geom_col(position = "stack") +
  labs(title = "Distribuição de casos de Sífilis por UF e Raça/cor ao longo dos anos",
       x = "Ano",
       y = "Número de casos",
       fill = "Raça/cor") +
  theme_minimal() +
  scale_y_continuous(labels = scales::number) +
  scale_x_discrete() +
  scale_fill_manual(values = c("Amarela" = "#FFD700", "Branca" = "#E1E1E1", "Indigena" = "brown",
                               "Parda" = "#8B4513", "Preta" = "black", "Todas válidas" = "gray"))

graf_uf_racacor_ano

saveRDS(graf_uf_racacor_ano, "data/graf_i_biv_racacor_ano_stack.RDS")

graf_uf_racacor_ano_todos <- tabela_uf_racacor_ano |>
  dplyr::filter(raca != "Todas válidas" & ano_diag != "****") |> # Remover todas válidas, manter apenas classes individuais
  ggplot(aes(x = ano_diag, y = n_casos_sifg)) +
  geom_col(fill = "#ABA2D1", position = "dodge") +
  labs(title = "Distribuição de casos de Sífilis por UF e Raça/cor ao longo dos anos",
       x = "Ano",
       y = "Número de casos") +
  theme_minimal() +
  scale_y_continuous(labels = scales::number) +
  scale_x_discrete()

# graf_uf_racacor_ano_todos

saveRDS(graf_uf_racacor_ano_todos, "data/graf_i_biv_racacor_ano_todas.RDS")



# Testando nova base -----
## Base com 13mi

df_sifilis_13mi <- readRDS("data-raw/df_sifilis_all.RDS") |>
  janitor::clean_names()

df_sifilis_13mi_g <- df_sifilis_13mi |>
  dplyr::mutate(taxa_sifg_nv = casos_sifg / nascidos_vivos) |>
  dplyr::group_by(id_mn_resi, ano, mes, fx_etaria) |>
  dplyr::summarise(n_casos_sifg = sum(casos_sifg),
                   nasc_viv = sum(nascidos_vivos),
                   nasc_c_anom = sum(nascidos_com_anomalia),
                   taxa_sifg_nv = sum(casos_sifg) / sum(nascidos_vivos)) |>
  dplyr::distinct(id_mn_resi, ano, mes, fx_etaria) |>
  dplyr::ungroup()

## group_by id_mn_resi, ano, mes, fx_etaria, cs_raca, escolaridade
## rows -> 13078512

## group_by id_mn_resi, ano, mes, fx_etaria, cs_raca
## rows -> 6849252


## group_by id_mn_resi, ano, mes, fx_etaria
## rows -> 4369221








































