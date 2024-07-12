library(dplyr)

## Dados de óbito
dados_quilo_u5 <- data.table::fread("data-raw/dados_poli/df_gr_mun_quilo_obito_u5.csv")

## Dados de óbito no formato wide
dados_quilo_u5_wide <- dados_quilo_u5 |>
  tidyr::pivot_wider(names_from = obito_pop_u5, values_from = count, values_fill = 0)

## Dados de municípios
dados_municipais <- data.table::fread("data-raw/cod_ibge_mun_ufs.csv")

## Dados de municípios com geometria
dados_espaciais <- sf::read_sf(here::here("data-raw/dados-espaciais/municipios_sf.shp")) |>
  dplyr::select(cod_mun = cod, geometry)

df_quilo_u5_map <- dados_espaciais |>
  dplyr::left_join(dados_quilo_u5_wide, by = c("cod_mun" = "codmunres_sinasc")) |>
  dplyr::mutate(across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) |>
  sf::st_as_sf()


df_quilo_u5_map <- sf::st_transform(df_quilo_u5_map, crs = '+proj=longlat +datum=WGS84')

library(ggplot2)

      quilo_u5_map <- df_quilo_u5_map |>
        ggplot2::ggplot() +
        ggplot2::geom_sf(aes(fill = `Óbito`)) +
        ggplot2::labs(title = "Óbito de nascidos vivos quilombolas menores de 5 anos por município",
                      fill = "Número de óbitos") +
        # geom_point(aes(x = lat, y = long, size = `Óbito`, color = `Óbito`), alpha = 0.9) +
        # scale_size_continuous(range = c(1, 12)) +
        # scale_color_viridis_c(trans = "log") +
        # theme_void()
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::scale_fill_gradient2() +
        ggplot2::guides(fill = guide_legend(theme = theme(
          legend.title = element_text(size = 15, face = "bold", colour = "black")
        )))

quilo_u5_map

max(dados_quilo_u5_wide$Óbito)


library(spData)
library(spDataLarge)
library(sf)
data(nz)

dados_espaciais_latlong <- dados_espaciais %>%
  transmute(lon = list(st_coordinates(.)[, 1]),
            lat = list(st_coordinates(.)[, 2])) %>%
  tidyr::unnest(lon, lat) %>%
  st_drop_geometry()


library(spData)
library(sf)
data(nz)

nzz <- nz
nz_latlong <- nz %>%
  transmute(lon = list(st_coordinates(.)[, 1]),
            lat = list(st_coordinates(.)[, 2])) %>%
  tidyr::unnest(lon, lat) %>%
  st_drop_geometry()

sfheaders::sf_to_df(nz)[c("x", "y")]

dados_espaciais_latlong <- sfheaders::sf_to_df(dados_espaciais)[c("cod_mun", "x", "y")]

dados_espaciais_latlong <- cbind(dados_espaciais_latlong, dados_espaciais$cod_mun)

dados_espaciais_ <-
  dados_espaciais$geometry %>%
  sf::st_coordinates() %>%
  as_tibble() %>%
  tidyr::unnest(c(X, Y)) %>%
  select("long" = X, "lat" = Y)

# Calculate the centroids
centroids <- st_centroid(dados_espaciais)
dados_espaciais_ <- sf::st_coordinates(centroids)

dados_espaciais_latlong <- cbind(dados_espaciais$cod_mun, dados_espaciais_) |>
  tibble::as_tibble() |>
  dplyr::rename(cod_mun = V1, lat = X, long = Y)

df_quilo_u5_map <- dados_espaciais_latlong |>
  dplyr::left_join(dados_quilo_u5_wide, by = c("cod_mun" = "codmunres_sinasc")) |>
  dplyr::mutate(across(where(is.numeric), ~ ifelse(.x == 0, NA, .x)))

library(ggplot2)

quilo_u5_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(dados_espaciais, fill = "gray", alpha = 0.3) +
  ggplot2::geom_point(df_quilo_u5_map_sf, aes(x = lat, y = long, size = `Óbito`, color = `Óbito`), alpha = 0.9) +
  # scale_size_continuous(range = c(1, 12)) +
  # scale_color_viridis_c(trans = "log") +
  # theme_void()
  ggplot2::labs(title = "Óbito de nascidos vivos quilombolas menores de 5 anos por município") +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::scale_fill_gradient2() +
  ggplot2::guides(fill = guide_legend(theme = theme(
    legend.title = element_text(size = 15, face = "bold", colour = "black")
  )))
quilo_u5_map


library(maps)
data <- world.cities %>% filter(country.etc == "UK")

# Left chart
ggplot() +
  geom_sf(data = UK, fill = "grey", alpha = 0.3) +
  geom_point(data = data, aes(x = long, y = lat)) +
  theme_void() +
  ylim(50, 59)

# quilo_u5_map <-


dados_espaciais_uf <- sf::read_sf(here::here("data-raw/dados-espaciais/uf_sf.shp")) |>
  dplyr::select(cod_stt, geometry)  |>
  dplyr::mutate(cod_stt = as.character(cod_stt))

ggplot2::ggplot() +
  ggplot2::geom_sf(data = dados_espaciais_uf, fill = "gray", alpha = 0.3) +
  ggplot2::geom_point(data = df_quilo_u5_map, aes(x = lat, y = long, size = `Óbito`, color = `Óbito`),
                      alpha = 0.9) +
  ggplot2::scale_size_continuous(breaks=c(1, 3, 5, 7, 10)) +
  ggplot2::scale_color_continuous(breaks=c(1, 3, 5, 7, 10)) +
  ggplot2::labs(title = "Óbitos de nascidos vivos quilombolas em menores de 5 anos por município") +
  ggplot2::theme_void() +
  guides(color= guide_legend(), size=guide_legend())

quilo_u5_map_ob <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dados_espaciais_uf, fill = "gray", alpha = 0.3) +
  ggplot2::geom_point(data = df_quilo_u5_map, aes(x = lat, y = long, size = `Óbito`, color = `Óbito`),
                      alpha = 0.9) +
  ggplot2::scale_size_continuous(breaks=c(1, 3, 5, 7, 10)) +
  ggplot2::scale_color_continuous(breaks=c(1, 3, 5, 7, 10)) +
  ggplot2::labs(title = "Óbitos de nascidos vivos quilombolas em menores de 5 anos por município") +
  ggplot2::theme_void() +
  ggplot2::guides(color= guide_legend(), size=guide_legend(),
                  fill = guide_legend(theme = theme(
                    legend.title = element_text(size = 15, face = "bold", colour = "black")
                  ))) +
  ggplot2::scale_fill_gradient2()

quilo_u5_map_ob

quilo_u5_map_nao_ob <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dados_espaciais_uf, fill = "gray", alpha = 0.3) +
  ggplot2::geom_point(data = df_quilo_u5_map, aes(x = lat, y = long, size = `Não Óbito`, color = `Não Óbito`),
                      alpha = 0.9) +
  ggplot2::scale_size_continuous(breaks=c(1, 25, 50, 100, 150, 200, 250)) +
  ggplot2::scale_color_continuous(breaks=c(1, 25, 50, 100, 150, 200, 250)) +
  ggplot2::labs(title = "Não Óbitos de nascidos vivos quilombolas em menores de 5 anos por município") +
  ggplot2::theme_void() +
  ggplot2::guides(color= guide_legend(), size=guide_legend(),
                  fill = guide_legend(theme = theme(
                    legend.title = element_text(size = 15, face = "bold", colour = "black")
                  ))) +
  ggplot2::scale_fill_gradient2()

quilo_u5_map_nao_ob

summary(df_quilo_u5_map$`Não Óbito`)
