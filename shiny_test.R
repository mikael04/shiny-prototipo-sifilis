#' func_name
#'
#' @description A function that
#'
#' @return No return
#'
#' @author
#'
#' @date:

library(shiny)
library(bslib)

# Consultas iniciais ----
df_sifilis <- readRDS("data-raw/df_sifilis.RDS")

## Anos ----
f_anos <- df_sifilis |>
  dplyr::select(ano_diag) |>
  dplyr::distinct(ano_diag) |>
  dplyr::filter(ano_diag != "****")
# List of months
last_year <- max(as.numeric(f_anos$ano_diag))
first_year <- min(as.numeric(f_anos$ano_diag))
# choices_month <- seq.Date(
#   from = as.Date(paste0(first_year, "-01-01")),
#   to = as.Date(paste0(last_year, "-12-01")),
#   by = "month")
# choices_years <- seq.Date(
#   from = as.Date(paste0(first_year, "-01-01")),
#   to = as.Date(paste0(last_year, "-12-01")),
#   by = "year")
f_years <- seq(from = first_year, to = last_year, by = 1)

lenght_years <- length(f_years)

## Faixa etária ----
f_fx_et <- df_sifilis |>
  dplyr::select(faixa_etaria) |>
  dplyr::distinct(faixa_etaria) |>
  dplyr::arrange(faixa_etaria)

f_fx_et_limits <- c("10-14", "15-19", "20-39", "49-59", "60-64",
                    "65-69", "70-79", "80 ou mais", "Em branco/Inválido")

## Raça/cor ----
racacor <- df_sifilis |>
  dplyr::select(raca) |>
  dplyr::distinct(raca) |>
  dplyr::arrange(raca)

racacor_limits <- c("Amarela", "Branca", "Indigena", "Parda", "Preta", "Em branco/Inválido")

## Escolaridade ----
esc <- df_sifilis |>
  dplyr::select(escolaridade) |>
  dplyr::distinct(escolaridade) |>
  dplyr::arrange(escolaridade)

esc_limits <- c("Analfabeto", "1ª a 4ª série incompleta do EF", "4ª série completa do EF",
                "5ª a 8ª série incompleta do EF",
                "Ensino fundamental completo",
                "Ensino médio incompleto", "Ensino médio completo",
                "Educação superior incompleto",
                "Educação superior completo",
                "Não se aplica", "Em branco/Inválido")

# UI ----
ui <- page_sidebar(
  ## CSS ----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  ## Título ----
  title = "Painel de monitoramento da Sifilis",
  ## Sidebar ----
  sidebar = sidebar(
    # "sidebar",
    class = "sidebar",
    width = 350,
    # varSelectInput(
    #   "uf", "Selecione a UF",
    #   ufs_f
    # ),
    ### Filtros ----
    shinyWidgets::sliderTextInput(
      inputId = "ano",
      label = "Escolha o período:",
      choices = f_years,
      selected = f_years[c(1, lenght_years)]
    ),
    checkboxGroupInput(
      inputId = "fx_et",
      label = "Faixa etária",
      choices = c("10-14" = "1",
                  "15-19" = "2",
                  "20-39" = "3",
                  "49-59" = "4",
                  "60-64" = "5",
                  "65-69" = "6",
                  "70-79" = "7",
                  "80 ou mais" = "8",
                  "Em branco/Inválido" = "0"
      ),
      selected = c("1", "2", "3", "4", "5", "6", "7", "8"),
      inline = TRUE
    ),

    checkboxGroupInput(
      inputId = "racacor",
      label = "Raça/Cor",
      choices = c("Amarela" = "1",
                  "Branca" = "2",
                  "Indigena" = "3",
                  "Parda" = "4",
                  "Preta" = "5",
                  "Em branco/Inválido" = "0"
      ),
      selected = c("1", "2", "3", "4", "5"),
      inline = TRUE
    ),
    checkboxGroupInput(
      inputId = "esc",
      label = "Escolaridade",
      choices = c("Analfabeto" = "1",
                  "Fundamental" = "2",
                  "Médio" = "3",
                  "Superior" = "4",
                  "Em branco/Inválido" = "0"
      ),
      selected = c("1", "2", "3", "4"),
      inline = TRUE
    ),
    shinyWidgets::pickerInput(
      label = "Localidade",
      inputId = "loc",
      choices = c("Brasil", "Região", "Unidade da federação",
                  "Município"),
      selected = "Brasil",
    ),
  ),
)
# Server ----
server <- function(session, input, output) {
}

shinyApp(ui, server)
