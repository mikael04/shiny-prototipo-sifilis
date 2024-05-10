# Library ----
library(shiny)
## Layout
library(bslib)
library(gridlayout)
## Gráficos
library(ggplot2)
## Manipulação de dados
library(dplyr)
library(dbplyr)
library(tidyr)
## Comunicação com o banco de dados
library(bigrquery)
library(DBI)
## Pacote para tabelas
library(gt)
library(reactable)

## Variáveis globais ----
ggiraph_plotly <- F
taxa_nasc <- T

## Labels
label_unit <- function(max_count){
  if(max_count < 1e3){
    return("")
  }
  if(max_count >= 1e3 & max_count < 1e6){
    return("mil")
  }
  if(max_count >= 1e6 & max_count < 1e9){
    return("M")
  }
}

label_scale <- function(max_count){
  if(max_count < 1e3){
    return(1)
  }
  if(max_count >= 1e3 & max_count < 1e6){
    return(1e-3)
  }
  if(max_count >= 1e6 & max_count < 1e9){
    return(1e-6)
  }
}

## Definindo opções de idiomas para tabelas
options(reactable.language = reactableLang(
  pageSizeOptions = "\u663e\u793a {rows}",
  pageInfo = "{rowStart} \u81f3 {rowEnd} \u9879\u7ed3\u679c,\u5171 {rows} \u9879",
  pagePrevious = "\u4e0a\u9875",
  pageNext = "\u4e0b\u9875"
))


# Consultas iniciais ----
df_sifilis <- readRDS("data-raw/df_sifilis.RDS")

## UF ----
f_ufs <- df_sifilis |>
  dplyr::select(uf_res) |>
  dplyr::distinct(uf_res) |>
  dplyr::mutate(uf_res = as.integer(uf_res))

df_ufs <- data.table::fread("data-raw/ibge-ufs-pop-2022-est.csv")

f_ufs <- dplyr::inner_join(f_ufs, df_ufs, by = c("uf_res" = "uf_cod")) |>
  dplyr::select(uf_cod = uf_res, uf_sigla, uf_nome, uf_reg)


f_regioes <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")

# df_ufs <- data.table::fread("data-raw/pop-2022-est-ibge.csv")

## Datas ----

f_anos <- df_sifilis |>
  dplyr::select(ano_diag) |>
  dplyr::distinct(ano_diag) |>
  dplyr::filter(ano_diag != "****")

length_anos <- length(f_anos$ano_diag)

f_meses <- df_sifilis |>
  dplyr::select(mes_diag) |>
  dplyr::distinct(mes_diag) |>
  dplyr::filter(mes_diag != "**")


cod_ibge_mun <- data.table::fread("data-raw/cod_ibge_mun_ufs.csv") |>
  dplyr::select(cod_ibge, nome_mun, uf, nome_uf)



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
    width = 350,
    #### Ano ----
    shinyWidgets::sliderTextInput(
      inputId = "ano",
      label = "Escolha o período:",
      choices = f_anos$ano_diag,
      selected = f_anos$ano_diag[c(1, length_anos)]
    ),
    #### Faixa etária ----
    checkboxGroupInput(
      inputId = "fx_et",
      label = "Faixa etária",
      choices = c("10-14" = "1",
                  "15-19" = "2",
                  "20-24" = "3",
                  "25-29" = "4",
                  "30-39" = "5",
                  "49-59" = "6",
                  "60 ou mais" = "7",
                  "Em branco/Inválido" = "0"
      ),
      selected = c("1", "2", "3", "4", "5", "6", "7", "8"),
      inline = TRUE
    ),
    #### Raça/Cor ----
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
    #### Escolaridade ----
    checkboxGroupInput(
      inputId = "esc",
      label = "Escolaridade",
      choices = c("Analfabeto" = "1", "1ª a 4ª série incompleta do EF" = "2",
                  "4ª série completa do EF" = "3", "5ª à 8ª série incompleta do EF" = "4",
                  "Ensino fundamental completo" = "5", "Ensino médio incompleto" = "6",
                  "Ensino médio completo" = "7", "Educação superior incompleta" = "8",
                  "Educação superior completa" = "9",
                  "Em branco/Inválido" = "0"
      ),
      selected = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
      inline = TRUE
    ),
    #### Localidade ----
    shinyWidgets::pickerInput(
      label = "Localidade",
      inputId = "loc",
      choices = c("Brasil", "Região", "Unidade da federação",
                  "Município"),
      selected = "Brasil",
    ),
    uiOutput("extra_geoloc_1"),
    uiOutput("extra_geoloc_2"),
    # shinyWidgets::sliderTextInput(
    #   inputId = "date",
    #   label = "Escolha o Período:",
    #   choices = date_f,
    #   selected = date_f[c(1, length(date_f))]
    # )
    # div(
    #   class="combine_vars",
    #   shinyWidgets::pickerInput(
    #     label = "Nascimentos por",
    #     inputId = "var_sel_1",
    #     choices = list(
    #       `Variáveis de nascidos vivos` = var_nasc_vivo,
    #       `Variáveis da mãe do nascido` = var_mae_nasc
    #     )
    #   )
    # ),
    div(
      class="botao-filtros",
      shinyWidgets::actionBttn(
        inputId = "applyFilters",
        label = "Aplicar",
        style = "jelly",
        color = "primary"
      )
    )
  ),
  ## Row panel ----
  page_navbar(
    id = "nav",
    ### Univariada ----
    nav_panel(
      "Univariada 1",
      id = "univ-1",
      div(
        class= "univ-1",
        fluidRow(
          column(
            class="mapa",
            width = 6,
            bslib::card(
              plotOutput("univ_mapa")
            )
          ),
          column(
            class="graf_ano",
            width = 6,
            bslib::card(
              plotOutput("univ_graf_ano")
            )
          )
        ),
        fluidRow(
          bslib::card(
            plotOutput("univ_graf_meses")
          )
        )
      )
    ),
    ### Raça/cor ----
    nav_panel(
      "Raça/cor",
      id = "racacor",
      div(
        class= "racacor",
        fluidRow(
          column(
            class="mapa",
            width = 6,
            bslib::card(
              plotOutput("racacor_mapa")
            )
          ),
          column(
            class="graf_ano",
            width = 6,
            bslib::card(
              plotOutput("racacor_graf_ano")
            )
          )
        ),
        fluidRow(
          bslib::card(
            plotOutput("racacor_graf_meses")
          )
        )
      )
    ),
  )
)

# Server ----
server <- function(session, input, output) {
  # Dados geográficos ----
  ## Municípios
  mun_sf <- sf::read_sf(here::here("data-raw/dados-espaciais/municipios_sf.shp"))
  ## UFs
  uf_sf <- sf::read_sf(here::here("data-raw/dados-espaciais/uf_sf.shp")) |>
    dplyr::select(cod_stt, geometry)
  ## Dados ibge municípios
  df_mun <- data.table::fread("data-raw/ibge-dados-municipais-tratados.csv")
  df_cap_ufs <- data.table::fread("data-raw/cod_ibge_mun_capitais.csv")
  ## Dados ibge mun e reg saúde (macro e micro)
  df_mun_reg_saude <- data.table::fread("data-raw/mun_uf_regiao_reg_saude.csv")

  # # Bigrquery ----
  # con <- dbConnect(
  #   bigrquery::bigquery(),
  #   project = "pdi-covid-basededados",
  #   dataset = "paineis"
  # )
  #
  # df_sinasc <- tbl(con, "view_sinasc_tratamento_painel")

  # Labels ----
  labels <- data.table::fread("data-raw/labels.csv") |>
    dplyr::select(nivel, label, variavel) |>
    dplyr::mutate(nivel = as.character(nivel))

  # Dados Sifilis ----
  df_sifilis <- readRDS("data-raw/df_sifilis.RDS")


  # Valores reativos ----
  start = reactiveValues(value = TRUE)

  #

  # Filtros reativos ----
  ## Geolocalização ----
  observeEvent(input$loc, {
    if(input$loc == "Brasil"){
      output$extra_geoloc_1 <- NULL
      output$extra_geoloc_2 <- NULL
    }
    if(input$loc == "Região"){
      output$extra_geoloc_1 <- renderUI({
        shinyWidgets::pickerInput(
          label = "Selecione a Região",
          inputId = "reg",
          choices = c("TODOS", f_regioes),
          selected = "TODOS",
        )
      })
      output$extra_geoloc_2 <- NULL
    }
    if(input$loc == "Unidade da federação"){
      output$extra_geoloc_1 <- renderUI({
        shinyWidgets::pickerInput(
          label = "Selecione a UF",
          inputId = "uf",
          choices = c("TODOS", f_ufs$uf_sigla),
          selected = "TODOS",
        )
      })
      output$extra_geoloc_2 <- NULL
    }
    if(input$loc == "Município"){
      output$extra_geoloc_1 <- renderUI({
        selectizeInput(
          label = "Selecione a UF",
          inputId = "uf",
          choices = c(f_ufs$uf_sigla),
          # selected = " ",
          options = list(title = "choose here")
        )
      })
      output$extra_geoloc_2 <- renderUI({
        selectizeInput(
          label = "Município",
          inputId = "mun",
          choices = c("Selecione a UF")
        )
      })
    }
  })
  observeEvent(input$uf,{
    if(input$uf != " "){
      muns_uf_sel <- cod_ibge_mun |>
        dplyr::filter(uf == input$uf) |>
        dplyr::arrange(nome_mun, .locale = "pt_BR") |>
        dplyr::pull(nome_mun)

      mun_cap_uf <- df_cap_ufs |>
        dplyr::filter(sigla_uf == input$uf) |>
        dplyr::pull(nome_mun)

      updateSelectizeInput(
        session = session, inputId = "mun",
        choices = muns_uf_sel,
        selected = mun_cap_uf
      )
    }
  })
  # Gráficos iniciais ----
  ## Univariada ----
  ### Mapa ----
  output$univ_mapa <- renderPlot({
    df_uf_sel <- readRDS("data/graf_i_uni_df_mapa.RDS")
    mapa <- df_uf_sel |>
      ggplot() +
      geom_sf(aes(fill = n_casos)) +
      # scale_fill_viridis_c() +
      labs(title = "Distribuição de casos de Sífilis por UF",
           fill = "Número de casos") +
      theme_void() +
      theme(legend.position = "bottom") +
      scale_fill_gradient2() +
      ggplot2::guides(fill = guide_legend(theme = theme(
        legend.title = element_text(size = 15, face = "bold", colour = "black")
      )))

    # browser()
    mapa
  })
  ### Distribuição de casos por ano ----
  output$univ_graf_ano <- renderPlot({
    graf_ano <- readRDS("data/graf_i_uni_ano.RDS")
    graf_ano

  })
  ### Distribuição de casos por mês ----
  output$univ_graf_meses <- renderPlot({
    graf_ano_mes <- readRDS("data/graf_i_uni_ano_mes.RDS")
    graf_ano_mes
  })

  ## Raça/cor ----
  ### Mapa ----
  output$racacor_mapa <- renderPlot({
    df_racacor_mapa <- readRDS("data/df_racacor_mapa.RDS")

    mapa <- df_racacor_mapa |>
      dplyr::filter(raca == "Parda") |>
      ggplot() +
      geom_sf(aes(fill = n_casos)) +
      # scale_fill_viridis_c() +
      labs(title = "Distribuição de casos de Sífilis por UF, raça/cor: Parda",
           fill = "Número de casos") +
      theme_void() +
      theme(legend.position = "bottom") +
      scale_fill_gradient2() +
      ggplot2::guides(fill = guide_legend(theme = theme(
        legend.title = element_text(size = 15, face = "bold", colour = "black")
      )))

    # browser()
    mapa
  })
  ### Removendo demais filtros ----
  observeEvent(input$nav, {
    # input$
    if(input$nav == "Raça/cor"){
      # browser()
      updateCheckboxGroupInput(
        session = session,
        inputId = "fx_et",
        NULL
      )
      output$fx_et <- NULL
      output$esc <- NULL
      output$loc <- NULL
    }
  })

}

shinyApp(ui, server)
