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

## Definindo opções de idiomas para tabelas
options(reactable.language = reactableLang(
  pageSizeOptions = "\u663e\u793a {rows}",
  pageInfo = "{rowStart} \u81f3 {rowEnd} \u9879\u7ed3\u679c,\u5171 {rows} \u9879",
  pagePrevious = "\u4e0a\u9875",
  pageNext = "\u4e0b\u9875"
))


# Consultas iniciais ----
df_sifilis <- readRDS("data-raw/df_sifilis_mun_ano_fx_et_racacor_esc.RDS")

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
  dplyr::filter(ano_diag != "****") |>
  dplyr::arrange(ano_diag)

length_anos <- length(f_anos$ano_diag)

# f_meses <- df_sifilis |>
#   dplyr::select(mes_diag) |>
#   dplyr::distinct(mes_diag) |>
#   dplyr::filter(mes_diag != "**")


## Índices de desigualdade socioeconômicos ----

f_ind_socio <- c("IBP", "IDH Municipal", "IDHM Renda", "IDHM Longevidade", "IDHM Educacao",
                 "PBP Percapita (2010)", "Renda Gini (2010)", "IBP (Índice ")

## Código dos municípios ----
cod_ibge_mun <- data.table::fread("data-raw/cod_ibge_mun_ufs.csv") |>
  dplyr::select(cod_ibge, nome_mun, uf, nome_uf)

var_desi <- c("IDH Municipal", "IDHM Renda", "IDHM Longevidade", "IDHM Educacao",
              "PIB Percapita (2010)", "Renda Gini (2010)")

var_ind <- c("Faixa etária", "Raça/cor", "Escolaridade")

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
    div(
      class="sidebar-buttons",
      shinyWidgets::actionBttn(
        inputId = "aplicar",
        label = "Aplicar Filtros",
        style = "jelly",
        color = "primary"
      ),
      shinyWidgets::actionBttn(
        inputId = "resetar",
        label = "Reiniciar Filtros",
        style = "jelly",
        color = "danger"
      )
    ),
    ### Filtros ----
    tags$div(
      class = "filtros_fixos", id="filtros_fixos",
      shinyWidgets::sliderTextInput(
        inputId = "ano",
        label = "Escolha o período:",
        choices = f_anos$ano_diag,
        selected = f_anos$ano_diag[c(1, length_anos)]
      ),
      shinyWidgets::pickerInput(
        label = "Localidade",
        inputId = "loc",
        choices = c("Brasil", "Região", "Unidade da federação",
                    "Município"),
        selected = "Brasil",
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
      shinyWidgets::actionBttn(
        inputId = "maisFiltros",
        label = "Mais Filtros",
        style = "jelly",
        color = "primary"
      )
    )
  ),
  ## Painel ----
  div(
    class= "univ-filt",
    fluidRow(
      column(
        class="mapa",
        width = 6,
        bslib::card(
          shinycssloaders::withSpinner(plotOutput("biv_mapa")),
          border = FALSE
        )
        # bslib::card(
        #   plotOutput("biv_mapa")
        # )
      ),
      column(
        class="graf_ano",
        width = 6,
        navset_card_tab(
          height = 450,
          full_screen = TRUE,
          title = "Distribuição no tempo por Raça/cor",
          nav_panel(
            "Empilhadas",
            plotOutput("biv_graf_ano_empil_2")
          ),
          nav_panel(
            "Agrupadas",
            plotOutput("biv_graf_ano_agrup_2")
          ),
          nav_panel(
            "Todas",
            plotOutput("biv_graf_ano_2")
          )
        )
      )
    ),
    fluidRow(
      navset_card_tab(
        height = 450,
        full_screen = TRUE,
        title = "Distribuição no tempo por Raça/cor",
        nav_panel(
          "Empilhadas",
          plotOutput("biv_graf_ano_empil")
        ),
        nav_panel(
          "Agrupadas",
          plotOutput("biv_graf_ano_agrup")
        ),
        nav_panel(
          "Todas",
          plotOutput("biv_graf_ano")
        )
      )
    )
    # fluidRow(
    #   bslib::card(
    #     plotOutput("biv_graf_meses")
    #   )
    # )
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
  ## Indicadores de desigualdades socioeconômicas ----
  observeEvent(input$biv_var_1, {
    if(input$biv_var_1 == "IDH Municipal" || input$biv_var_1 == "IDHM Renda" ||
       input$biv_var_1 == "IDHM Longevidade" || input$biv_var_1 == "IDHM Educacao"){
      # browser()
      output$extra_ind_desi_socio <- renderUI({
        shinyWidgets::pickerInput(
          inputId = "side_filt_ind_desi_idh",
          label = "Selecione um nível de IDH",
          choices = c("Baixa" = "1",
                      "Média" = "2",
                      "Alta" = "3",
                      "Muito alta" = "4",
                      "Todas válidas" = "5",
                      "Em branco/Inválido" = "0"
          ),
          selected = c("5"),
          inline = TRUE
        )
      })
    }
  })
  ## Indivíduo ----
  observeEvent(input$biv_var_2, {
    if(input$biv_var_2 == "Faixa etária"){
      # browser()
      output$extra_indiv <- renderUI({
        shinyWidgets::pickerInput(
          inputId = "side_filt_indiv",
          label = "Selecione uma faixa etária",
          choices = c("10-14" = "1",
                      "15-19" = "2",
                      "20-24" = "3",
                      "25-29" = "4",
                      "30-39" = "5",
                      "40-59" = "6",
                      "60 ou mais" = "7",
                      "Todas válidas" = "8",
                      "Em branco/Inválido" = "0"
          ),
          selected = c("8"),
          inline = TRUE
        )
      })
    }
    if(input$biv_var_2 == "Raça/cor"){
      # browser()
      shinyWidgets::updatePickerInput(
        session = session,
        label = "Raça/cor",
        inputId = "side_filt_indiv",
        choices = c("Amarela" = "1",
                    "Branca" = "2",
                    "Indigena" = "3",
                    "Parda" = "4",
                    "Preta" = "5",
                    "Todas válidas" = "6",
                    "Em branco/Inválido" = "0"
        ),
        selected = c("1")
      )
      # output$extra_indiv <- renderUI({
      #   shinyWidgets::pickerInput(
      #     inputId = "side_filt_indiv",
      #     label = "Raça/cor",
      #     choices = c("Amarela" = "1",
      #                 "Branca" = "2",
      #                 "Indigena" = "3",
      #                 "Parda" = "4",
      #                 "Preta" = "5",
      #                 "Em branco/Inválido" = "0"
      #     ),
      #     selected = c("1", "2", "3", "4", "5"),
      #     inline = TRUE
      #   )
      # })
    }
  })

  # Gráficos iniciais ----
  ## bivariada ----
  ### Mapa ----
  output$biv_mapa <- renderPlot({
    df_uf_racacor <- readRDS("data/graf_i_biv_df_mapa_uf_racacor.RDS")
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
  })
  ### Distribuição de casos por ano ----
  #### Todas -----
  output$biv_graf_ano <- renderPlot({
    graf_ano <- readRDS("data/graf_i_biv_racacor_ano_todas.RDS")
    graf_ano
  })
  #### Agrupadas ----
  output$biv_graf_ano_agrup <- renderPlot({
    graf_ano_agrup <- readRDS("data/graf_i_biv_racacor_ano_dodge.RDS")
    graf_ano_agrup
  })
  #### Empilhadas ----
  output$biv_graf_ano_empil <- renderPlot({
    # browser()
    graf_ano_empil <- readRDS("data/graf_i_biv_racacor_ano_stack.RDS")
    graf_ano_empil
  })
  #### Todas -----
  output$biv_graf_ano_2 <- renderPlot({
    graf_ano <- readRDS("data/graf_i_biv_racacor_ano_todas.RDS")
    graf_ano
  })
  #### Agrupadas ----
  output$biv_graf_ano_agrup_2 <- renderPlot({
    graf_ano_agrup <- readRDS("data/graf_i_biv_racacor_ano_dodge.RDS")
    graf_ano_agrup
  })
  #### Empilhadas ----
  output$biv_graf_ano_empil_2 <- renderPlot({
    # browser()
    graf_ano_empil <- readRDS("data/graf_i_biv_racacor_ano_stack.RDS")
    graf_ano_empil
  })
  ### Distribuição de casos por mês ----
  output$biv_graf_meses <- renderPlot({
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
      geom_sf(aes(fill = n_casos_sifg)) +
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
