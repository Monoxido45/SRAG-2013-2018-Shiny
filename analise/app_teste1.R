library(tidyverse)
library(shiny)
library(shinydashboard)
library(highcharter)

dados_SRAG <- "dados_SRAG.rds" |>
  readRDS()

cols <- RColorBrewer::brewer.pal(4, "Set1")
# alguns inputs
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "SRAG 2013-2018"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Informações gerais", tabName = "info"),
      menuItem("Mapa do SRAG", tabName = "map")
    )),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "info",
        h2("Informações gerais do SRAG entre 2013 e 2018"),
        br(),
        fluidRow(
          column(selectInput(inputId = "ano_srag",
                             label = "Selecione um ou mais anos",
                             multiple = TRUE,
                             choices = dados_SRAG |> pull(NU_ANO) |> unique(),
                             selected = 2013), width = 6),
          column(selectInput(inputId = "var_srag",
                             label = "Selecione uma variável",
                             multiple = FALSE,
                             choices = dados_SRAG |> select(CS_SEXO:OUTRO_SIN,
                                                            CARDIOPATI:OUT_MORBI) |>
                               names()), width = 6),
        ),
        br(),
        fluidRow(
          infoBoxOutput(outputId = "num_casos", width = 4),
          infoBoxOutput(outputId = "num_mortes", width = 4),
          infoBoxOutput(outputId = "num_hospit", width = 4)
        ),
        br(),
        fluidRow(
          box(
            width = 6,
            title = "Gráfico de barras de indicadores da doença",
            highchartOutput("hcbarras")
          ),
          box(
            width = 6,
            title = "Série temporal dos casos",
            highchartOutput("hcserie")
          )
        )
      ),
      
      #####
      # Output do Mapa - (soh com variaveis do
      # bloco de Conclusao por enquanto)
      #####
      
      tabItem(
        tabName = "map",
        h2("Mapa dos casos (Conclusão)"),
        br(),
        fluidRow(
          column(selectInput(inputId = "map_ano_srag",
                             label = "Selecione um ou mais anos",
                             multiple = TRUE,
                             choices = dados_SRAG |> pull(NU_ANO) |> unique(),
                             selected = 2013), width = 6),
          
          column(selectInput(inputId = "map_var_srag",
                             label = "Selecione uma variável",
                             multiple = FALSE,
                             choices = dados_SRAG |> select(CLASSI_FIN,
                                                            CRITERIO,EVOLUCAO) |>
                               names()),width = 3),
          
          column(selectInput(inputId = "map_nivel_srag",
                             label = "Selecione uma categoria",
                             multiple = FALSE,
                             choices = NULL) ,width = 3)
        ),
        br(),
        fluidRow(
          box()
        ),
      )
      
    )
  )
)


server <- function(input, output, session){
  
  # variaveis reativas
  ano <- reactive(input$ano_srag)
  var <- reactive(input$var_srag)
  
  ### botao com sub itens da variavel
  ## escolhida no mapa
  observeEvent(input$map_var_srag,{
    updateSelectInput(session,'map_nivel_srag',
                      choices=(unique(dados_SRAG[input$map_var_srag]) ))
  }) 
  
  map_ano <- reactive(input$map_ano_srag)
  map_var <- reactive(input$map_var_srag)
  map_nivel <- reactive(input$map_nivel_srag)
  
  # infobox com informacoes gerais de numerode casos, morte e hospit --------
  output$num_casos <- renderInfoBox({
    numero_de_casos <- dados_SRAG |> filter(NU_ANO %in% ano()) |> nrow()
    infoBox(
      title = "Número de casos",
      value = numero_de_casos,
      color = "orange",
      fill = TRUE
    ) 
  })
  output$num_mortes <- renderInfoBox({
    numero_de_casos <- dados_SRAG |> filter(NU_ANO %in% ano() & EVOLUCAO == 2) |> nrow()
    infoBox(
      title = "Número de Mortes",
      value = numero_de_casos,
      color = "red",
      fill = TRUE
    ) 
  })
  output$num_hospit <- renderInfoBox({
    numero_de_casos <- dados_SRAG |> filter(NU_ANO %in% ano() & HOSPITAL == 1) |> nrow()
    infoBox(
      title = "Número de Hospitalizações",
      value = numero_de_casos,
      color = "green",
      fill = TRUE
    ) 
  })
  
  output$hcbarras <- renderHighchart({
    if(length(ano()) != 0){
      # definindo cores
      dados_SRAG |> 
        filter(NU_ANO %in% ano()) |>
        count(!!sym(var())) |> 
        mutate(p = n/sum(n),
               p = scales::percent(p)) |>
        hchart('column', hcaes(x = !!sym(var()), y = n,
                               color = !!sym(var()))) |>
        hc_yAxis(title = list(text="Frequência"), 
                 labels = list(format = "{value}")) |>
        hc_xAxis(alignTicks = TRUE) |>
        hc_tooltip(pointFormat = '<b>Frequência:</b> {point.y} <br>
                 <b> Porcentagem:</b> {point.p:,.2f}%') |>
        hc_add_theme(hc_theme_ggplot2())
    }
  })
  
  output$hcserie <- renderHighchart({
    if(length(ano()) != 0){
      mortes <- dados_SRAG |>
        filter(NU_ANO %in% ano()) |>
        filter(!is.na(DT_OBITO)) |>
        count(DT_OBITO) |>
        arrange(DT_OBITO |> lubridate::dmy()) |>
        mutate(DT_OBITO = lubridate::as_date(DT_OBITO, format = "%d/%m/%Y")) |>
        complete(DT_OBITO = seq.Date(paste0("01/01/", ano() |> head(1)) |>
                                       as.Date(format = "%d/%m/%Y"),
                                     paste0("31/12/", ano() |> tail(1)) |>
                                       as.Date(format = "%d/%m/%Y"),
                                     by = "day")) |>
        transmute(DT_NOTIFIC = DT_OBITO,
                  mortes = ifelse(is.na(n), 0, n),
                  ma7_mortes = round(forecast::ma(mortes, 7), 3))
      
      
      dados_SRAG |> 
        filter(NU_ANO %in% ano()) |>
        count(DT_NOTIFIC) |> 
        arrange(DT_NOTIFIC |> lubridate::dmy()) |>
        mutate(DT_NOTIFIC = lubridate::as_date(DT_NOTIFIC, format = "%d/%m/%Y")) |>
        complete(DT_NOTIFIC = seq.Date(paste0("01/01/", ano() |> head(1)) |>
                                         as.Date(format = "%d/%m/%Y"),
                                       paste0("31/12/", ano() |> tail(1)) |>
                                         as.Date(format = "%d/%m/%Y"),
                                       by = "day")) |>
        transmute(DT_NOTIFIC,
                  casos = ifelse(is.na(n), 0, n),
                  ma7_casos = round(forecast::ma(casos, 7), 3)) |>
        left_join(mortes, by = "DT_NOTIFIC") |>
        pivot_longer(c(casos,ma7_casos, ma7_mortes, mortes), 
                     names_to = "contagem", values_to = "valor") |> 
        hchart('line', hcaes(x = DT_NOTIFIC, y = valor,
                             group = contagem),
               opacity = c(0.3, 0.8, 0.8, 0.3),
               name = c("Número de casos", "Média móvel dos casos",
                        "Número de mortes", "Média móvel de mortes")) |>
        hc_colors(cols[1:4]) |>
        hc_xAxis(title = list(text="Data da notificação"), 
                 dateTimeLabelFormats = list(day = '%m  %Y'), 
                 type = "datetime") |>
        hc_yAxis(title = list(text="Número de casos"), 
                 labels = list(format = "{value}"))|>
        hc_add_theme(hc_theme_ggplot2())
      
    }
  })
}


shinyApp(ui, server)