library(tidyverse)
library(shiny)
library(shinydashboard)
library(highcharter)

dados_SRAG <- "dados_SRAG.rds" |>
  readRDS()

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
                      choices = 2013:2018,
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
            title = "Gráfico de barras de indicadores da doença",
            highchartOutput("hcbarras")
          )
        )
      ),
      tabItem(
        tabName = "map",
        h2("Mapa dos casos")
      )
    )
  )
)


server <- function(input, output, session){
  
  # variaveis reativas
  ano <- reactive(input$ano_srag)
  var <- reactive(input$var_srag)
  
  # primeira aba do app
  
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
  
  output$hcbarras <- renderHighchart(
    if(length(ano()) != 0){
    # definindo cores

    dados_SRAG |> 
      filter(NU_ANO %in% ano()) |>
      count(!!sym(var())) |> 
      mutate(p = n/sum(n),
             p = scales::percent(p)) |>
      hchart('column', hcaes(x = !!sym(var()), y = n,
                             color = !!sym(var()))) |>
      hc_yAxis(title = list(text="Proporção"), 
               labels = list(format = "{value}")) |>
      hc_xAxis(alignTicks = TRUE) |>
      hc_tooltip(pointFormat = '<b>Frequência:</b> {point.y} <br>
                 <b> Porcentagem:</b> {point.p:,.2f}%') |>
      hc_add_theme(hc_theme_ggplot2())
    }
  )
}

shinyApp(ui, server)