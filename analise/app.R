library(tidyverse)
library(shiny)
library(shinydashboard)
library(highcharter)
library(brazilmaps)
library(leaflet)
library(sf)
library(reactable)
library(chron)
library(survival)
library(flexsurv)


dados_SRAG <- "dados_SRAG.rds" |>
  readRDS()

pop_MesoRegion <- "pop_MesoRegion.rds" |>
  readRDS()

pop_State <- "pop_State.rds" |>
  readRDS()
  
pop_Region <- "pop_Region.rds" |>
  readRDS()

cols <- RColorBrewer::brewer.pal(4, "Set1")

marker_icon <- makeIcon(
  iconUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.8.0-beta.0/images/marker-icon.png",
  shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.8.0-beta.0/images/marker-shadow.png",
)

# dados do mapa dos estados
uf_keys <- get_brmap(geo = "State",class = "sf") |>
  select(nome, State)


# alguns inputs
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "SRAG 2013-2018"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Informações gerais", tabName = "info"),
      menuItem("Mapa do SRAG", tabName = "map"),
      menuItem("Análise de sobrevivência", tabName = "surv")
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
                        names()), width = 6)
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
      
      
      tabItem(
        tabName = "map",
        h2("Mapa dos casos por ano"),
        br(),
        box(
        h2("Mapa geral do Brasil"),
        fluidRow(
          column(selectInput(inputId = "ano_srag_mapa",
                             label = "Selecione um ano",
                             multiple = FALSE,
                             choices = dados_SRAG |> pull(NU_ANO) |> unique(),
                             selected = 2013), width = 4),
          column(selectInput(inputId = "div_mapa",
                             label = "Selecione uma divisão do mapa",
                             multiple = FALSE,
                             choices = c("Regiao" = "Region",
                                         "UF" = "State",
                                         "Macrorregião" = "MesoRegion")),
                 width = 4),
          column(selectInput(inputId = "cont_srag",
                             label = "Selecione uma estatística",
                             choices = c("Nº de casos" = "n_casos",
                                         "Nº de internações" = "n_intern",
                                         "Nº de mortes" = "n_mortes")),
                 width = 4)
        ), width = 12),
        fluidRow(
          leafletOutput("mapa_geral")
        ),
        br(),
        box(
          h2("Mapa dos Estados"),
          fluidRow(
            column(selectInput(inputId = "ano_srag_mapa_UF",
                               label = "Selecione um ano",
                               multiple = FALSE,
                               choices = dados_SRAG |> pull(NU_ANO) |> unique(),
                               selected = 2013), width = 4),
            column(selectInput(inputId = "uf",
                               label = "Selecione um estado",
                               multiple = FALSE,
                               choices = setNames(uf_keys$State,
                                                  uf_keys$nome),
                               selected = 35),
                   width = 4),
            column(selectInput(inputId = "cont_srag_UF",
                               label = "Selecione uma estatística",
                               choices = c("Nº de casos" = "n_casos",
                                           "Nº de internações" = "n_intern",
                                           "Nº de mortes" = "n_mortes")),
                   width = 4)
          ),width = 12
        ), 
        fluidRow(
          leafletOutput("mapa_estado")
        )
      ),
      
      #####
      # Output do Surv
      #####
      
      tabItem(
        tabName = "surv",
        h2("Análise de Sobrevivência"),
        br(),
        fluidRow(
          column(selectInput(inputId = "surv_ano_srag",
                             label = "Selecione um ou mais anos",
                             multiple = TRUE,
                             choices = dados_SRAG |> pull(NU_ANO) |> unique(),
                             selected = 2013), width = 3),
          
          column(selectInput(inputId = "surv_var_srag",
                             label = "Selecione uma variável",
                             multiple = FALSE,
                             choices = dados_SRAG |> 
                               select(CS_SEXO:OUTRO_SIN,
                                      CARDIOPATI:OUT_MORBI) |>
                               names()),width = 3),
          
          column(selectInput(inputId = "surv_model_srag",
                             label = "Selecione uma distribuição",
                             multiple = FALSE,
                             choices = c("exponential",
                                         "weibull",
                                         "loglogistic")),
                 width = 3)
          
        ),
        br(),
        fluidRow(
          box(
            width = 4,
            title = "Kaplan-Meyer",
            highchartOutput("hc_surv_km")
          ),
          box(
            width = 4,
            title = "Curva de sobrevivêcia (paramétrica)",
            highchartOutput("hc_surv_param")
          ),
          box(
            width = 4,
            title = "Curva de Risco (paramétrica)",
            highchartOutput("hc_hazard_param")
          )
        ),
        br(),
        fluidRow(
          column(12, align="center",
                 reactableOutput("table_all",width = "80%"),
                 titlePanel(" "),
                 reactableOutput("table_coef",width = "50%"))
          
        )
      )
      
      
      
      
      
    )
  )
)


server <- function(input, output, session){
  
  # variaveis reativas
  ano <- reactive(input$ano_srag |> 
                    sort())
  var <- reactive(input$var_srag)
  # variaveis do mapa
  ano_mapa <- reactive(input$ano_srag_mapa)
  regiao <- reactive(input$div_mapa)
  escolhas_cont <- reactive(input$cont_srag)
  
  # mapa dos estados
  uf <- reactive(input$uf)
  ano_estado <- reactive(input$ano_srag_mapa_UF)
  escolhas_cont_estado <- reactive(input$cont_srag_UF)

  
  ### Sobrevivencia
  surv_ano <- reactive(input$surv_ano_srag)
  surv_var <- reactive(input$surv_var_srag)
  surv_model <- reactive(input$surv_model_srag)
  
# infobox com informacoes gerais de numerode casos, morte e hospit --------
  output$num_casos <- renderInfoBox({
  numero_de_casos <- dados_SRAG |> 
    filter(NU_ANO %in% ano()) |> 
    nrow()
   infoBox(
     title = "Número de Notificações",
     value = numero_de_casos,
     color = "orange",
     fill = TRUE
   ) 
  })
  output$num_mortes <- renderInfoBox({
    numero_de_mortes <- dados_SRAG |> mutate(DT_OBITO = DT_OBITO |> lubridate::dmy()) |>
      filter(NU_ANO %in% ano() & EVOLUCAO == 2 & lubridate::year(DT_OBITO) %in% ano()) |> 
      nrow()
    infoBox(
      title = "Número de Mortes",
      value = numero_de_mortes,
      color = "red",
      fill = TRUE
    ) 
  })
  output$num_hospit <- renderInfoBox({
    numero_de_hospit <- dados_SRAG |> mutate(DT_INTERNA = DT_INTERNA |> lubridate::dmy()) |>
      filter(NU_ANO %in% ano() & HOSPITAL == 1 & lubridate::year(DT_INTERNA) %in% ano())|> 
      nrow()
    infoBox(
      title = "Número de Hospitalizações",
      value = numero_de_hospit,
      color = "green",
      fill = TRUE
    ) 
  })
  
  output$hcbarras <- renderHighchart({
    if(length(ano()) != 0){
    # definindo cores
    dados_SRAG |> 
      filter(NU_ANO %in% ano()) |>
      mutate(!!as.symbol(var()) := recode_factor(!!as.symbol(var()),
                                        "9" = "Ignorado")) |>
      count(!!sym(var())) |> 
      mutate(p = n/sum(n),
             p = scales::percent(p)) |>
      hchart('column', hcaes(x = !!sym(var()), y = n,
                             color = !!sym(var()))) |>
      hc_yAxis(title = list(text="Frequência"), 
               labels = list(format = "{value}")) |>
      hc_xAxis(alignTicks = TRUE) |>
      hc_tooltip(pointFormat = '<b>Frequência:</b> {point.y} <br>
                 <b> Porcentagem:</b> {point.p:,.2f}') |>
      hc_add_theme(hc_theme_ggplot2())
    }
  })
  
  output$hcserie <- renderHighchart({
    if(length(ano()) != 0){
      mortes <- dados_SRAG |>
        mutate(DT_OBITO = DT_OBITO |> lubridate::dmy()) |>
        filter(NU_ANO %in% seq(ano() |> head(1),ano() |> tail(1)),
               EVOLUCAO == 2 & lubridate::year(DT_OBITO) %in% 
                 seq(ano() |> head(1),
                     ano() |> tail(1))) |>
        filter(!is.na(DT_OBITO)) |>
        count(DT_OBITO) |>
        arrange(DT_OBITO) |>
        mutate(DT_OBITO = lubridate::as_date(DT_OBITO, format = "%d/%m/%Y")) |>
        complete(DT_OBITO = seq.Date(paste0("01/01/", ano() |> head(1)) |>
                                         as.Date(format = "%d/%m/%Y"),
                                       paste0("31/12/", ano() |> tail(1)) |>
                                         as.Date(format = "%d/%m/%Y"),
                                       by = "day")) |>
        transmute(DT_NOTIFIC = DT_OBITO,
                  mortes = ifelse(is.na(n), 0, n),
                  ma7_mortes = round(forecast::ma(mortes, 7), 2))
      
      
        dados_SRAG |> 
        filter(NU_ANO %in% seq(ano() |> head(1),ano() |> tail(1))) |>
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
                  ma7_casos = round(forecast::ma(casos, 7), 2)) |>
        left_join(mortes, by = "DT_NOTIFIC") |>
        pivot_longer(c(casos,ma7_casos, ma7_mortes, mortes), 
                     names_to = "contagem", values_to = "valor") |> 
        hchart('line', hcaes(x = DT_NOTIFIC, y = valor,
                               group = contagem),
               opacity = c(0.3, 0.8, 0.8, 0.3),
               name = c("Número de casos", "Média móvel dos casos",
                        "Média móvel de mortes", "Número de mortes")) |>
        hc_colors(cols[1:4]) |>
        hc_xAxis(title = list(text="Data da notificação"), 
                 dateTimeLabelFormats = list(day = '%m  %Y'), 
                 type = "datetime") |>
        hc_yAxis(title = list(text="Número de casos"), 
                 labels = list(format = "{value}"))|>
          hc_add_theme(hc_theme_ggplot2())
        
    }
  })
  
  output$mapa_geral <- renderLeaflet({
    # ajeitando os dados antes
    # primeiro ajeitando as regioes se preciso
    dados_reg <- dados_SRAG |> 
      group_by(!!sym((regiao())))
    
    # setando o nome da regiao
    nome_reg <- ifelse(regiao() == "Region",
                       "Região", ifelse(
                         regiao() == "State",
                         "Estado", "Mesorregião"
                       ))
    
    # analisando qual estatistica que se quer
    if(escolhas_cont() == "n_casos"){
      nome_var = "Número de casos"
      # dados pela regiao escolhida
      SRAG_selecionado <- dados_reg |> 
        filter(NU_ANO == ano_mapa()) |>
        summarise(n = n()) |>
        left_join(get(paste0("pop_",
                            regiao())), by = regiao()) |>
        mutate(n_100k = round(n/pop*(100000), 1))
      
      # dados por municipio
      SRAG_muni <- dados_SRAG |> 
        filter(NU_ANO == ano_mapa()) |>
        rename("nome_muni" = "Nome_Município") |>
        group_by(nome_muni) |>
        summarise(n = n(),
                  lat = unique(lat),
                  lon = unique(lon),
                  pop = unique(pop)) |>
        mutate(n_100k = round(n/pop*(100000), 1))
      
    }else{
      if(escolhas_cont() == "n_mortes"){
        nome_var = "Número de mortes"
        # dados pela regiao escolhida
        SRAG_selecionado <- dados_reg |> 
          mutate(DT_OBITO = DT_OBITO |> lubridate::dmy()) |>
          filter(NU_ANO %in% ano_mapa() & 
                   EVOLUCAO == 2 & 
                   lubridate::year(DT_OBITO) %in% ano_mapa()) |>
          summarise(n = n()) |>
          left_join(get(paste0("pop_",
                               regiao())), by = regiao()) |>
          mutate(n_100k = round(n/pop*(100000), 1))
        
        # dados por municipio
        SRAG_muni <- dados_SRAG |> 
          mutate(DT_OBITO = DT_OBITO |> lubridate::dmy()) |>
          filter(NU_ANO %in% ano_mapa() & 
                   EVOLUCAO == 2 & 
                   lubridate::year(DT_OBITO) %in% ano_mapa()) |>
          rename("nome_muni" = "Nome_Município") |>
          group_by(nome_muni) |>
          summarise(n = n(),
                    lat = unique(lat),
                    lon = unique(lon),
                    pop = unique(pop)) |>
          mutate(n_100k = round(n/pop*(100000), 1))
      }else{
        nome_var = "Número de internações"
          # dados pelo estado
          SRAG_selecionado <- dados_reg |> 
            mutate(DT_OBITO = DT_OBITO |> lubridate::dmy()) |>
              mutate(DT_INTERNA = DT_INTERNA |> lubridate::dmy()) |>
              filter(NU_ANO %in% ano() & 
                       HOSPITAL == 1 & 
                       lubridate::year(DT_INTERNA) %in% ano()) |>
            summarise(n = n()) |>
            left_join(get(paste0("pop_",
                                 regiao())), by = regiao()) |>
            mutate(n_100k = round(n/pop*(100000), 1))
          
          # dados por municipio
          SRAG_muni <- dados_SRAG |> 
            mutate(DT_OBITO = DT_OBITO |> lubridate::dmy()) |>
            mutate(DT_INTERNA = DT_INTERNA |> lubridate::dmy()) |>
        filter(NU_ANO %in% ano() & 
                 HOSPITAL == 1 & 
                 lubridate::year(DT_INTERNA) %in% ano()) |>
            rename("nome_muni" = "Nome_Município") |>
            group_by(nome_muni) |>
            summarise(n = n(),
                      lat = unique(lat),
                      lon = unique(lon),
                      pop = unique(pop)) |>
            mutate(n_100k = round(n/pop*(100000), 1))
        }
    }
   # construindo mapa
    state_map <- get_brmap(geo = regiao(),class = "sf") |>
      st_as_sf() |>
      st_transform(4326) |>
      left_join(SRAG_selecionado,
                by=c(regiao())) |>
      mutate(n_100k = ifelse(is.na(n_100k), 0, n_100k),
             n = ifelse(is.na(n), 0, n)
             ) |>
      rename_at(vars(contains("desc_rg")),
                ~sub("desc_rg", "nome", .x))
  
    pal <- colorNumeric(palette = "Reds", 
                        domain = state_map$n_100k)
    
    pal_mark <- colorNumeric(palette = "Blues", SRAG_muni$n_100k)
    
    SRAG_muni %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        color = ~pal_mark(n_100k),
        fillOpacity = 0.6,
        popup = ~paste0(sep = " ",
                        "<b>Cidade: </b>", nome_muni, "<br>",
                        "<b>", nome_var, ": </b>", n, "<br>",
                        "<b>", nome_var, " por 100k habitantes: </b>", 
                        n_100k),
        label = ~nome_muni,
        clusterOptions = markerClusterOptions()) %>%
      addPolygons(data = state_map,
                  smoothFactor = 0.5,
                  fillOpacity = 0.5,
                  weight = 0.5,
                  color = ~pal(state_map$n_100k),
                  opacity = 0.8,
                  highlightOptions = highlightOptions(color = "black",
                                                      weight = 2,
                                                      bringToFront = FALSE),
                  popup = ~paste0(sep = " ",
                                  "<b>", nome_reg, ": </b>", nome, "<br>",
                                  "<b>", nome_var, ": </b>", n, "<br>",
                                  "<b>", nome_var, " por 100k habitantes: </b>", 
                                  n_100k),
                  label = ~ nome) %>%
      addLegend(data = state_map,
                "bottomright",
                title = "Casos confirmados por 100k habitantes", 
                pal = pal, 
                values = ~n_100k, 
                opacity = 0.8)
  })
  
  output$mapa_estado <- renderLeaflet({
    # ajeitando os dados antes
    # primeiro ajeitando as regioes se preciso
    dados_reg <- dados_SRAG |> 
      filter(State %in% uf()) |>
      group_by(State)

    # para o poligono
    state_map <- get_brmap(geo = "State",class = "sf") |>
      st_as_sf() |>
      filter(State %in% uf()) |>
      st_transform(4326)
    
    # analisando qual estatistica que se quer
    if(escolhas_cont_estado() == "n_casos"){
      nome_var = "Número de casos"
      
      # dados por municipio
      SRAG_muni <- dados_reg |> 
        filter(NU_ANO == ano_estado()) |>
        rename("nome_muni" = "Nome_Município") |>
        group_by(nome_muni) |>
        summarise(n = n(),
                  lat = unique(lat),
                  lon = unique(lon),
                  pop = unique(pop)) |>
        mutate(n_100k = round(n/pop*(100000), 1))
      
    }else{
      if(escolhas_cont_estado() == "n_mortes"){
        nome_var = "Número de mortes"
        
        # dados por municipio
        SRAG_muni <- dados_reg |> 
          mutate(DT_OBITO = DT_OBITO |> lubridate::dmy()) |>
          filter(NU_ANO %in% ano_estado() & 
                   EVOLUCAO == 2 & 
                   lubridate::year(DT_OBITO) %in% ano_mapa()) |>
          rename("nome_muni" = "Nome_Município") |>
          group_by(nome_muni) |>
          summarise(n = n(),
                    lat = unique(lat),
                    lon = unique(lon),
                    pop = unique(pop)) |>
          mutate(n_100k = round(n/pop*(100000), 1))
      }else{
        nome_var = "Número de internações"
        
        # dados por municipio
        SRAG_muni <- dados_reg |> 
          mutate(DT_OBITO = DT_OBITO |> lubridate::dmy()) |>
          mutate(DT_INTERNA = DT_INTERNA |> lubridate::dmy()) |>
          filter(NU_ANO %in% ano_estado() & 
                   HOSPITAL == 1 & 
                   lubridate::year(DT_INTERNA) %in% ano()) |>
          rename("nome_muni" = "Nome_Município") |>
          group_by(nome_muni) |>
          summarise(n = n(),
                    lat = unique(lat),
                    lon = unique(lon),
                    pop = unique(pop)) |>
          mutate(n_100k = round(n/pop*(100000), 1))
      }
    }
    
    if(nrow(SRAG_muni) != 0){
    SRAG_muni %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        radius = ~sqrt(n_100k),
        lng = ~lon,
        lat = ~lat,
        color = "red",
        fillOpacity = 0.5,
        stroke= F,
        popup = ~paste0(sep = " ",
                        "<b> Cidade: </b>", nome_muni, "<br>",
                        "<b>", nome_var, ":</b>", n, "<br>",
                        "<b>", nome_var, " por 100k habitantes: </b>", 
                        n_100k),
        label = ~nome_muni) %>%
        addPolygons(data = state_map,
                    fill = FALSE,
                    smoothFactor = 25,
                    opacity = 0.01,
                    highlightOptions = highlightOptions(color = "white", 
                                                        weight = 2,
                                                        bringToFront = TRUE)
                    )
    }
    else{
      state_map |>
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fill = FALSE,
                    smoothFactor = 25,
                    opacity = 0.01,
                    highlightOptions = highlightOptions(color = "white", 
                                                        weight = 2,
                                                        bringToFront = TRUE))
    }
  })
  
  
  
  #####
  # Analise de sobrevivencia
  #####
  
  dados_surv <- reactive({
    
    dados_surv <- dados_SRAG |>
      filter(NU_ANO %in% surv_ano() ) |>
      select(EVOLUCAO,Tempo_Surv, !!(sym(surv_var())) ) |>
      na.omit() |>
      subset(EVOLUCAO!=9) |>
      filter(!!(sym(surv_var())) != "9") |>
      subset(Tempo_Surv>1&Tempo_Surv<800) |>
      droplevels()  
    
    dados_surv[,1] <- ifelse(dados_surv[,1]==1,0,1)
    
    dados_surv
  })
  
  fit_param <- reactive({
    
    survreg(as.formula(paste("Surv(Tempo_Surv,EVOLUCAO) ~ ",paste(surv_var()))), 
            data=dados_surv(), 
            dist=surv_model())
    
  })
  
  
  dados_surv_hc <- reactive({
    
    df_hc <- data.frame(Sobrevivencia=double(),
                       Tempo=double(),
                       Classe=double())
    
    for(i in 1:length(levels(dados_surv()[,3]))){
      
      novos_dados <- data.frame("termo"=levels(dados_surv()[,3])[i])
      names(novos_dados) <- as.character(fit_param()$terms[[3]])
      
      Tempo <-  predict(fit_param(), 
                      newdata=novos_dados, 
                      type='quantile',
                      p=1:98/100)
      
      Sobrevivencia <-  1 - 1:98/100
      
      Classe <- rep(levels(dados_surv()[,3])[i],98)
      
      df_hc <- rbind(df_hc, 
                    data.frame(Sobrevivencia,
                               Tempo,
                               Classe))
    }
    
    return(df_hc)
    
  })
  
  
  dados_hazard_hc <- reactive({
    
    df_hc <- data.frame(Tempo=double(),
                       Classe=double(),
                       Risco=double())
    
    for( i in 1:length(levels(dados_surv()[,3])) ){
      
      #novos_dados = data.frame("termo"=levels(dados_surv()[,3])[i])
      #names(novos_dados) =  as.character(fit_param()$terms[[3]])
      
      #Tempo = seq(1,600,0.3)
      
      Tempo <-  seq(1,600,0.5)
      
      #Sobrevivencia = 1-1:98/100
      
      Classe <- rep(levels(dados_surv()[,3])[i],1199)
      
      if(fit_param()$dist=="exponential"){
        
        if(i==1){
          Risco <- hexp(Tempo,rate=exp(-1*fit_param()$coefficients[c(1)]))
        }else{
          Risco <- hexp(Tempo,rate=exp(-1*sum(fit_param()$coefficients[c(1,i)])))
        }
        
      }
      
      if(fit_param()$dist=="weibull"){
        if(i == 1){
          Risco <- hweibull(Tempo,
                            scale=exp(fit_param()$coefficients[c(1)]),
                            shape=1/fit_param()$scale)
        }else{
          Risco <- hweibull(Tempo,
                            scale=exp(sum(fit_param()$coefficients[c(1,i)])),
                            shape=1/fit_param()$scale)
        }
      }
      
      if(fit_param()$dist=="loglogistic"){
        if(i == 1){
          Risco <- hllogis(Tempo, 
                           scale=exp(fit_param()$coefficients[c(1)]), 
                           shape=1/fit_param()$scale)
        }else{
          Risco <- hllogis(Tempo, 
                           scale=exp(sum(fit_param()$coefficients[c(1,i)]) ), 
                           shape=1/fit_param()$scale)
        }
        
      }
      
      
      
      df_hc <- rbind(df_hc, 
                    data.frame(Tempo,
                               Classe,
                               Risco))
    }
    
    return(df_hc)
    
  })
  
  
  output$hc_surv_km <-renderHighchart({
    if(length(surv_ano()) != 0){
      
      
      fit <- survfit(as.formula(paste("Surv(Tempo_Surv,EVOLUCAO) ~ ",paste(surv_var()))),
                    data=dados_surv())
      
      fit |>
      hchart(ranges=F,
             rangesOpacity = 0.3,
             markTimes=F) |>
        hc_tooltip(pointFormat = '<b>• Sobrevivência:</b> {point.y:,.2f} <br>
                 <b>• Tempo:</b> {point.x:,.0f}',
                   headerFormat=" ") |>
        hc_xAxis(title = list(text="Tempo em dias")) |>
        hc_yAxis(title = list(text="Sobrevivência (probabilidade)")) |>
        hc_add_theme(hc_theme_ggplot2()) 
      
    }
  })
  
  output$hc_surv_param <-renderHighchart({
    if(length(surv_ano()) != 0){
      
      dados_surv_hc() |>
        hchart('line', hcaes(x = Tempo, y = Sobrevivencia, group = Classe)) |>
        hc_tooltip(pointFormat = '<b>• Sobrevivência:</b> {point.y:,.2f} <br>
                 <b>• Tempo:</b> {point.x:,.0f}',
                   headerFormat=" ") |>
        hc_xAxis(title = list(text="Tempo em dias")) |>
        hc_yAxis(title = list(text="Sobrevivência (probabilidade)")) |>
        hc_add_theme(hc_theme_ggplot2()) 
      
    }
  })
  
  output$hc_hazard_param <-renderHighchart({
    if(length(surv_ano()) != 0){
      
      dados_hazard_hc() %>% 
        hchart(
          'line', hcaes(x = Tempo, y = Risco, group = Classe)) |>
        hc_tooltip(pointFormat = '<b>• Risco:</b> {point.y:,.4f} <br>
                 <b>• Tempo:</b> {point.x:,.0f}',
                   headerFormat=" ") |>
        hc_xAxis(title = list(text="Tempo em dias")) |>
        hc_yAxis(title = list(text="Risco")) |>
        hc_add_theme(hc_theme_ggplot2()) 
      
    }
  })
  
  output$table_all <- renderReactable({
    if(length(surv_ano()) != 0){
      
      df_all <- data.frame(dados_surv() |> nrow(),
                          sum(dados_surv()[,1] == 1),
                          round(AIC(fit_param()),digits=3),
                          round(BIC(fit_param()),digits=3),
                          summary(fit_param())$iter)
      
      colnames(df_all) <- c("Número de Pessoas",
                           "Número de Mortes (censuras)",
                           "AIC do modelo",
                           "BIC do modelo",
                           "Iterações de Newton-Raphson")
      
      reactable(df_all)
    }
  })
  
  
  output$table_coef <- renderReactable({
    if(length(surv_ano()) != 0){
      
      df_coef <- summary(fit_param())$table[,c(1,2,4)]
      df_coef[,c(1,2)] <- round(df_coef[,c(1,2)],digits=3)
      df_coef[,3] <- format.pval(df_coef[,3],
                              digits = 2,
                              eps = 0.001,
                              nsmall = 3)
      
      colnames(df_coef) <- 
        c("Estimativa do Coeficiente",
          "Erro padrão da estimativa",
          "Valor-p")
      
      reactable(df_coef)
    }
  })
  
  
}


shinyApp(ui, server)






