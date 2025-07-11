source("setup.R")
ui <- fluidPage(
  theme = bs_theme(
    version = 5, 
    bootswatch = "flatly",  # Otros: "minty", "cerulean", "cosmo", "lux"
    primary = "#0073e6"),
  titlePanel(div("Calculadora del Régimen Obligatorio de Pensiones (ROP)", style = "background-color: #0073e6; color: white; padding: 10px; border-radius: 5px;")),
  
  sidebarLayout(
    sidebarPanel(
      h4("Montos de los rendimientos de los años:", style = "font-weight: bold; color: #0073e6;"),
      numericInput("r2023", "2023", value = 0),
      numericInput("r2024", "2024", value = 0),
      numericInput("r2025", "2025", value = 0),
      hr(),
      h4("Características:", style = "font-weight: bold; color: #0073e6;"),
      numericInput("monto", "Monto acumulado final:", value = 0, min = 0),
      numericInput("edad", "Edad actual:", value = 65, min = 18, max = 100),
      radioButtons("modo_tasa", "Modo de tasa de interés:",
                   choices = c("Constante 3.6%", "Aleatoria", "Media estocástica"),
                   selected = "Constante 3.6%",
                   inline = TRUE),
      selectInput("sexo", "Sexo:", choices = c("Masculino", "Femenino")),
      #numericInput("jub", "Año de jubilación:", value = 2025, min = 1950, max = 2100),
      actionButton("calcular", "Calcular", style = "background-color: #0073e6; color: white; border-radius: 5px;")
    ),
    
    mainPanel(
      h3("Expectativa de vida", style = "font-weight: bold; color: #0073e6;"),
      textOutput("expectativa"),
      br(),
      fluidRow(
        valueBoxOutput("vb_rp"),
        valueBoxOutput("vb_rt"),
        valueBoxOutput("vb_rpmt")
      ),
      
      h3("Tipo de retiro", style = "font-weight: bold; margin-top: 30px; color: #0073e6;"),
      
      tabsetPanel(
        selected = NULL,  # Ninguna pestaña activa al inicio
        tabsetPanel(
          selected = NULL,  # Ninguna pestaña activa al inicio
          tabPanel("Información",
                   h4("Instrucciones de uso"),
                   p("Para utilizar esta herramienta, siga los siguientes pasos:"),
                   tags$ol(
                     tags$li("Complete los datos en el panel izquierdo."),
                     tags$li("Presione el botón \"Calcular\" y espere a que se muestre su expectativa de vida."),
                     tags$li("Acceda a la pestaña de Renta que sea de su interés.")
                   ),
                   br(),
                   p("Cada pestaña mostrará los resultados correspondientes al tipo de renta seleccionado.")
          ),
          tabPanel("Retiro Programado",
                   h5(textOutput("pension_actual_rp")),
                   h5(textOutput("pension_prox_rp")),
                   h4("Gráficos"),
                   plotlyOutput("graf_reserva1") %>% withSpinner(color = "#0073e6"),
                   plotlyOutput("graf_pension1") %>% withSpinner(color = "#0073e6"),
                   downloadButton("descargar_tabla_rp", "Descargar tabla RP", class = "btn btn-outline-primary")
                   
                   
          ),
          tabPanel("Renta Temporal",
                   h5(textOutput("pension_actual_rt")),
                   h5(textOutput("pension_prox_rt")),
                   h4("Gráficos"),
                   plotlyOutput("graf_reserva2") %>% withSpinner(color = "#0073e6"),
                   plotlyOutput("graf_pension2") %>% withSpinner(color = "#0073e6"),
                   downloadButton("descargar_tabla_rt", "Descargar tabla RT", class = "btn btn-outline-primary")
                   
                   
          ),
          tabPanel("Renta Permanente",
                   h5(textOutput("pension_actual_rpmt")),
                   h5(textOutput("pension_prox_rpmt")),
                   h4("Gráficos"),
                   plotlyOutput("graf_reserva3") %>% withSpinner(color = "#0073e6"),
                   plotlyOutput("graf_pension3") %>% withSpinner(color = "#0073e6"),
                   downloadButton("descargar_tabla_rpmt", "Descargar tabla RPmt", class = "btn btn-outline-primary")
          ),
          tabPanel("Tasas Estocásticas",
                   h4("Simulaciones de tasas de interés"),
                   p("Simulando 1000 trayectorias de tasas de interés, se calcula el promedio de estos movimientos y se obtiene la tasa de interés de la media estocástica, que es una de las tres opciones de las tasas a visualizar"),
                   tags$img(src = "rplot.png", style = "max-width:60%; height:40%;")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  # (ANTONI) LEER MIS FUNCIONES
  
  # Variables reactivas para almacenar los resultados
  valores <- reactiveValues(
    edad = NULL,
    expectativa = NULL,
    retiro_programado = NULL,
    renta_permanente = NULL,
    renta_temporal = NULL,
    
    ### ANTHONY
    tabla_programado = NULL,
    tabla_temporal = NULL,
    tabla_temporal_sim = NULL,
    graficos = NULL,
    graficos_temporal = NULL
  )
  observeEvent(input$calcular, {
    sexo_cod <- ifelse(input$sexo == "Masculino", 1, 2)
    valores$edad <- input$edad
    valores$expectativa <- tabla[tabla$edad == input$edad & tabla$year == 2025 & tabla$sex == sexo_cod, 7]
    valores$retiro_programado <- input$monto * 0.03
    valores$renta_permanente <- input$monto * 0.02
    valores$renta_temporal <- input$monto * 0.04
    # Pensiones de renta permanente para mostrar

    
    if (input$modo_tasa == "Aleatoria") {
      tasas_usuario <- intereses(input$edad)
    } else if(input$modo_tasa == "Constante 3.6%") {
      tasas_usuario <- rep(0.036, 115 - input$edad + 1)  # constante anual 3.6%
    } else {
      tasas_usuario <- read_csv("media.csv", 
                                col_names = c("tiempo", "media"),
                                show_col_types = FALSE,
                                skip = 1)
      tasas_usuario <- tasas_usuario$media[1:(116-x)]
    }
    
    edad_retiro <- input$edad + (2025 - 2025)
    
    ### ANTHONY
    # VANU Completa y simulación
    vanu_comp <- vanuCompleta(edad_inicio = edad_retiro, sexo = sexo_cod, tabla = tabla, anno_objetivo = 2025)
    
    vanu_temp <- vanuTemporal(edad_inicio = edad_retiro, sexo = sexo_cod, tabla = tabla, anno_objetivo = 2025)
    
    sim1 <- simular_reserva(input$monto, vanu_comp, tasas_usuario, edad_retiro = edad_retiro)
    
    sim2 <- simular_reserva_temporal(input$monto, vanu_temp, tasas_usuario)
    sim3 <- renta_permanente(c(input$r2023, input$r2024, input$r2025), input$edad, input$monto, 2025, tasas_usuario)

    
    # Pensiones para mostrar
    valores$pension_actual_rp <- sim1$Pension[1]
    valores$pension_prox_rp <- sim1$Pension[2]
    
    valores$pension_actual_rt <- sim2$Pension[1]
    valores$pension_prox_rt <- sim2$Pension[2]
    valores$pension_actual_rpmt <- sim3$Pension_Mensual[1]
    valores$pension_prox_rpmt <- sim3$Pension_Mensual[2]
    
    # Guardar resultados
    valores$tabla_programado <- sim1
    valores$tabla_temporal <- vanu_temp
    valores$tabla_temporal_sim <- sim2
    
    plots1 <- graficos(sim1)
    plots2 <- graficos(sim2)
    plots3 <- graficos(sim3)
    
    valores$graficos1 <- plots1
    valores$graficos2 <- plots2
    valores$graficos3 <- plots3
    
    
  })
  
  # Mostrar resultados solo cuando existan
  output$expectativa <- renderText({
    req(valores$expectativa, valores$edad)
    paste("Su expectativa de vida condicionado a que tiene", valores$edad, "años es de", valores$expectativa, "años")
  })
  
  output$retiro_programado <- renderText({
    req(valores$retiro_programado)
    paste("Retiro Programado: ", round(valores$retiro_programado, 2))
  })
  
  output$renta_temporal <- renderText({
    req(valores$renta_temporal)
    paste("Renta Temporal: ", round(valores$renta_temporal, 2))
  })
  
  output$renta_permanente <- renderText({
    req(valores$renta_permanente)
    paste("Renta Permanente: ", round(valores$renta_permanente, 2))
  })
  
  # Gráficos
  output$graf_reserva1 <- renderPlotly({
    valores$graficos1$Reserva
  })
  output$graf_pension1 <- renderPlotly({
    valores$graficos1$Pension
  })
  output$graf_reserva2 <- renderPlotly({
    valores$graficos2$Reserva
  })
  output$graf_pension2 <- renderPlotly({
    valores$graficos2$Pension
  })
  output$graf_reserva3 <- renderPlotly({
    valores$graficos3$Reserva
  })
  output$graf_pension3 <- renderPlotly({
    valores$graficos3$Pension
  })
  
  # Retiro Programado
  output$pension_actual_rp <- renderText({
    req(valores$pension_actual_rp)
    paste("Pensión mensual este año:", round(valores$pension_actual_rp, 2), "colones")
  })
  
  output$pension_prox_rp <- renderText({
    req(valores$pension_prox_rp)
    paste("Pensión mensual el próximo año:", round(valores$pension_prox_rp, 2), "colones")
  })
  
  # Renta Temporal
  output$pension_actual_rt <- renderText({
    req(valores$pension_actual_rt)
    paste("Pensión mensual para este año:", round(valores$pension_actual_rt, 2), "colones")
  })
  
  output$pension_prox_rt <- renderText({
    req(valores$pension_prox_rt)
    paste("Pensión mensual para el próximo año:", round(valores$pension_prox_rt, 2), "colones")
  })
  # Renta Permanente
  output$pension_actual_rpmt <- renderText({
    req(valores$pension_actual_rpmt)
    paste("Pensión mensual para este año:", round(valores$pension_actual_rpmt, 2), "colones")
  })
  
  output$pension_prox_rpmt <- renderText({
    req(valores$pension_prox_rpmt)
    paste("Pensión mensual para el próximo año:", round(valores$pension_prox_rpmt, 2), "colones")
  })
  output$vb_rp <- renderValueBox({
    req(valores$pension_actual_rp)
    valueBox(
      paste0("₡", format(round(valores$pension_actual_rp, 0), big.mark = ",")),
      "Pensión Retiro Programado",
      icon = icon("money-bill"),
      color = "blue"
    )
  })
  
  output$vb_rt <- renderValueBox({
    req(valores$pension_actual_rt)
    valueBox(
      paste0("₡", format(round(valores$pension_actual_rt, 0), big.mark = ",")),
      "Pensión Renta Temporal",
      icon = icon("clock"),
      color = "teal"
    )
  })
  
  output$vb_rpmt <- renderValueBox({
    req(valores$pension_actual_rpmt)
    valueBox(
      paste0("₡", format(round(valores$pension_actual_rpmt, 0), big.mark = ",")),
      "Pensión Renta Permanente",
      icon = icon("lock"),
      color = "green"
    )
  })
  output$descargar_tabla_rp <- downloadHandler(
    filename = function() paste0("retiro_programado_", Sys.Date(), ".csv"),
    content = function(file) write.csv(valores$tabla_programado, file, row.names = FALSE)
  )
  
  output$descargar_tabla_rt <- downloadHandler(
    filename = function() paste0("renta_temporal_", Sys.Date(), ".csv"),
    content = function(file) write.csv(valores$tabla_temporal_sim, file, row.names = FALSE)
  )
  
  output$descargar_tabla_rpmt <- downloadHandler(
    filename = function() paste0("renta_permanente_", Sys.Date(), ".csv"),
    content = function(file) write.csv(valores$tabla_temporal, file, row.names = FALSE)
  )
  
  
}

shinyApp(ui = ui, server = server)

