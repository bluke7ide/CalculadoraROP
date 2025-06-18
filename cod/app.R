ui <- fluidPage(
  titlePanel(div("Calculadora del Régimen Obligatorio de Pensiones (ROP)", style = "background-color: #0073e6; color: white; padding: 10px; border-radius: 5px;")),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("monto", "Monto acumulado:", value = 100, min = 0),
      h5("Rendimientos de los años", style = "font-weight: bold; color: #333;"),
      fluidRow(
        column(4, numericInput("tasa1", "2023", value = 5, min = 0, max = 100)),
        column(4, numericInput("tasa2", "2024", value = 4, min = 0, max = 100)),
        column(4, numericInput("tasa3", "2025", value = 3, min = 0, max = 100))
      ),
      numericInput("edad", "Edad actual:", value = 20, min = 18, max = 100),
      selectInput("sexo", "Sexo:", choices = c("Masculino", "Femenino")),
      numericInput("jub", "Año de jubilación:", value = 0, min = 1950, max = 2100),
      actionButton("calcular", "Calcular", style = "background-color: #0073e6; color: white; border-radius: 5px;")
    ),
    
    mainPanel(
      h3("Información General", style = "font-weight: bold; margin-top: 20px; color: #0073e6;"),
      
      wellPanel(
        tags$h4("Expectativa de vida", style = "font-weight: bold; color: #0073e6;"),
        textOutput("expectativa")
      ),
      
      h3("Seleccionar tipo de retiro", style = "font-weight: bold; margin-top: 30px; color: #0073e6;"),
      
      tabsetPanel(
        selected = NULL,  # Ninguna pestaña activa al inicio
        tabsetPanel(
          selected = NULL,  # Ninguna pestaña activa al inicio
          tabPanel("Retiro Programado",
                   h4("Tabla de resultados"),
                   tableOutput("tabla_programado"),
                   h4("Gráficos"),
                   plotOutput("graf_reserva"),
                   plotOutput("graf_pension"),
                   plotOutput("graf_rendimiento")
          ),
          tabPanel("Renta Temporal",
                   h4("Tabla de resultados"),
                   tableOutput("tabla_temporal_sim"),
                   h4("Gráficos"),
                   plotOutput("graf_reserva_temp"),
                   plotOutput("graf_pension_temp"),
                   plotOutput("graf_rendimiento_temp")
          ),
          tabPanel("Renta Permanente",
                   textOutput("renta_permanente")
          )
        )
      )
    )
  )
)

tabla <- read.csv("tavid2000-2150 - qxhasta2150-v2018.csv")

server <- function(input, output) {
  # (ANTONI) LEER MIS FUNCIONES
  source("retiro_programado.R")
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
    valores$edad <- input$edad
    valores$expectativa <- input$edad + 20
    valores$retiro_programado <- input$monto * 0.03
    valores$renta_permanente <- input$monto * 0.02
    valores$renta_temporal <- input$monto * 0.04
    sexo_cod <- ifelse(input$sexo == "Masculino", 1, 2)
    tasas_usuario <- intereses(input$edad)
    edad_retiro <- input$edad + (input$jub - 2025)
    
    ### ANTHONY
    # VANU Completa y simulación
    vanu_comp <- vanuCompleta(edad_inicio = edad_retiro, sexo = sexo_cod, tabla = tabla, anno_objetivo = input$jub)
    
    vanu_temp <- vanuTemporal(edad_inicio = edad_retiro, sexo = sexo_cod, tabla = tabla, anno_objetivo = input$jub)
    
    sim <- simular_reserva(input$monto, vanu_comp, tasas_usuario, edad_retiro = edad_retiro)
    
    sim_temp <- simular_reserva_temporal(input$monto, vanu_temp, tasas_usuario)
    plots <- graficos(sim)
    
    # VANU Temporal y su simulación
    plots_temp <- graficos(sim_temp)
    
    # Guardar resultados
    valores$tabla_programado <- sim
    valores$graficos <- plots
    valores$tabla_temporal <- vanu_temp
    valores$tabla_temporal_sim <- sim_temp
    valores$graficos_temporal <- plots_temp
    ###
    
  })
  
  # Mostrar resultados solo cuando existan
  output$expectativa <- renderText({
    req(valores$expectativa, valores$edad)
    paste("Su expectativa de vida condicionado a que tiene", valores$edad, "años es de:", valores$expectativa)
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
  
  ### ANTHONY
  
  output$tabla_programado <- renderTable({
    req(valores$tabla_programado)
    valores$tabla_programado
  })
  
  output$graf_reserva <- renderPlot({
    req(valores$graficos)
    print(valores$graficos$Reserva)
  })
  
  output$graf_pension <- renderPlot({
    req(valores$graficos)
    print(valores$graficos$Pension)
  })
  
  output$graf_rendimiento <- renderPlot({
    req(valores$graficos)
    print(valores$graficos$Rendimiento)
  })
  
  output$tabla_temporal <- renderTable({
    req(valores$tabla_temporal)
    valores$tabla_temporal
  })
  
  output$tabla_temporal_sim <- renderTable({
    req(valores$tabla_temporal_sim)
    valores$tabla_temporal_sim
  })
  
  output$graf_reserva_temp <- renderPlot({
    req(valores$graficos_temporal)
    print(valores$graficos_temporal$Reserva)
  })
  
  output$graf_pension_temp <- renderPlot({
    req(valores$graficos_temporal)
    print(valores$graficos_temporal$Pension)
  })
  
  output$graf_rendimiento_temp <- renderPlot({
    req(valores$graficos_temporal)
    print(valores$graficos_temporal$Rendimiento)
  })
  
}

shinyApp(ui = ui, server = server)

