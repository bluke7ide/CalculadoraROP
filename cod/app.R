ui <- fluidPage(
  titlePanel(div("Calculadora del Régimen Obligatorio de Pensiones (ROP)", style = "background-color: #0073e6; color: white; padding: 10px; border-radius: 5px;")),
  
  sidebarLayout(
    sidebarPanel(
      h4("Montos de los rendimientos de los años:", style = "font-weight: bold; color: #0073e6;"),
      numericInput("2023", "2023", value = 15203642, min = 18, max = 100),
      numericInput("2024", "2024", value = 9546136, min = 18, max = 100),
      numericInput("2025", "2025", value = 25462189, min = 18, max = 100),
      h4("Características:", style = "font-weight: bold; color: #0073e6;"),
      numericInput("monto", "Monto acumulado final:", value = 100000000, min = 0),
      numericInput("edad", "Edad actual:", value = 65, min = 18, max = 100),
      selectInput("sexo", "Sexo:", choices = c("Masculino", "Femenino")),
      numericInput("jub", "Año de jubilación:", value = 2025, min = 1950, max = 2100),
      actionButton("calcular", "Calcular", style = "background-color: #0073e6; color: white; border-radius: 5px;")
    ),
    
    mainPanel(
      h3("Información General", style = "font-weight: bold; margin-top: 20px; color: #0073e6;"),
      
      wellPanel(
        tags$h4("Expectativa de vida", style = "font-weight: bold; color: #0073e6;"),
        textOutput("expectativa")
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
                     tags$li("Presione el botón \"Calcular\"."),
                     tags$li("Acceda a la pestaña de Renta que sea de su interés.")
                   ),
                   br(),
                   p("Cada pestaña mostrará los resultados correspondientes al tipo de renta seleccionado.")
          ),
          tabPanel("Retiro Programado",
                   h4("Gráficos"),
                   plotOutput("graf_reserva"),
                   plotOutput("graf_pension"),
                   plotOutput("graf_rendimiento")
          ),
          tabPanel("Renta Temporal",
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
  
  ### ANTHONY
  
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

