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
                   textOutput("retiro_programado")
          ),
          tabPanel("Renta Temporal",
                   textOutput("renta_temporal")
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
  # Variables reactivas para almacenar los resultados
  valores <- reactiveValues(
    edad = NULL,
    expectativa = NULL,
    retiro_programado = NULL,
    renta_permanente = NULL,
    renta_temporal = NULL
  )
  
  observeEvent(input$calcular, {
    valores$edad <- input$edad
    valores$expectativa <- input$edad + 20
    valores$retiro_programado <- input$monto * 0.03
    valores$renta_permanente <- input$monto * 0.02
    valores$renta_temporal <- input$monto * 0.04
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
}

shinyApp(ui = ui, server = server)

