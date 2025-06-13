ui <- fluidPage(
  titlePanel(div("Calculadora del Régimen Obligatorio de Pensiones (ROP)", style = "background-color: #0073e6; color: white; padding: 10px; border-radius: 5px;")),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("monto", "Monto acumulado:", value = 100, min = 0),
      h5("Rendimientos de los años (%)", style = "font-weight: bold; color: #333;"),
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
      h3("Resultado del Cálculo", style = "font-weight: bold; margin-top: 20px; color: #0073e6;"),
      wellPanel(
        tags$h4("Expectativa de vida", style = "font-weight: bold; color: #0073e6;"),
        textOutput("expectativa")
      ),
      wellPanel(
        tags$h4("Monto inicial de pensión", style = "font-weight: bold; color: #0073e6;"),
        fluidRow(
          column(4, textOutput("retiro_programado")),
          column(4, textOutput("renta_permanente")),
          column(4, textOutput("renta_temporal"))
        )
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$calcular, {
    expectativa <- input$edad + 20
    retiro_programado <- input$monto * 0.03
    renta_permanente <- input$monto * 0.02
    renta_temporal <- input$monto * 0.04
    
    output$expectativa <- renderText({
      paste("Su expectativa de vida condicionado a que tiene", input$edad, "años es de:", expectativa)
    })
    
    output$retiro_programado <- renderText({
      paste("Retiro Programado: ", round(retiro_programado, 2))
    })
    
    output$renta_permanente <- renderText({
      paste("Renta Permanente: ", round(renta_permanente, 2))
    })
    
    output$renta_temporal <- renderText({
      paste("Renta Temporal: ", round(renta_temporal, 2))
    })
  })
}

shinyApp(ui = ui, server = server)
