library(shiny)
source("1-CompareToGS.R")

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("ACLEW Esquema de Anotación: Prueba de Gold Standard"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Annotation file ----
      fileInput("file1", "Seleccioná tu archivo de anotación",
                accept = c("text/tab-separated-values",
                         ".txt")),

      # Input: Annotated recording ----
      selectizeInput("recording", "¿Qué grabación anotaste?",
                   choices = c("2337", "5959", "VanFJ11"),
                   options = list(
                     placeholder = 'Seleccioná una grabación',
                     onInitialize = I('function() { this.setValue(""); }'))),

      # Input: Annotated minute ----
      selectizeInput("minute", "¿Qué minuto querés evaluar?",
                   choices = 1:5,
                   options = list(
                     placeholder = 'Seleccioná tu minuto evaluado',
                     onInitialize = I('function() { this.setValue(""); }'))),

      # Input: Annotator's name ----
      textInput("coder", "Nombre del anotador",
                placeholder = "Tu nombre y apellido"),

      # Input: Annotator's PI's name ----
      textInput("PI", "Nombre del laboratorio",
                placeholder = "El nombre de la jefa de tu grupo"),

      # Submit button:
      actionButton("submit", "Actualizar")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      uiOutput("report"),
      uiOutput("downloadErrors")
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  report <- eventReactive(input$submit, {
    req(input$file1, input$recording, input$minute,
        input$coder, input$PI)
    compare.files(input$file1$datapath,
                  input$recording,
                  as.numeric(input$minute),
                  input$coder,
                  input$PI)
  })

  output$report <- renderUI({
    req(report())
    
    tagList(
      tags$div(as.character(report()$compare.stmt),
               tags$br(),
               as.character(report()$coder.stmt),
               tags$br()),
      tags$h1("Configuración para la prueba"),
      tags$div("Apareamos las líneas siguientes del GS con tus líneas:"),
      renderTable(report()$tier.equiv),
      renderText(as.character(report()$tier.incons)),
      tags$div("Las líneas siguientes contienen habla en el GS durante el minuto evaluado:"),
      renderTable(report()$tiers.w.spch),
      tags$h1("Precisión"),
      tags$div("Tu precisión para el minuto evaluado (limitado a líneas apareadas con las del GS):"),
      renderTable(report()$gs.tiers.print),
      tags$div("Tus puntajes para las tres subpartes son:"),
      renderText(as.character(report()$chi.diar)),
      renderText(as.character(report()$nch.diar)),
      renderText(as.character(report()$dep.acc)),
      tags$h1("Resumen"),
      renderText(as.character(report()$summ.bad.tiers)),
      renderText(as.character(report()$summ.weighted.score)),
      renderText(as.character(report()$summ.total.subparts)),
      tags$h4(renderText(as.character(report()$pass.message))),
      tags$div("Para aprobar la prueba necesitas:"),
      renderText(as.character(report()$req.tiers)),
      renderText(as.character(report()$req.wscore)),
      renderText(as.character(report()$req.subpts)),
      tags$br()
    )
  })
  
  output$downloadErrors <- renderUI({
    # Output file name
    time.now <- gsub('-|:', '', as.character(Sys.time()))
    time.now <- gsub(' ', '_', time.now)
    
    errors <- report()$errors.tbl
  
    output$downloadErrorsHandler <- downloadHandler(
      filename = paste0("GS_comparacion-",time.now,"-errores_encontrados-",
             input$recording,"-minuto_", input$minute,
             "-por_", input$coder, "_de_", input$PI, ".csv"),
      content = function(file) {
        write.csv(errors, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    downloadButton("downloadErrorsHandler", "Descargá tu hoja de cálculo de errores")
  })
}

# Create Shiny app ----
shinyApp(ui, server)