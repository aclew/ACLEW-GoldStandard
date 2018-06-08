library(shiny)
source("1-CompareToGS.R")

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("ACLEW Annotation Scheme: Gold Standard Test"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Annotation file ----
      fileInput("file1", "Choose your annotation file",
                accept = c("text/tab-separated-values",
                         ".txt")),

      # Input: Annotated recording ----
      selectizeInput("recording", "Which recording did you annotate?",
                   choices = c("2337", "5271", "5959", "9909", "VanFJ11"),
                   options = list(
                     placeholder = 'Select a recording below',
                     onInitialize = I('function() { this.setValue(""); }'))),

      # Input: Annotated minute ----
      selectizeInput("minute", "Which minute do you want to test?",
                   choices = 1:5,
                   options = list(
                     placeholder = 'Select a test minute below',
                     onInitialize = I('function() { this.setValue(""); }'))),

      # Input: Annotated recording ----
      selectizeInput("native", "Are you a native speaker of the language in the recording?",
                   choices = c("Yes", "No"),
                   options = list(
                     placeholder = 'Select an option below',
                     onInitialize = I('function() { this.setValue(""); }'))),

      # Input: Last chance ----
      selectizeInput("evaltype", "What type of test do you want to conduct on your file?",
                   choices = c("Normal", "Last-chance"),
                   options = list(
                     placeholder = 'Select an option below',
                     onInitialize = I('function() { this.setValue(""); }'))),

      # Input: Annotator's name ----
      textInput("coder", "Annotator name",
                placeholder = "your first and last name"),

      # Input: Annotator's PI's name ----
      textInput("PI", "Lab name",
                placeholder = "your lab PI's last name"),

      # Submit button:
      actionButton("submit", "Update")
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
                  input$native,
                  as.numeric(input$minute),
                  input$evaltype,
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
      tags$h1("Set up for evaluation"),
      tags$div("We matched the following GS tiers to your tiers:"),
      renderTable(report()$tier.equiv),
      renderText(as.character(report()$tier.incons)),
      tags$div("The following tiers had speech in the GS during the test minute:"),
      renderTable(report()$tiers.w.spch),
      tags$h1("Accuracy reports"),
      tags$div("Your accuracy for the test minute is as follows (limited to GS-matching tiers):"),
      renderTable(report()$gs.tiers.print),
      tags$div("Your weighted scores for non-CHI diarization and xds are as follows:"),
      renderText(as.character(report()$nch.diar)),
      renderText(as.character(report()$xds.acc)),
      tags$h1("Summary"),
      renderText(as.character(report()$summ.weighted.score)),
      renderText(as.character(report()$summ.bad.tiers)),
      tags$h4(renderText(as.character(report()$pass.message))),
      tags$div("To pass you needed:"),
      renderText(as.character(report()$req.wscore)),
      renderText(as.character(report()$req.tiers.univ)),
      renderText(as.character(report()$req.tiers.lgsp)),
      tags$br()
    )
  })
  
  output$downloadErrors <- renderUI({
    # Output file name
    time.now <- gsub('-|:', '', as.character(Sys.time()))
    time.now <- gsub(' ', '_', time.now)
    
    errors <- report()$errors.tbl
  
    output$downloadErrorsHandler <- downloadHandler(
      filename = paste0("GS_comparison-",time.now,"-detected_errors-",
             input$recording,"-minute_", input$minute,
             "-by_", input$coder, "_from_", input$PI, ".csv"),
      content = function(file) {
        write.csv(errors, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    downloadButton("downloadErrorsHandler", "Download your error spreadsheet")
  })
}

# Create Shiny app ----
shinyApp(ui, server)