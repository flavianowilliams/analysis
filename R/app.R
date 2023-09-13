library(tidyverse)
library(shiny)

ui = fluidPage(
  titlePanel("Results of the MD simulation of condensed systems"),
  
  sidebarLayout(
    sidebarPanel(helpText("Analysis is an open source platform dedicated
                          to data analysis of physical problems. For more information, visit ",
                 a("wiki documentation",href="https://github.com/flavianowilliams/analysis"),
                 " in the GitHub page."),
    fileInput("file","Upload your dataset",accept = ".csv"),
    textInput("x", h3("Valores de x"), value = "x"),
    textInput("y", h3("Valores de y"), value = "y"),
    br(),
    actionButton("apply", label="Apply"),
    br(),
    #    selectInput("variable",
#                label = "Enter a variable",
#                choices = list("Volume"="volume",
#                               "Temperature"="temperature",
#                               "Pressure"="pressure",
#                               "Intramolecular energy"="intramolecular",
#                               "Intermolecular energy"="intermolecular",
#                               "Energy"="energy"),
#                selected = "energy"),
    downloadButton("download", label="Download")),
    mainPanel(
      textOutput("stats"),
      plotOutput("plot")
    )
  )
)

# Define server logic ----
server = function(input, output) {
  
  v = reactiveValues(data = NULL)
  
  currentFile = reactive({
    file = input$file
    ext = tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please, upload a csv file!"))
    read.csv2(file$datapath)
  })

  observeEvent(input$apply, {
    
    x = input$x
    y = input$y
    regressao = lm(y~x, data = currentFile())
    summary(regressao)
    stats_df = summary(regressao)$coefficients
    
    v$A = stats_df["x", "Estimate"]
    v$B = stats_df["(Intercept)", "Estimate"]
    
  })
  
  output$stats = renderText({
    
    if (is.null(v$A)) return()
    paste("Coeficiente angular:", v$A)
    
  })

    plotreact = eventReactive(input$apply, {
    x = input$x
    y = input$y
    ggplot(data=currentFile(),aes(x=x,y=y))+
      geom_point()+
      geom_abline(intercept = v$B, slope = v$A, colour = "red")+
#      annotate("text", x=0.75*max(x), y = 0.4*max(y), label=sprintf("y = %1.2f", 1.1))
      xlab(x)+
      ylab(y)
  })
  
  output$plot = renderPlot({
      if (is.null(v$A)) return()
      plotreact()
  })

  output$download = downloadHandler(
    filename = function() {
      paste(input$x, "_", input$y, "-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
    ggsave(file,plot = plotreact())
  })

}

shinyApp(ui, server)
