library(tidyverse)
library(shiny)
library(plotly)

ui = fluidPage(
  titlePanel("Statistical analysis of physical problems"),
  sidebarLayout(
    sidebarPanel(helpText("Analysis is an open source platform dedicated
                          to data analysis of physical problems. For more information, visit ",
                 a("wiki documentation",href="https://github.com/flavianowilliams/analysis"),
                 " in the GitHub page."),
    fileInput("file","Upload your dataset",accept = ".csv"),
    textInput("x", h4("Informe a coluna com os valores de x"), value = "X"),
    textInput("y", h4("Informe a coluna com os valores de y"), value = "Y"),
    br(),
    actionButton("apply", label="Apply")),
#    br(),
#    downloadButton("download", label="Download")),
    mainPanel(
      navbarPage("Analysis",
                 tabPanel("Dataframe",
                          tableOutput("dataframe")
                 ),
                 tabPanel("Statistics",
                          textOutput("stats")
                 ),
                 tabPanel("Plot",
                            plotlyOutput("plot")
    )
      )
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

  output$dataframe = renderTable({currentFile()})
  
  observeEvent(input$apply, {

    data = currentFile() %>%
      mutate(x = get(input$x)) %>%
      mutate(y = get(input$y))
    
    regressao = lm(y ~ x, data = data)
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
    
    p = ggplot(data=currentFile(),aes(x=get(x),y=get(y)))+
    geom_point()+
      geom_abline(intercept = v$B, slope = v$A, colour = "red")+
      xlab(x)+
      ylab(y)
    
    ggplotly(p)
    
  })
  
  output$plot = renderPlotly({
      if (is.null(v$A)) return()
      plotreact()
  })

#  output$download = downloadHandler(
#    filename = function() {
#      paste(input$x, "_", input$y, "-", Sys.Date(), ".pdf", sep = "")
#    },
#    content = function(file) {
#    ggsave(file,plot = plotreact())
#  })

}

shinyApp(ui, server)
