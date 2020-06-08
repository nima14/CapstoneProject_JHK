server <-  function(input,output){
  
  suppressWarnings(library(dplyr))
  suppressWarnings(library(stringi))
  suppressWarnings(library(stringr))
  suppressWarnings(library(data.table))
  suppressWarnings(library(quanteda))
  suppressWarnings(library(shinydashboard))
  suppressWarnings(library(shiny))
  suppressWarnings(library(DT)) 
  source("KatzBackoff_Model.R")
  Ngram_Words <<- readRDS("Ngram_Words.Rdata")
  
  Res <- reactive({suppressWarnings( GetObsProbs(input$inp_Prefix) )})
  
  
  output$out_Pred_table <-  renderDataTable(
    
    Res(),options = list(searching = FALSE,
                         lengthChange = FALSE,
                         info=FALSE,
                         paging=FALSE) 
  )
  
  output$out_Pred <- renderDataTable( Res()[,1] ,options = list(searching = FALSE,
                                                                lengthChange = FALSE,
                                                                info=FALSE,
                                                                paging=FALSE
                                                                , selection = 'single' )
                                              )
  
  output$out_Prefix <- reactive({ input$inp_Prefix} )   
}



ui <- fluidPage(
  titlePanel("Predict The Next Word"),
  
  sidebarLayout(
    sidebarPanel(
      
      
      textInput("inp_Prefix","Put your sentence here:")
    ),
    
    mainPanel(
      dataTableOutput("out_Pred_table"),
      dataTableOutput("out_Pred"),
      h1(textOutput("out_Prefix"))
      
    )
  )
)




shinyApp(ui = ui, server = server)





