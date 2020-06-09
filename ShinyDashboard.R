suppressWarnings(library(dplyr))
suppressWarnings(library(stringi))
suppressWarnings(library(stringr))
suppressWarnings(library(data.table))
suppressWarnings(library(quanteda))
suppressWarnings(library(shinydashboard))
suppressWarnings(library(shiny))
suppressWarnings(library(DT)) 


server <-  function(session,input,output){
  
  suppressWarnings(library(dplyr))
  suppressWarnings(library(stringi))
  suppressWarnings(library(stringr))
  suppressWarnings(library(data.table))
  suppressWarnings(library(quanteda))
  suppressWarnings(library(shinydashboard))
  suppressWarnings(library(shiny))
  suppressWarnings(library(DT)) 
  source("KatzBackoff_Model.R")
  
  wordButton <- function(word) {
                        
                        actionButton(word,word)
  }
  
  ClickButton <- function(word){
    
                observeEvent(input[[word]],
                 {updateTextInput(session,"inp_Prefix",value=paste(isolate(input$inp_Prefix)
                                                                   , isolate(word)))})
  }
  
  Ngram_Words <<- readRDS("Ngram_Words.Rdata")
  
  Res <- reactive({suppressWarnings( GetObsProbs(input$inp_Prefix) )})
  
  
  output$out_Pred_table <-  renderDataTable(
    
                    Res(),options = list(searching = FALSE,
                         lengthChange = FALSE,
                         info=FALSE,
                         paging=FALSE) 
  )
  
  
  output$out_Prefix <- reactive({ input$inp_Prefix} )  
  
  
  Preds <- reactive({ as.list(Res()[,1])[[1]] })
  
  
  output$words <- renderUI({
                              lapply(as.list(Preds()), wordButton)
                            })
  

  Pred1 <- reactive({Res()[1,1][[1]]})
  Pred2 <- reactive({Res()[2,1][[1]]})
  Pred3 <- reactive({Res()[3,1][[1]]})
  Pred4 <- reactive({Res()[4,1][[1]]})
  Pred5 <- reactive({Res()[5,1][[1]]})
  
  observeEvent(input[[Pred1()]],
               {updateTextInput(session,"inp_Prefix",
                                value=paste(isolate(input$inp_Prefix), isolate(Pred1())))})
  
  observeEvent(input[[Pred2()]],
               {updateTextInput(session,"inp_Prefix",
                                value=paste(isolate(input$inp_Prefix), isolate(Pred2())))})
  observeEvent(input[[Pred3()]],
               {updateTextInput(session,"inp_Prefix",
                                value=paste(isolate(input$inp_Prefix), isolate(Pred3())))})
  observeEvent(input[[Pred4()]],
               {updateTextInput(session,"inp_Prefix",
                                value=paste(isolate(input$inp_Prefix), isolate(Pred4())))})
  observeEvent(input[[Pred5()]],
               {updateTextInput(session,"inp_Prefix",
                                value=paste(isolate(input$inp_Prefix), isolate(Pred5())))})
  
  

  
  
  
  
}



ui <- fluidPage(
  titlePanel("Predict The Next Word"),
  
  sidebarLayout(
    sidebarPanel(
      
      
      textInput("inp_Prefix","Put your sentence here:")
    ),
    
    mainPanel(
                  dataTableOutput("out_Pred_table"),
                  h1(textOutput("out_Prefix")),
                  uiOutput("words"),
                  # actionButton("go", "Go"),
                # verbatimTextOutput("x"),
                  verbatimTextOutput("Click")

    )
  )
)




shinyApp(ui = ui, server = server)





