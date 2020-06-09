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
    #div(word, class = "btn btn-info", id=word)
    actionButton(word,word)
  }
  
  ClickButton <- function(word){
    
    #observeEvent(input$word, {word}
    observeEvent(input[[word]],output$Click <-   renderText(word))
    #observeEvent(input$word,output$wordtext <-   renderText(word))
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
  
  
  
  output$words <- renderUI({
    lapply(as.list(Res()[,1])[[1]], wordButton)
                    })
  

  Pred1 <- reactive({Res()[1,1][[1]]})
  Pred2 <- reactive({Res()[2,1][[1]]})
  Pred3 <- reactive({Res()[3,1][[1]]})
  Pred4 <- reactive({Res()[4,1][[1]]})
  Pred5 <- reactive({Res()[5,1][[1]]})
  
  
  output$words <- renderUI({actionButton(Res()[1,1][[1]],Res()[1,1][[1]])})
  
  observeEvent(input[[Res()[1,1][[1]]]],{updateTextInput(session,"inp_Prefix",value=paste(isolate(input$inp_Prefix)
                                                                                          , isolate(Res()[1,1][[1]])))})
  
    #reactive({lapply(as.list(Res()[,1])[[1]], ClickButton)})
  
  
   #observeEvent(input$go,output$x <-   renderText("test"))
   
 
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





