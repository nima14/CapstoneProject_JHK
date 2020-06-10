
server <-  function(session,input,output){
  
  
  source("KatzBackoff_Model.R")
  
  wordButton <- function(word) {

    actionButton(word,word,     style = "color: #EDF5E1; background-color: #05386B")
  }
  
  ClickButton <- function(word){
    
    observeEvent(input[[word]],
                 {updateTextInput(session,"inp_Prefix",value=paste(isolate(input$inp_Prefix)
                                                                   , isolate(word)))})
  }
  
  Ngram_Words <<- readRDS("Ngram_Words.Rdata")
  
  Res <- reactive({suppressWarnings( GetObsProbs(input$inp_Prefix) )})
  
  
  output$out_Pred_table <-  renderDataTable( {
    
    Res()  }
    
    ,options = list(searching = FALSE,
                         lengthChange = FALSE,
                         info=FALSE,
                         paging=FALSE,
                        
                         initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#05386B', 'color': '#EDF5E1'});",
                           "}")
                        ,columnDefs = list(list(className = 'dt-center',targets="_all")
                          
                               )),rownames=FALSE) 
  
  
  
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


ui <-function(){
  
  bootstrapPage( 
    
  navbarPage("Word Suggestion",        

   
                
   
           tabPanel("Home",

                    # use a gradient in background
                    setBackgroundColor(
                      color = c("#379683")
                    ),
                    sidebarLayout(
                      
                      
                      sidebarPanel(
                        tags$style(".well {background-color: #EDF5E1;}"),
                        
                        
                        
     
                     
                        textAreaInput("inp_Prefix",
                                      
                                      HTML("<p> <span style='color:#05386B '>Put your sentence here:</span></p>")
                                      , height = "100px"
                                      ),
                        br(),
                      #  h1(textOutput("out_Prefix"))
                        uiOutput("words")
                        )
                      ,
                      mainPanel( 
                              dataTableOutput("out_Pred_table" , width="125px")
                        
                      )
                    )
           ),
           tabPanel("Instructions",
                    verbatimTextOutput("summary")
           ),
           navbarMenu("About Me",
                      tabPanel("Table",
                               DT::dataTableOutput("table")
                      ),
                      tabPanel("About",
                               fluidRow(
                                 column(6,
                                        "xa"
                                 ),
                                 column(3,
                                        img(class="img-polaroid",
                                            src=paste0("http://upload.wikimedia.org/",
                                                       "wikipedia/commons/9/92/",
                                                       "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                        tags$small(
                                          "Source: Photographed at the Bay State Antique ",
                                          "Automobile Club's July 10, 2005 show at the ",
                                          "Endicott Estate in Dedham, MA by ",
                                          a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                            "User:Sfoskett")
                                        )
                                 )
                               )
                      )
           )
),

tags$style(type = 'text/css', 
           HTML('.navbar { background-color: #05386B;}
                          .navbar-default .navbar-brand{color: #EDF5E1;}
                          .tab-panel{ background-color: red; color: white}
                          .navbar-default .navbar-nav > .active > a, 
                           .navbar-default .navbar-nav > .active > a:focus, 
                           .navbar-default .navbar-nav > .active > a:hover {
                                color: #05386B;
                                background-color: #EDF5E1;
                            }')
))
}






shinyApp(ui ,server)