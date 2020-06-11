suppressWarnings(library(dplyr))
suppressWarnings(library(stringi))
suppressWarnings(library(stringr))
suppressWarnings(library(data.table))
suppressWarnings(library(quanteda))
suppressWarnings(library(shinydashboard))
suppressWarnings(library(shiny))
suppressWarnings(library(DT)) 
suppressWarnings(library(shinyWidgets))

server <-  function(session,input,output){
  
  #Get the model function:  
     source("KatzBackoff_Model.R")
  
  
 
    # Load N-grams: 
    Ngram_Words <<- readRDS("Ngram_Words.Rdata")
  
  
    # Run the model:
    Res <- reactive({
          suppressWarnings( GetObsProbs(input$inp_Prefix) )
                })
  

#DataTable-----------------------------------------------------------------------------------        
    #  Create DataTable 
    output$out_Pred_table <-  
    renderDataTable( {
                       Res()
                     }
    # Remove Search , paging , ... from DataTable              
                    ,options = list(searching = FALSE,
                         lengthChange = FALSE,
                         info=FALSE,
                         paging=FALSE,
    # Change the color of the header of the DataTable                       
                         initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#05386B', 'color': '#EDF5E1'});",
                           "}")
                          ,columnDefs = list(list(className = 'dt-center',targets="_all")
                            
                               )),rownames=FALSE
                        ) 
  
  #-------------------------------------------------------------------------------------

    output$out_Prefix <- reactive({ input$inp_Prefix} )  
  
    # Get just the words from the result of the model: 
    Preds <- reactive({ as.list(Res()[,1])[[1]] })
  
    # Function for creating buttons
    wordButton <- function(word)
    {
      
      actionButton(word,word, 
                   style = "color: #EDF5E1; background-color: #05386B")
    }
    
 
    #  Create Button for all the suggested words:
    output$words <- 
        renderUI({
            lapply(as.list(Preds()), wordButton)
              })
  
    
  #------------------------------------------------------------------------------  
    # Put each suggested word in a variable:
    Pred1 <- reactive({Res()[1,1][[1]]})
    Pred2 <- reactive({Res()[2,1][[1]]})
    Pred3 <- reactive({Res()[3,1][[1]]})
    Pred4 <- reactive({Res()[4,1][[1]]})
    Pred5 <- reactive({Res()[5,1][[1]]})
    
    # When clicking the button, update the text input:
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

#-------------------------------------------------------------------------------
ui <-function(){
  
      bootstrapPage( 
        
      navbarPage("Word Suggestion",        
    
       
#Tab1-------------------------------------------------------------------                    
       
               tabPanel("Home",
    
                            # Color of the background
                            setBackgroundColor(
                              color = c("#f3f3f3")
                            ),
                            sidebarLayout(
                          
                          
                                sidebarPanel(
                                  #Color of sidebar's background:
                                  tags$style(".well {background-color: #d3d3d3;}"),
                                  
                            
                            
         
                         
                                    textAreaInput("inp_Prefix",
                                                  #Change the color of textInput's label
                                                  HTML("<p> <span style='color:#05386B '>
                                                          Put your sentence here:</span></p>")
                                                  , height = "100px"
                                                  ),
                                    br(),
                                    uiOutput("words"),
                                    br(),
                                    h3(textOutput("out_Prefix"))
                            
                
      
                            )
                          ,
                          mainPanel( 
                            #Add logo
                                  align="center",
                                     HTML('<footer>
                                          <img src="logos.png"</img>
                                               </footer>'),
                                        br(),
                                        br(),
                                        br(),
                                                     
                                  dataTableOutput("out_Pred_table" , width="125px")
                          
                          )
                        )
               ),
               
#Tab2------------------------------------------------------------------------------

               tabPanel("Instructions",
                        includeMarkdown("Instruction.Rmd")
                          ),
     
#Tab3------------------------------------------------------------------------------                          
               tabPanel("About",
                                   includeMarkdown("About.Rmd") 
                          )
    ),
    
    # Chnage the color of Navbar:
    
    tags$style(type = 'text/css', 
               HTML('.navbar { background-color: #05386B;}
                              .navbar-default .navbar-brand{color: #EDF5E1;}
                              .tab-panel{ background-color: red; color: white}
                              .navbar-default .navbar-nav > .active > a, 
                               .navbar-default .navbar-nav > .active > a:focus, 
                               .navbar-default .navbar-nav > .active > a:hover {
                                    color: #05386B;
                                    background-color: #EDF5E1;
                                }
                    .navbar-default .navbar-nav>li>a {color: #EDF5E1;}
                    .navbar .nav > li.current-menu-item > a, .navbar .nav > li.current-menu-ancestor > a, 
                    .navbar .nav > li > a:hover, .navbar .nav > li > a:focus {
                    color: #f5e1ed;}')
    ))
    }


shinyApp(ui ,server)