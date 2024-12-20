#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(data.table)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  ngrams <- reactive({
    print('loading data set...')
    fread('3gramOver1.csv')
  })
  
  output$output1 <- renderText({

    str <- input$input1
    if (is.na(str) | stringr::str_length(str) < 1) {
      return('Waiting for input...')
    }
    
    last2 <- str |> stringr::str_extract('[^ ]* [^ ]*$')
    
    if (is.na(last2)) {
      return('Invalid input - we need at least 2 words!')
    }
    
    pred <- ngrams()[query == last2, predict][1]
    return(pred)
  })
  
    # renderText({ input$text })
}


