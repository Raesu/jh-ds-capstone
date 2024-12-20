#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("JHU Capstone: N-gram Prediction Models"),
    h5('Author: Ryan Summe'),
    h5('2024-12-19'),
    textInput('input1', 'Enter text to generate prediction:', '',
     placeholder = '...'),
    
    textOutput('output1')
)
