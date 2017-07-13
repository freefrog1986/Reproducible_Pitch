#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Manner Predicting Using Machine Learning"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        sliderInput(
                "slider1",
                "Select one entry from test data",
                min = 1,
                max = 5885,
                value = 2500),
        
        selectInput("select1", "Choose a model:",
                    choices = c("Regression Tree", "Random Forest")),
        submitButton("Submit")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("Plot_Tree"),
        textOutput('text_Tree'),
        textOutput("text_True"),
        plotOutput("Plot_Rf"),
        textOutput('text_Rf'),
        textOutput("text_Rf_True")
    )
  )
))
