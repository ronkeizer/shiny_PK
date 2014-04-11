
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("PK example"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("cl", 
                "Clearance:", 
                min = 1, 
                max = 50, 
                value = 5),
    sliderInput("v", 
                "Volume of distribution:", 
                min = 5, 
                max = 200, 
                value = 25),
    sliderInput("ka", 
                "Ka:", 
                min = .05, 
                max = 5, 
                value = 1),
    sliderInput("n_doses", 
                "Number of doses:", 
                min = 1, 
                max = 10, 
                value = 5),
    sliderInput("interval", 
                "Dosing interval:", 
                min = 6, 
                max = 168, 
                value = 24)    
  ),  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("pk_plot")
  )
))
