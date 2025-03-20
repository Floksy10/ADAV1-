#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# ui.R
library(shiny)
library(tidyverse)
library(RColorBrewer)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Diamond Price vs Carat"),
  
  # Description
  p("This application allows you to explore the relationship between diamond price and carat, and filter the data based on price, carat, and cut ranges. Additionally, you can select different color palettes to visualize the data and add regression lines."),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      # Slider for price range
      sliderInput("priceInput",
                  "Price Range:",
                  min = 0, max = 20000, value = c(500, 10000), step = 500),
      # Slider for carat range
      sliderInput("caratInput",
                  "Carat Range:",
                  min = 0, max = 5, value = c(0.2, 2.5), step = 0.1),
      # Checkbox for diamond cut
      checkboxGroupInput("cutInput",
                         "Diamond Cut:",
                         choices = levels(diamonds$cut),
                         selected = levels(diamonds$cut)),
      # Select input for color palette
      selectInput("colorInput",
                  "Color Palette:",
                  choices = rownames(brewer.pal.info),
                  selected = "Set1"),
      # Checkboxes for regression lines
      checkboxInput("showLinReg", "Show Linear Regression", value = TRUE),
      checkboxInput("showSqReg", "Show Quadratic Regression", value = TRUE),
      checkboxInput("showCubReg", "Show Cubic Regression", value = TRUE)
    ),
    
    # Show the generated plots
    mainPanel(
      # Sub-Heading 
      h4("Diamond Price vs Carat Scatter Plot"),
      
      # Main Graphical Output
      plotOutput("diaPlot"),
      
      # Sub-Heading for Table
      h4("Model Summary Table"),
      
      # Table Output
      tableOutput("stattable"),
      
      # Sub-Heading for Text
      h4("Model Explanation"),
      
      # Text Output
      textOutput("statout")
    )
  )
))
