#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# server.R
library(shiny)
library(tidyverse)
library(RColorBrewer)

shinyServer(function(input, output) {
  
  output$diaPlot <- renderPlot({
    # Filter the dataset based on user input
    dia.filtered <- diamonds %>%
      filter(price >= input$priceInput[1], price <= input$priceInput[2]) %>%
      filter(carat >= input$caratInput[1], carat <= input$caratInput[2]) %>%
      filter(cut %in% input$cutInput)
    
    # Initialize the ggplot object
    p <- ggplot(dia.filtered, aes(x = carat, y = price, color = cut)) +
      geom_point() +
      scale_colour_brewer(palette = input$colorInput) +
      labs(title = "Diamond Price vs Carat",
           x = "Carat",
           y = "Price") +
      theme_minimal()
    
    # Add linear regression line if selected
    if (input$showLinReg) {
      x_pred <- seq(min(dia.filtered$carat), max(dia.filtered$carat), length.out = 100)
      y_pred <- predict(lm(price ~ carat, data = dia.filtered), newdata = tibble(carat = x_pred))
      p <- p + geom_line(data = tibble(carat = x_pred, price = y_pred), aes(x = carat, y = price), color = "blue", size = 1)
    }
    
    # Add quadratic regression line if selected
    if (input$showSqReg) {
      x_pred <- seq(min(dia.filtered$carat), max(dia.filtered$carat), length.out = 100)
      y_pred <- predict(lm(price ~ carat + I(carat^2), data = dia.filtered), newdata = tibble(carat = x_pred))
      p <- p + geom_line(data = tibble(carat = x_pred, price = y_pred), aes(x = carat, y = price), color = "red", size = 1)
    }
    
    # Add cubic regression line if selected
    if (input$showCubReg) {
      x_pred <- seq(min(dia.filtered$carat), max(dia.filtered$carat), length.out = 100)
      y_pred <- predict(lm(price ~ carat + I(carat^2) + I(carat^3), data = dia.filtered), newdata = tibble(carat = x_pred))
      p <- p + geom_line(data = tibble(carat = x_pred, price = y_pred), aes(x = carat, y = price), color = "green", size = 1)
    }
    
    # Return the plot
    p
  })
  
  output$stattable <- renderTable({
    # Filter the dataset based on user input
    dia.filtered <- diamonds %>%
      filter(price >= input$priceInput[1], price <= input$priceInput[2]) %>%
      filter(carat >= input$caratInput[1], carat <= input$caratInput[2]) %>%
      filter(cut %in% input$cutInput)
    
    # Define the regression models
    model.lin <- lm(price ~ carat, data = dia.filtered)
    model.sq <- lm(price ~ carat + I(carat^2), data = dia.filtered)
    model.cub <- lm(price ~ carat + I(carat^2) + I(carat^3), data = dia.filtered)
    
    # Summarize the models
    model.lin.sum <- summary(model.lin)
    model.sq.sum <- summary(model.sq)
    model.cub.sum <- summary(model.cub)
    
    # Create the results table
    tablemodelres <- matrix(c("Linear regression", "Square regression", "Cubic regression",
                              round(model.lin.sum$r.squared, 2), round(model.sq.sum$r.squared, 2), round(model.cub.sum$r.squared, 2),
                              round(model.lin.sum$adj.r.squared, 2), round(model.sq.sum$adj.r.squared, 2), round(model.cub.sum$adj.r.squared, 2),
                              model.lin.sum$df[2], model.sq.sum$df[2], model.cub.sum$df[2]), 
                            ncol = 4, byrow = TRUE)
    
    # Define the column names
    colnames(tablemodelres) <- c("Model", "R-squared", "Adjusted R-squared", "Degrees of Freedom")
    
    # Return the table
    tablemodelres
  })
  
  output$statout <- renderText({
    # Filter the dataset based on user input
    dia.filtered <- diamonds %>%
      filter(price >= input$priceInput[1], price <= input$priceInput[2]) %>%
      filter(carat >= input$caratInput[1], carat <= input$caratInput[2]) %>%
      filter(cut %in% input$cutInput)
    
    >
    # Summarize the models
    model.lin.sum <- summary(model.lin)
    model.sq.sum <- summary(model.sq)
    model.cub.sum <- summary(model.cub)
    
    # Add names to the models
    model.lin.sum[["name"]] <- "Linear regression"
    model.sq.sum[["name"]] <- "Square regression"
    model.cub.sum[["name"]] <- "Cubic regression"
    
    # Determine the best model based on adjusted R-squared
    best_model <- if((model.lin.sum$adj.r.squared > model.sq.sum$adj.r.squared) & 
                     (model.lin.sum$adj.r.squared > model.cub.sum$adj.r.squared)){
      model.lin.sum[["name"]]
    } else if((model.sq.sum$adj.r.squared > model.lin.sum$adj.r.squared) & 
              (model.sq.sum$adj.r.squared > model.cub.sum$adj.r.squared)){
      model.sq.sum[["name"]]
    } else {
      model.cub.sum[["name"]]
    }
    
    # Return the explanatory text
    paste0("Explaining the association between Diamond Price, Carat and Cut. The best fitting model is: ", best_model)
  })
})
