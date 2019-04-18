#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#1. Initial 
library(shiny)
library(ggplot2)
library(reshape)

#2. Define UI 
ui <- fluidPage(
  
  #App title
  titlePanel("Investment Modality Comparison"),
  
  #Sidebar  
  fluidRow(
    column(4,
           sliderInput("initial",
                       label = "Initial Amount",
                       min = 0,
                       max = 100000,
                       value = 1000,
                       pre = "$"),
           sliderInput("annual_contrib",
                       label = "Annual Contribution",
                       min = 0,
                       max = 50000,
                       value = 2000,
                       pre = "$")),
    column(4,
           sliderInput("rate",
                       label = "Return rate (in %)",
                       min = 0,
                       max = 20,
                       value = 5
           ),
           sliderInput("g",
                       label = "Growth rate (in %)",
                       min = 0,
                       max = 20,
                       value = 2)),
    column(4,
           sliderInput("year",
                       label = "Years",
                       min = 0,
                       max = 50,
                       value = 20),
           selectInput("facet",
                       label = "Facet?",
                       choices = c("No", "Yes")
           ))
    
    
  ),
  
  #3. Plot of relative frequencies
  mainPanel(
    h4(textOutput("plot_title")),
    plotOutput("timeline", width = "150%"),
    h4(textOutput("balance_title")),
    tableOutput("balances")
  )
)




#4. Server logic
server <- function(input, output) {
  
  future_value <- function(amount, rate, years){
    result <- amount*(1+rate)^years
    return(result)
  }
  annuity <- function(contrib, rate, years){
    result <- contrib*(((1+rate)^years - 1)/rate)
    return(result)
  }
  growing_annuity <- function(contrib, rate, growth, years){
    result <- contrib*(((1+rate)^years - (1+growth)^years)/(rate-growth))
    return(result)
  }
  
  #5. Finish plot
  
  output$timeline <- renderPlot({
    year <- 0:input$year
    no_contrib <- rep(0,input$year+1)
    fixed_contrib <- rep(0,input$year+1)
    growing_contrib <- rep(0,input$year+1)
    
    #a) no contrib
    for (i in 0:input$year){
      no_contrib[i+1] <- future_value(amount = input$initial, rate = input$rate/100, years = i)
    }
    #b) fixed contrib
    for (i in 0:input$year){
      fixed_contrib[i+1] <- future_value(amount = input$initial, rate = input$rate/100, years = i) + 
        annuity(contrib = input$annual_contrib, rate = input$rate/100, years = i)
    }
    #c) growing contrib
    for (i in 0:input$year){
      growing_contrib[i+1] <- future_value(amount = input$initial, rate = input$rate/100, years = i) + 
        growing_annuity(contrib =  input$annual_contrib, rate = input$rate/100, growth = input$g/100, years = i)
    }
    
    raw_table <- data.frame(year = year, no_contrib = no_contrib, fixed_contrib = fixed_contrib, growing_contrib = growing_contrib)
    mods <- melt(raw_table,"year")
    options(scipen=10000)
    original <- ggplot(data = mods, aes(x = year,y = value,color = variable)) +
      geom_line(size = 3) +
      geom_point(shape = 17, size = 5)+
      ggtitle("Investing Modalities")
    
    if (input$facet == "No") {
      print(original)
    }
    else{
      facetted <- original + facet_wrap(~variable)
      print(facetted)
    }
    
  })
  
  #6 Finish table
  output$plot_title <- renderText("Timelines")
  output$balance_title <- renderText("Balances")
  output$balances <- renderTable({
    year <- 0:10
    no_contrib <- rep(0,11)
    fixed_contrib <- rep(0,11)
    growing_contrib <- rep(0,11)
    
    #no contrib
    for (i in 0:10){
      no_contrib[i+1] <- future_value(amount = input$initial, rate = input$rate/100, years = i)
    }
    #fixed contrib
    for (i in 0:10){
      fixed_contrib[i+1] <- future_value(amount = input$initial, rate = input$rate/100, years = i) + 
        annuity(contrib = input$annual_contrib, rate = input$rate/100, years = i)
    }
    #growing contrib
    for (i in 0:10){
      growing_contrib[i+1] <- future_value(amount = input$initial, rate = input$rate/100, years = i) + 
        growing_annuity(contrib =  input$annual_contrib, rate = input$rate/100, growth = input$g/100, years = i)
    }
    mods <- data.frame(year = year, no_contrib = no_contrib, fixed_contrib = fixed_contrib, growing_contrib = growing_contrib)
  })
  
}

#7. Run
shinyApp(ui = ui, server = server)