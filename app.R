#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Define UI for application

ui <- fluidPage(
  titlePanel("Savings/Investing app"),

  fluidRow(
    column(4, 
      sliderInput(
        inputId = "initialamount",
        "Initial Amount",
        min = 1,
        max = 100000,
        value = 1000
      ),
      sliderInput(
        inputId = "anncontrib",
        "Annual Contribution",
        min = 0,
        max = 50000,
        value = 2000
      )
    ), 
    column(4, 
      sliderInput(
        inputId = "returnrate",
        "Return Rate (in %)",
        min = 0,
        max = 20,
        value = 5
      ),
      sliderInput(
        inputId = "growthrate",
        "Growth Rate (in %)",
        min = 0,
        max = 20,
        value = 2
      )
    ), 
    column(3,
      sliderInput(
        inputId = "years",
        "Years",
        min = 0,
        max = 50, 
        value = 20
      )
    ),
    column(4, 
      selectInput(
        inputId = "facet",
        label = "Facet?",
        choices = list("No" = 0, "Yes" = 1),
        selected = 0
      )
    )
  ),
  fluidRow(
    h3("Timeline"),
    plotOutput("moneyplot")
  ),
  fluidRow(
    h3("Balances"),
    dataTableOutput("summary_table")
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  future_value <- function(amount, rate, years) {
    return(amount*((1 + rate)^years))
  }
  annuity <- function(contrib, rate, years) {
    return(contrib*((1 + rate)^years - 1)/rate)
  }
  growing_annuity <- function(contrib, rate, growth, years) {
    return(contrib*((1 + rate)^years - (1 + growth)^years)/(rate - growth))
  }
  
  update <- reactive({
    rate <- input$returnrate/100
    growth <- input$growthrate/100
    contrib <- input$anncontrib
    initial <- input$initialamount
    mode1 <- c(initial)
    mode2 <- c(initial)
    mode3 <- c(initial)
    
    for (year in 1:input$years) {
      mode1 <- c(mode1, future_value(mode1[1], rate, year))
      mode2 <- c(mode2, future_value(mode2[1], rate, year) + annuity(contrib, rate, year))
      mode3 <- c(mode3, future_value(mode3[1], rate, year) + growing_annuity(contrib, rate, growth, year))
    }
    
    modalities <- data.frame("year" = 0:input$years, "no_contrib" = mode1, "fixed_contrib" = mode2, "growing_contrib" = mode3)
    modalities
  }) 
  
  output$moneyplot <- renderPlot({
    modalities <- update()
    if(input$facet == 1) {
      modalities <- gather(data = modalities, key = Modality,value = amount, c(no_contrib, fixed_contrib, growing_contrib))
      modalities$Modality <- factor(modalities$Modality, levels = c("no_contrib", "fixed_contrib", "growing_contrib"))
      ggplot(data = modalities, aes(x = year, color = Modality, fill = Modality)) + 
        geom_area(aes(y = amount)) + 
        geom_area(aes(y = amount)) + 
        geom_area(aes(y = amount)) +
        facet_wrap(~Modality) +
        labs(title = "Comparison of three investing modalities", x = "Year after start", y = "Total amount")
    } else {
      ggplot(data = modalities, aes(x = year)) + 
        geom_line(aes(y = no_contrib, color = "No contribution")) + 
        geom_line(aes(y = fixed_contrib, color = "Fixed contribution")) + 
        geom_line(aes(y = growing_contrib, color = "Growing contribution")) +
        labs(title = "Comparison of three investing modalities", x = "Year after start", y = "Total amount") +
        scale_color_manual("modality", breaks = c("No contribution", "Fixed contribution", "Growing contribution"),  values=c("red","green","blue"))
      
    }
  })
  
  output$summary_table <- renderDataTable({
    update()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
