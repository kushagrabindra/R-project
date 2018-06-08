library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("sentiment.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("Sentiments Histogram"),
 
    sidebarPanel(
      sliderInput("sentimentInput", "airline_sentiment_confidence", 0.0, 1.0, c(0.05, 0.45), pre = "#"),
      actionButton("goButton", "Go!"),
      plotOutput("distPlot"),
      radioButtons("typeInput", "airline",
                   choices = c("Virgin America","United","Southwest","Delta","US Airways","American"),
                   selected = "United"),
      selectInput("nreasonInput", "negativereason",
                  choices = c("Bad Flight","Can't Tell","Late Flight","Customer Service Issue","Flight Booking Problems","Lost Luggage","Flight Attendant Complaints","Cancelled Flight","Damaged Luggage","longlines")) 
      ),
       
       mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
  )
)

server <- function(input, output) {
  output$coolplot <- renderPlot({
    filtered <-
      bcl %>%
      filter(airline_sentiment_confidence >= input$sentimentInput[1],
             airline_sentiment_confidence <= input$sentimentInput[2],
             airline==input$typeInput,
             negativereason==input$nreasonInput
      )         
    
    ggplot(filtered, aes(airline_sentiment_confidence)) +
      geom_histogram()
  })
  
  output$results <- renderTable({
    filtered <-
      bcl %>%
      filter(airline_sentiment_confidence >= input$sentimentInput[1],
             airline_sentiment_confidence <= input$sentimentInput[2],
             airline==input$typeInput,
             negativereason==input$nreasonInput
            )
    
    filtered
  })
  output$distPlot <- renderPlot({
    # Take a dependency on input$goButton. This will run once initially,
    # because the value changes from NULL to 0.
    input$goButton
    
    # Use isolate() to avoid dependency on input$obs
    dist <-isolate((input$sentimentInput))
    boxplot(dist)
  })
  
}

shinyApp(ui = ui, server = server)
