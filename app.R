library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      #6.Add an image to the UI. 
      img(src = "test.png"),
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      #5.Allow the user to search for multiple entries simultaneously
      checkboxGroupInput("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      uiOutput("countryOutput"),
      #3.Allow the user to download your table as a .csv file
      downloadButton("downloadData"),
    ),
    mainPanel(
      #2.place plot and table in separate tabs
      tabsetPanel(
        tabPanel("Plot", plotOutput("coolplot")),
        #1. Use the DT package to turn a static table into an interactive table
        tabPanel("Table", DT::dataTableOutput("results")) 
      ),
      br(), br(),
      #4.Show the number of results found whenever the filters change.
      textOutput("summary")
    )
  )
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    if (is.null(input$typeInput)) {
      return(NULL)
    }
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })
  #1. Use the DT package to turn a static table into an interactive table.
  output$results <- DT::renderDataTable({
    filtered()
  })
  #3.Allow the user to download your table as a .csv file
  output$downloadData <- downloadHandler(
    filename = function() {
      "rawdata.csv"
    },
    content = function(file) {
      write.csv(bcl, file)
    }
  )
  #4.Show the number of results found whenever the filters change.
  output$summary <- renderText({
    paste0("We found ", nrow(filtered()), " options for you.")
  })
}

shinyApp(ui = ui, server = server)