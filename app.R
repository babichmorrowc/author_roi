library(shiny)
library(DT)
library(tidyverse)

# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("ROI Calculator"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    # Input: Enter series being advertised ----
    textInput("series", "Which series are you advertising?"),
    
    # Input: Enter month and year ----
    dateRangeInput("dates", "What date range?", start = Sys.Date() - lubridate::days(30)),
    
    # Input: Enter retailer ----
    textInput("retailer", "What retailer?", value = "Amazon"),
    
    # Input: Enter country ----
    textInput("country", "What country?", value = "United States"),
    
    # Input: Enter sales of book 1 ----
    numericInput("book1_sales", "Book 1 sales", value = 1),
    
    # Input: Enter sales of book 2 ----
    numericInput("book2_sales", "Book 2 sales", value = 1),
    
    # Input: Enter sales of book 3 ----
    numericInput("book3_sales", "Book 3 sales", value = 1),
    
    # Input: Enter sales of book 4 ----
    numericInput("book4_sales", "Book 4 sales", value = 1),
    
    # Input: Enter price of book 1 ----
    numericInput("book1_price", "Book 1 price", value = 2.99),
    
    # Input: Enter price of book 2 ----
    numericInput("book2_price", "Book 2 price", value = 2.99),
    
    # Input: Enter price of book 3 ----
    numericInput("book3_price", "Book 3 price", value = 2.99),
    
    # Input: Enter price of book 4 ----
    numericInput("book4_price", "Book 4 price", value = 2.99),
    
    # Input: Enter sales of book 4 ----
    numericInput("book4_sales", "Book 4 sales", value = 1),
    
    # # Input: Enter royalty percentage of book 1 ----
    # numericInput("book1_royalty", "Book 1 royalty percentage", value = 60),
    # 
    # # Input: Enter royalty percentage of book 2 ----
    # numericInput("book2_royalty", "Book 2 royalty percentage", value = 60),
    # 
    # # Input: Enter royalty percentage of book 3 ----
    # numericInput("book3_royalty", "Book 3 royalty percentage", value = 60),
    # 
    # # Input: Enter royalty percentage of book 4 ----
    # numericInput("book4_royalty", "Book 4 royalty percentage", value = 60),
    
    # Input: Factor in KU? ----
    checkboxInput("ku_estimation", "Are your books in Kindle Unlimited?", value = TRUE),
    
    # Kindle Unlimited panel ----
    conditionalPanel(
      condition = "input.ku_estimation == TRUE",
      
      # Input: Enter payment per page ----
      numericInput("payment_per_page", "Payment Per Page", value = 0.0044),
      
      # Input: Enter page reads of book 1 ----
      numericInput("book1_pagereads", "Book 1 page reads", value = 60),
      
      # Input: Enter page reads of book 2 ----
      numericInput("book2_pagereads", "Book 2 page reads", value = 60),
      
      # Input: Enter page reads of book 3 ----
      numericInput("book3_pagereads", "Book 3 page reads", value = 60),
      
      # Input: Enter page reads of book 4 ----
      numericInput("book4_pagereads", "Book 4 page reads", value = 60),
      
      # Input: Enter page count of book 1 ----
      numericInput("book1_pagecount", "Book 1 page count", value = 400),
      
      # Input: Enter page count of book 2 ----
      numericInput("book2_pagecount", "Book 2 page count", value = 400),
      
      # Input: Enter page count of book 3 ----
      numericInput("book3_pagecount", "Book 3 page count", value = 400),
      
      # Input: Enter page count of book 4 ----
      numericInput("book4_pagecount", "Book 4 page count", value = 400)
    )
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    # Output: text for caption ----
    h1(textOutput("caption")),
    
    h2(textOutput("subcaption")),
    
    h3("Sales revenue"),
    
    DTOutput('salestable'),
    
    h3("KU revenue"),
    
    DTOutput('kutable')
    
    # conditionalPanel(
    #   condition = "input.ku_estimation == TRUE",
    #   DTOutput('kutable')
    # )
    
    # p(textOutput("nonKU_return"))
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  captionText <- reactive({
    paste0("ROI for ", input$series)
  })
  
  subcaptionText <- reactive({
    paste0("Sold on ", input$retailer, " in ", input$country)
  })
  
  # Return the text for printing
  output$caption <- renderText({
    captionText()
  })
  
  # Return the text for printing
  output$subcaption <- renderText({
    subcaptionText()
  })
  
  
  sales_table <- reactive({
    data.frame("Book" = c("Book 1", "Book 2", "Book 3", "Book 4"),
               "Sales" = c(input$book1_sales, input$book2_sales, input$book3_sales, input$book4_sales),
               "Price" = c(input$book1_price, input$book2_price, input$book3_price, input$book4_price)) %>%
      mutate(Royalty = ifelse(Price >= 2.99, 0.7, 0.2)) %>%
      mutate(Revenue = Price * Royalty) %>%
      mutate("Sell-through" = ifelse(Book == "Book 1", 1, Sales / input$book1_sales)) %>%
      mutate("Return on conversion" = Revenue * `Sell-through`)
    
  })
  
  ku_table <- reactive({
      data.frame("Book" = c("Book 1", "Book 2", "Book 3", "Book 4"),
                 "Page Reads" = c(input$book1_pagereads, input$book2_pagereads, input$book3_pagereads, input$book4_pagereads),
                 "Page Count" = c(input$book1_pagecount, input$book2_pagecount, input$book3_pagecount, input$book4_pagecount)) %>%
        mutate("Units Read" = Page.Reads / `Page.Count`) %>%
        mutate("Sell-through" = ifelse(Book == "Book 1", 1, `Units Read` / `Units Read`[Book == "Book 1"])) %>%
        mutate("KU Revenue" = 0.0044 * `Page.Count` * `Sell-through`)
    })
    
  output$salestable = renderDataTable(sales_table())
  
  output$kutable <- renderDataTable(ku_table())
  
  # nonKU_returnText <- reactive({
  #   paste0("For every book 1 sold or downloaded, you earn $", rv$sales_table$`Return on conversion`, " via sales, taking into account both the royalty rate and the percentage sell-through.")
  # })
  # 
  # output$nonKU_return <- renderText({
  #   nonKU_returnText()
  # })
  
}

shinyApp(ui, server)