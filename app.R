library(shiny)
library(DT)
library(tidyverse)
library(shinyWidgets)

#### UI ####
ui <- fluidPage(
  theme = "app.css",
  setBackgroundColor(
    color = c("#bfa3e5", "#aac0de"),
    gradient = "linear",
    direction = "bottom"
  ),
  
  # App title ----
  titlePanel(h1("ROI Calculator")),
  
 tabsetPanel(
   #### About tab ####
   tabPanel("About",
            fluid = TRUE,
            sidebarLayout(
              sidebarPanel(
                img(src = "billboard_cropped1.jpg", width = "100%")
              ), # Close sidebarPanel
              mainPanel(
                h1("About"),
                "Happy anniversary to my bad-ass girlfriend ❤️ I will make this section more professional-sounding someday, but for now, enjoy my incredibly nerdy labor of love. I am so proud of your work, every single day, and I hope you always remember exactly how bad-ass you are.",
                img(src = "couplepic_ferriswheel.png", width = "80%")
              ) # Close mainPanel
            ) # Close sidebarLayout
   ), # Close About tab
   
   #### Sales tab ####
    tabPanel("Sales",
             fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 # Input: Enter sales of book 1
                 numericInput("book1_sales", "Book 1 sales", value = 1),
                 
                 # Input: Enter sales of book 2
                 numericInput("book2_sales", "Book 2 sales", value = 1),
                 
                 # Input: Enter sales of book 3
                 numericInput("book3_sales", "Book 3 sales", value = 1),
                 
                 # Input: Enter sales of book 4 
                 numericInput("book4_sales", "Book 4 sales", value = 1),
                 
                 # Input: Enter price of book 1 
                 numericInput("book1_price", "Book 1 price", value = 2.99),
                 
                 # Input: Enter price of book 2 
                 numericInput("book2_price", "Book 2 price", value = 2.99),
                 
                 # Input: Enter price of book 3 
                 numericInput("book3_price", "Book 3 price", value = 2.99),
                 
                 # Input: Enter price of book 4
                 numericInput("book4_price", "Book 4 price", value = 2.99)
               ), # Close sidebarPanel
             mainPanel(
               h1("Sales Revenue"),
               DTOutput('salestable'),
               textOutput("salesvalue"),
               "See Details for more information about royalty percentages."
             ) # Close mainPanel
    ) # Close sidebarLayout
  ), # Close Sales tab
  
  #### KU tab ####
    tabPanel("Kindle Unlimited",
             fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 # Input: Enter payment per page
                 numericInput("payment_per_page", "Payment Per Page", value = 0.0044),
                 
                 # Input: Enter page reads of book 1
                 numericInput("book1_pagereads", "Book 1 page reads", value = 60),
                 
                 # Input: Enter page reads of book 2 
                 numericInput("book2_pagereads", "Book 2 page reads", value = 60),
                 
                 # Input: Enter page reads of book 3 
                 numericInput("book3_pagereads", "Book 3 page reads", value = 60),
                 
                 # Input: Enter page reads of book 4 
                 numericInput("book4_pagereads", "Book 4 page reads", value = 60),
                 
                 # Input: Enter page count of book 1 
                 numericInput("book1_pagecount", "Book 1 page count", value = 400),
                 
                 # Input: Enter page count of book 2 
                 numericInput("book2_pagecount", "Book 2 page count", value = 400),
                 
                 # Input: Enter page count of book 3 
                 numericInput("book3_pagecount", "Book 3 page count", value = 400),
                 
                 # Input: Enter page count of book 4 
                 numericInput("book4_pagecount", "Book 4 page count", value = 400)
               ), # close sidebarPanel
             mainPanel(
               h1("Kindle Unlimited Revenue"),
               DTOutput('kutable'),
               textOutput("kuvalue")
             ) # close mainPanel
    ) # Close sidebarLayout
    ), # Close KU tab
  #### Average conversion tab ####
  tabPanel("Average conversion",
           fluid = TRUE,
           sidebarLayout(
             sidebarPanel(
               img(src = "dollarbill2.jpg", width = "100%")
             ), # Close sidebarPanel
             mainPanel(
               h1("Average overall conversion"),
               textOutput("overallavg")
             ) # Close mainPanel
           ) # Close sidebarLayout
  ), # Close Avg conversion tab
  #### Details tab ####
  tabPanel("Details",
           fluid = TRUE,
           sidebarLayout(
             sidebarPanel(
               img(src = "billboard_cropped2.jpg", width = "100%")
             ), # Close sidebarPanel
             mainPanel(
               h1("Details"),
               "• For books costing $2.99 and up, royalty percentages are set at 70%.",
               verbatimTextOutput("<br>"),
               "• For books less than $2.99, royalties are set at 20%."
             ) # Close mainPanel
           ) # Close sidebarLayout
  ) # Close Details tab
) # close tabsetPanel
) # Close UI
    
#### SERVER ####

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
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
    
  output$salestable = renderDataTable(sales_table(), rownames = FALSE)
  
  output$kutable <- renderDataTable(ku_table(), rownames = FALSE)
  
  output$salesvalue <- renderText({
    sales_sum <- sum(sales_table()$`Return on conversion`)
    paste("For every copy of book 1 sold, you earn", sales_sum, " accounting for the royalty rate and the percentage sell-through .")
  })
  
  output$kuvalue <- renderText({
    ku_sum <- sum(ku_table()$`KU Revenue`)
    paste("For every conversion of book 1, you earn approximately", ku_sum, "for pages read across the whole series.")
  })
  
  output$overallavg <- renderText({
    sales_ku_avg <- mean(sum(sales_table()$`Return on conversion`), sum(ku_table()$`KU Revenue`))
    paste("If you have Kindle Unlimited income, the approximate value of your overall first conversion is",
          round(sales_ku_avg, 2), ".")
  })
  
}

shinyApp(ui, server)
