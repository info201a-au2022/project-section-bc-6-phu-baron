library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(readxl)


# Set working directory and read data
setwd("/Users/phu/Documents/info201_assignment/project-section-bc-6-phu-baron/Coding-Folder")
df <- read_excel("Washington_Offense_Type_by_Agency_2012.xls")

# Define UI
ui <- fluidPage(
  titlePanel("2020, Washington Criminal Record"),
  tabsetPanel(
    tabPanel("About",
             fluidRow(
               column(width = 12,
                      h4("What data we are looking at"),
                      p("Here is just a sample of 10 different location data"),
                      verbatimTextOutput("sample"))
             )
    ),
    tabPanel("Plot 1",
             fluidRow(
               column(width = 12,
                      h4("Plot 1"),
                      plotOutput("plot1"))
             )
    ),
    tabPanel("Plot 2",
             sidebarLayout(
               sidebarPanel(
                 selectInput("cities", "Select a city",
                             choices = print(df$Cities))
               ),
               mainPanel(
                 plotOutput("plot2")
               )
             )
    ),
    tabPanel("Plot 3",
             fluidRow(
               column(width = 12,
                      h4("Plot 3"),
                      plotOutput("plot3"))
             )
    ),
    tabPanel("Summary",
             fluidRow(
               column(width = 12,
                      h4("Summary"),
               )
             )
    )
  )
)

# Define server
server <- function(input, output) {
  sample_data <- df %>%
    filter(Cities %in% c("Seattle","Bellevue","Tacoma", "Port Angeles", "University Place", 
                         "Redmond", "Bothell", "Yakima", "Tukwila", "Renton")) %>%
    select(Cities, Population, `Total Offenses`, `Crimes Against Persons`, `Crimes Against Property`)
  
  output$sample <- renderPrint({
    knitr::kable(sample_data, format = "markdown", row.names = FALSE)
  })
  
  output$plot1 <- renderPlot({
    # Code to generate plot 1
    plot(rnorm(100), rnorm(100), main = "Plot 1")
  })
  
  output$plot2 <- renderPlot({
    # Code to generate plot 2
    filtered_data <- filter(df, Cities == input$cities)
    plot(filtered_data$Population, filtered_data$`Total Offenses`, 
         xlab = "Population", ylab = "Total Offenses", 
         main = paste("Total Offenses vs Population"))
  })
  
  output$plot3 <- renderPlot({
    # Code to generate plot 3
    plot(rnorm(100), rnorm(100), main = "Plot 3")
  })
}

# Run the app
shinyApp(ui, server)

