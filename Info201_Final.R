library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(readxl)
library(DT)
library("ggplot2")


##install.packages("ggplot2")
##install.packages("DT")


# Set working directory and read data
##setwd("/Users/baroncabudol/Desktop/info201/FinalProject/project-section-bc-6-phu-baron")
df <- read_excel("Washington_Offense_Type_by_Agency_2022 copy.xls")

# Define UI
ui <- fluidPage(
  titlePanel("2020, Washington Criminal Record"),
  tabsetPanel(
    tabPanel("Landing",
             fluidRow(
               column(width = 12,
                      br(),
                      HTML("<h3>About The Dataset</h3>
                        <p>The dataset being used is called <b>Washington Offense Type by Agency 2020</b>. This dataset was collected by the 
                        Federal Bureau of Investigation (FBI) using the National Incident-Based Reporting System (NIBRS). It contains data for 
                        120 cities in Washington state on 60 different types of crimes.There is a chance that there may be some inaccuracies 
                        since the data is taken through human input of the police officers contributing to NIBRS.</p>
                        <br>
                        <b>Ethical Issues</b>
                        <li>This dataset can be used to make assumptions that the data could be justifies increased police funding or harsher 
                        sentencing laws for Washington State, which could lead to further oppression of margainalized people.</li>
                        <li>The dataset may contain information that reflects biased policing practices, such as over-policing in certain 
                        neighborhoods. If this bias is present in the dataset, it could perpetuate discrimination and harm marginalized communities.</li>
                        <li>This dataset can also have some inaccuracies, since NIBRS is a database updated and maintained by police, there 
                        can be input errors such as misinputs of the dates of the offenses or types of offenses. These inaccuracies can contribute
                        false ideas about a group of people in the dataset.</li>
                        <br>
                        <h4>Target Audience</h4>
                        <p>The target audience are residents of Washington State wanting to know how safe their area is, relative to how many crimes
                        were committed, and what types of crimes were committed. The target audience can also be people who are looking to move in/out 
                        based on how safe the area is. This app targets users who would want to see visually how other cities/areas are safe compard to 
                        eachother as well as what crimes are normally committed, and the frequency of the offense.  </p>
                        <br>
                        <h4>Research Questions</h4>
                        <ul>
                        <li>What area of Washington State has the highest danger level?</li>
                        <li>What type of offenses are the most common in each city?</li>
                        <li>Does a higher population have correlation between more crimes?</li>
                        </ul>
                        <br>
                        <h4>Random Sample of the Data:</h4>
                  </ul>"),
                      verbatimTextOutput("sample")),
               img(alt = "Tuition Graphic", 
                   src = "https://cdn.pulse2.com/cdn/2018/10/02045457/seattle-police.jpg"),
               
             )
    ),
    ##This is the danger level tab where users will be able to see where the most dangerous cities are
    tabPanel("Danger Level",
             fluidRow(
               column(width = 4,
                      HTML("<h4>Finding the Level of Dangerousness</h4>
           <p>Dangerous Level allows the user to see which city is the most dangerous and which is the least dangerous.
           For cities that are the most dangerous, the value would be 0.3, while for cities that are least dangerous,
           the value would be 0.1. To determine the dangerous level of a city, we take the total city population and
           divide it by the total number of offenses or reports for that city.</p>"),
                      numericInput("danger_level",
                                   "Select dangerous level (between 0.1 and 0.3): 0.1 is the lowest, while 0.3 is the highest; input any number between", min = 0.00, max = 0.03, value = 0.01)
               ),
               column(width = 8,
                      plotOutput("plot3")
               )
             ),
             tags$style(".js-irs-0 .irs-bar {height: 8px;}
              .js-irs-0 .irs-line {display: none;}
              .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge {height: 20px; width: 20px; margin-top: -6px;}
              .js-irs-0 .irs-handle {height: 20px; width: 20px; border-radius: 10px; margin-top: -6px;}
              .js-irs-0 .irs-handle:after {display: none;}
              .js-irs-0 .irs-handle:hover {box-shadow: none;}
              .js-irs-0 .irs-from, .js-irs-0 .irs-to, .js-irs-0 .irs-min, .js-irs-0 .irs-max, .js-irs-0 .irs-grid-pol {font-size: 10px !important;}
              .js-irs-0 .irs-from, .js-irs-0 .irs-to {bottom: -20px !important;}
              .js-irs-0 .irs-grid-pol {bottom: -15px !important;}") # customize the slider style
    ),
    tabPanel("A better insight",
             sidebarLayout(
               sidebarPanel(
                 selectInput("cities", "Select a city",
                             choices = unique(df$Cities)),
                 checkboxGroupInput("variables", "Select variables to display", 
                                    choices = c(colnames(df)[colnames(df) != "Cities"]),
                                    selected = c("Population", "Total Offenses", 
                                                 "Crimes Against Persons", "Crimes Against Property"))
               ),
               mainPanel(
                 plotOutput("plot2")
               )
             )
    ),
    tabPanel("Scatter Plot",
             fluidRow(
               column(width = 12,
                      HTML("<h4>      We are looking at the total offense to popultion for each city</h4>"),
                      plotOutput("plot1")),
               column(width = 12,
                      sliderInput("slider1", "Plot zoomer: 0 = zoom in, 100 = zoom out", min = 0, max = 100, value = 50))
             )
    ),
    tabPanel("Sort by Population or Total Offenses",
             sidebarLayout(
               sidebarPanel(
                 selectInput("sort_by", "Sort by", 
                             choices = list("Population (Ascending)" = "asc_pop", 
                                            "Population (Descending)" = "desc_pop",
                                            "Total Offenses (Ascending)" = "asc_off", 
                                            "Total Offenses (Descending)" = "desc_off"),
                             selectize = FALSE,
                             width = "50%"),
                 wellPanel(
                   p("This table shows Washington all the data in the dataset in ascending/descending order. 
                       You can select a sorting option in the dropdown menu to order the table by population or 
                       total offenses. You can also select an up or down arrow at the top of each column to see 
                       the top/lowest cities in ascending/descending order. If you want to find specific data 
                       you can use the search bar at the top right. "),
                 )
               ),
               mainPanel(
                 DT::dataTableOutput("table")
               )
             )
    ),
    
    tabPanel("Summary",
             fluidRow(
               column(width = 12,
                      br(),
                      HTML("<h4>Research Questions Addressed</h4>
                           <li>What area of Washington State has the highest danger level?</li>
                           <li>What type of offenses are the most common in each city?</li>
                           <li>Does a higher population have correlation between more crimes?</li>
                           <br>
                           <p>1. The area of Washington State that has the highest danger level was the city of Tukwila.<p>
                           <p>2. The most common offense in each city is crimes against property.<p>
                           <p>3. There is no correlation between the population and crime frequency.<p>
                           <br>
                           <b>Future Implications/Advancements For The Project</b>
                           <p> To further build this project we could add a feature where we can predict future trends of crime in Washington State using linear regression. We would do this by obtaining more datasets from the past 10 years to analyze on top of our 2020 data set. After that we would be able to provide users with visualization on what the future crime rates would be so that they can decide whether or not they want to move in the area. We can also implement a heat map in the future. That way users that are new to Washington State and do not know where these cities are can visualize the which cities on the map have more crime than others. <p>
                           <b> Conclusion </b>
                           <p> Our main findings was that residents in Tukwila should be aware as they have the highest danger 
                           rates by far out of all the cities in Washington. The cities Yarrow Point and Palouse have the lowest 
                           danger levels, making them the most safe city to live in. Out of the 3 main crime categories that make 
                           up the total offenses, crime against persons, crime against society, and crime against property, the most
                           common crime committed were crimes against property. Finally we found that there correlation between 
                           population and crimes committed in the scatter plot. However, there are other factors that can influence crime rates. These factors include
                           socioeconomic conditions, demographic factors, and type of policing play a significant role in a city's crime rates. Therefore,
                           we cannot say for sure that if a city has a higher population, they would experience more crime, since there are other factors that contribute to crime rates as well.<p>
                          <br>")
                      
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
    # Code to generate scatter plot
    ggplot(df, aes(x = Population, y = `Total Offenses`, color = Cities)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Population vs Total Offenses with Trend Line", 
           x = "Population", y = "Total Offenses") +
      coord_cartesian(xlim = c(min(df$Population), max(df$Population) * (input$slider1 / 100)),
                      ylim = c(min(df$`Total Offenses`), max(df$`Total Offenses`) * (input$slider1 / 100)))
  })
  
  output$plot2 <- renderPlot({
    # Code to generate plot 2
    filtered_data <- filter(df, Cities == input$cities)
    selected_variables <- input$variables
    data_to_plot <- filtered_data[, selected_variables]
    barplot(as.matrix(data_to_plot), 
            main = paste("City: ", input$cities), 
            xlab = "Category", ylab = "Count", 
            col = c("blue", "red", "green", "orange", "brown", "purple"), 
            legend.text = selected_variables)
  })
  
  output$plot3 <- renderPlot({
    # Create reactive expression for filtered data
    filtered_data <- reactive({
      danger_data <- df %>%
        select(Cities, Population, `Total Offenses`, `Crimes Against Persons`, `Crimes Against Property`) %>%
        mutate(Danger_Level = (`Total Offenses`) / Population) %>%
        arrange(desc(Danger_Level)) 
      danger_data[danger_data$Danger_Level >= input$danger_level,]
    })
    
    # Create bar chart
    ggplot(filtered_data(), aes(x = Cities, y = Danger_Level)) +
      geom_bar(stat = "identity", fill = "red") +
      labs(title = "Danger Level of Washington Cities",
           x = "City", y = "Danger Level") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  ##Render out the table to compare the highest and lowest city in each category, also makes it ascending/descending order
  output$table <- DT::renderDataTable({
    ##Drop down menu sort by ascending and descending for sort by population and total crime
    if (is.null(input$sort_by)) {
      df
    } else if (length(input$sort_by) == 1) {
      if (input$sort_by == "asc_pop") {
        df <- df[order(df$Population),]
      }
      if (input$sort_by == "desc_pop") {
        df <- df[order(df$Population, decreasing = TRUE),]
      }
      if (input$sort_by == "asc_off") {
        df <- df[order(df$`Total Offenses`),]
      }
      if (input$sort_by == "desc_off") {
        df <- df[order(df$`Total Offenses`, decreasing = TRUE),]
      }
    } else {
      # Handle multiple selection case
      if ("asc_pop" %in% input$sort_by) {
        df <- df[order(df$Population),]
      }
      if ("desc_pop" %in% input$sort_by) {
        df <- df[order(df$Population, decreasing = TRUE),]
      }
      if ("asc_off" %in% input$sort_by) {
        df <- df[order(df$`Total Offenses`),]
      }
      if ("desc_off" %in% input$sort_by) {
        df <- df[order(df$`Total Offenses`, decreasing = TRUE),]
      }
    }
    DT::datatable(df, rownames = FALSE)
  })
}

# Run the app
shinyApp(ui, server)

