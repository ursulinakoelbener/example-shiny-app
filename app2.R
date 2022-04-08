library(shiny) # Package for shiny applications
library(dplyr) # Package for data manipulations
library(magrittr) # Package for pipe operator
library(ggplot2) # Package for creating graphs
library(shinythemes)

options(dplyr.summarise.inform = FALSE)

# course_data <- readRDS("data/europe.rds") %>% # Load the course data
# mutate(AvgTemperatureC = round((AvgTemperatureF - 32)*5/9, 1)) # Create a new column with Avg Temperature in Celsius

# Increas Uploadsize for files ----
options(shiny.maxRequestSize = 30*1024^2)

# Define UI for application
ui <- fluidPage(
  
  theme = shinytheme(theme = "united"),
  
  # Application title
  titlePanel("COURSE SHINY APP"),
  
  sidebarLayout(
    # Sidebar panel
    sidebarPanel(
      "This is the sidebar panel",
      
      #Input: Upload a RDS file ----
      fileInput(inputId = "file", label = "Upload a file (RDS)",
                multiple = FALSE,
                accept = c(".rds")),
      
      # Input: A simple slider ----
      sliderInput(inputId = "slider_year", label = "Year:",
                  min = 2000,
                  max = 2019,
                  step = 1,
                  value = 2000,
                  sep = ""),
      
      # Input: A simple drop down list  ----
      selectInput(inputId = "drop_down_country", label = "Country:",
                  choices = NULL),
      
      # Input: A simple drop down list  ----
      selectInput(inputId = "drop_down_cities", label = "City:",
                  choices = NULL),
      
      # Input: A simple text input  ----
      textInput(inputId = "text_input", label = "Input text here:"),
      
      # Input: A simple radio button input  ----
      radioButtons(inputId = "radio_temp", label = "Temperatures Scale:",
                   choices = list("Celsius" = "AvgTemperatureC", "Fahrenheit" = "AvgTemperatureF"),
                   selected = "AvgTemperatureC"),
      
      # Input: Action button ----
      actionButton(inputId = "button", label = "get started"),
      
      # Input: Download file ----
      downloadButton(outputId = "download", label = "Download")
      
    ),
    
    # Main panel
    mainPanel(
      h3("This is the main panel"),
      
      p(textOutput(outputId = "text_output")),
      
      # Layout: Tabset with info, data, and plots tabs ----
      tabsetPanel(type = "pills",
                  tabPanel(title = "Info",
                           h3("App Description"),
                           p("This is the course shiny app. It is created during the course 
                           exercises using the europe.rds data:", br(), 
                             strong("Average daily temperatures"),"(in Fahrenheit) from cities around
                           Europe from 2000 to 2019"),
                           
                           verbatimTextOutput("data_summary")
                  ),
                  tabPanel(title = "Data",
                           
                           dataTableOutput("data_table")
                  ),
                  tabPanel(title = "Plots",
                           fluidRow(
                             column(width = 12,plotOutput("lineplot"))
                             ,
                             fluidRow(
                               column(width = 6, plotOutput("boxplot")),
                               column(width = 6, plotOutput("lineplot_temp"))
                             )
                           )
                           
                  )
      )
    )
  )
)

# Define server side logic
server <- function(input, output, session) {
  
  # Reactive Expression 1----
  text <- reactive({
    paste("Your inputs are: Year:", input$slider_year, 
          ", Country:", input$drop_down_country, 
          ", City:", input$drop_down_cities, input$text_input, 
          ", Temp:", input$radio_temp)
  })
  
  # Reactive Expression 2 ----
  country_df <- eventReactive(input$button,{
    course_data() %>%
      filter(Year >= input$slider_year) %>% # Subset the rows to keep data more than or equal to a year
      filter(Country == input$drop_down_country) # Subset the rows to keep a specific country
  })
  
  # Reactive Expression 3----
  city_df <- reactive({
    country_df() %>% 
      filter(City == input$drop_down_cities) %>% # Subset the rows for specific City
      filter(Year == input$slider_year) # Subset the rows for specific Year
  })
  
  #Reactive Expression 4----
  year_df <- eventReactive(input$button, {
    country_df() %>% 
      filter(City == input$drop_down_cities) %>% # Subset the rows for specific City
      filter(Year == input$slider_year) %>%  # Subset the rows for specific Year
      group_by(Country, City, Year, Month) %>% 
      summarise(MinTempF = min(AvgTemperatureF),
                MeanTempF = round(mean(AvgTemperatureF), 1),
                MaxTempF = max(AvgTemperatureF),
                MinTempC = min(AvgTemperatureC),
                MeanTempC = round(mean(AvgTemperatureC), 1),
                MaxTempC = max(AvgTemperatureC)) %>% 
      ungroup()
  })
  
  #Reactive Expression 5----
  course_data <- eventReactive(input$file, { 
    readRDS(input$file$datapath) %>% 
      mutate(AvgTemperatureC = round((AvgTemperatureF - 32)*5/9, 1)) # Create a new column with Avg Temperature in Celsius
    
  })
  
  # Output: Render a text output  ----
  output$text_output <- renderText({
    text()
  })
  
  # Output: Render a print output  ----
  output$data_summary <- renderPrint({
    summary(course_data())
  })
  
  # Output: Render a (dynamic) table output  ----
  output$data_table <- renderDataTable({
    city_df()
  })
  
  # Output: Render a plot output  ----
  output$lineplot <- renderPlot({
    ggplot(data = city_df()) +
      geom_line(mapping = aes(x = Date, y = AvgTemperatureF), size = 1) +
      ylab("Average daily temperatures (in Fahrenheit)")
  })
  
  # Output: Render a plot output  ----
  output$boxplot <- renderPlot({
    ggplot(data = country_df()) +
      geom_boxplot(mapping = aes(x = Month, y = AvgTemperatureF, group = Year))
  })
  
  # Output: Render a plot output  ----
  output$lineplot_temp <- renderPlot({
    if(input$radio_temp == "AvgTemperatureF"){
      res <- ggplot(data = year_df()) +
        geom_line(mapping = aes(x = Month, y = MinTempF), 
                  size = 1, colour = "red", linetype = "dotted") +
        geom_line(mapping = aes(x = Month, y = MeanTempF), 
                  size = 1, colour = "black") +
        geom_line(mapping = aes(x = Month, y = MaxTempF), 
                  size = 1, colour = "red", linetype = "dotted") +
        scale_x_discrete(name = "", limits = month.abb) +
        ylab("Average daily temperatures (in Fahrenheit)")
    }
    
    if(input$radio_temp == "AvgTemperatureC"){
      res <- ggplot(data = year_df()) +
        geom_line(mapping = aes(x = Month, y = MinTempC), 
                  size = 1, colour = "red", linetype = "dotted") +
        geom_line(mapping = aes(x = Month, y = MeanTempC), 
                  size = 1, colour = "black") +
        geom_line(mapping = aes(x = Month, y = MaxTempC), 
                  size = 1, colour = "red", linetype = "dotted") +
        scale_x_discrete(name = "", limits = month.abb) +
        ylab("Average daily temperatures (in Celsius)")
    }
    
    return (res)
    
  })
  
  # Output: Downloadable csv dataset ---
  output$download <- downloadHandler(
    filename = "temp_data.csv",
    content = function(file) {
      write.csv(course_data(), file, row.names = FALSE)
    }
  )
  
  # Observer 1 ----
  observe({
    new_choice_country <- unique(course_data()$Country)
    updateSelectInput(session, inputId = "drop_down_country", 
                      choices = new_choice_country)
  }
  )
  
  # Observer 2 ----
  observe({
    new_choice_city <- unique(course_data()$City[course_data()$Country == input$drop_down_country])
    updateSelectInput(session, inputId = "drop_down_cities", 
                      choices = new_choice_city)
  }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
