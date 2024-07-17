library(shiny)
library(shinycssloaders)
library(shinythemes)
library(lubridate)

# Define UI for the app
shinyUI(fluidPage(theme = shinytheme("united"),
                  
                  # Application title
                  titlePanel("Map your repository"),
                  p("This app maps the downloads and views of a set of item from the Loughborough Research Repository."),
                  p("Upload your file, make sure it is a CSV with a single column of data, with a header. 
                    Then select your parameters."),
                  p("The sample data is from the", a("Open Research Week 2024", href = "https://doi.org/10.17028/rd.lboro.c.7105741"), "recordings."),
                  p("Our servers are slow - please be patient :)"),
                  
                  # Sidebar layout with a sidebar and main panel
                  sidebarLayout(
                    
                    # Sidebar panel for inputs
                    sidebarPanel(
                      # Dropdown menu for selecting metric (views or downloads)
                      selectInput("metric", 
                                  label = "Select Metric:",
                                  choices = list("Downloads" = "downloads", "Views" = "views"),
                                  selected = "downloads"),
                      
                      # Date input for start date
                      dateInput("start_date", 
                                label = "Start Date:",
                                value <- "2024-02-26",  # Default to earliest date
                                max = Sys.Date()),  # Latest selectable date
                      
                      # Date input for end date
                      dateInput("end_date", 
                                label = "End Date:",
                                value = Sys.Date(),  # Default to today
                                min = "2024-04-24",  # Earliest selectable date
                                max = Sys.Date()),  # Latest selectable date
                      
                      # Download button for the CSV file
                      downloadButton("downloadData", "Download data as CSV"),
                      
                      p(),
                      
                      # Download button for the JPEG map
                      downloadButton("downloadMap", "Download map as JPEG")
                    ),
                    
                    # Main panel for displaying outputs
                    mainPanel(
                      # Output: Plot with a spinner
                      withSpinner(plotOutput("heatMapPlot"))
                    )
                  )
))
