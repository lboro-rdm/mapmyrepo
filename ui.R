library(shiny)
library(shinycssloaders)
library(shinythemes)
library(lubridate)

# Define UI for the app
shinyUI(fluidPage(
  tags$head(
    includeCSS("www/custom.css")  # Link to your custom CSS file
  ),
                  # Application title
                  titlePanel("Map your repository"),
                  p(HTML("This app maps the downloads and views of a set of item from the <a href='https://repository.lboro.ac.uk'>Loughborough Research Repository</a>.")),
                  p("Upload your file, make sure it is a CSV with a single column of item IDs, with a header. 
                    If you aren't sure of the file structure, download the current file and replace the item IDs with
                    the set you are interested in. Next, select your parameters."),
  p("Note that the system only allows for one year of data to be mapped at any one time."),
                  p(em("Our servers are slow - please be patient :)")),
                  
                  # Sidebar layout with a sidebar and main panel
                  sidebarLayout(
                    
                    # Sidebar panel for inputs
                    sidebarPanel(
                      # File upload input
                      fileInput("file1", 
                                "Upload CSV File",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")),
                      
                      # Dropdown menu for selecting metric (views or downloads)
                      selectInput("metric", 
                                  label = "Select Metric:",
                                  choices = list("Downloads" = "downloads", "Views" = "views"),
                                  selected = "downloads"),
                      
                      # Date input for start date
                      dateInput("start_date", 
                                label = "Start Date:",
                                value = Sys.Date() - 365,  # Default to 30 days ago
                                max = Sys.Date()),        # Latest selectable date
                      
                      dateInput("end_date", 
                                label = "End Date:",
                                value = Sys.Date(),       # Default to today
                                min = Sys.Date() - 365,   # Earliest selectable date
                                max = Sys.Date()),         # Latest selectable date
                      
                      # Download button for the CSV file
                      downloadButton("downloadData", "Download data as CSV"),
                      
                      p(),
                      
                      # Download button for the JPEG map
                      downloadButton("downloadMap", "Download map as JPEG"),
                      
                      fluidRow(
                        column(12,
                               HTML("<p></p>
                     <p></p>
                     <p style='font-size: 10px;'>The sample data is from the <a href='https://doi.org/10.17028/rd.lboro.c.7105741'>Open Research Week 2024</a> recordings.</p>
                     <p style='font-size: 10px;'>The code for this app can be found at <a href='https://github.com/lboro-rdm/mapmyrepo.git'>GitHub</a>.</p>
                     <p style='font-size: 10px;'>To cite this item: Skelly, Lara (2024). Map my repository: a R/Shiny app. Loughborough University. Software. <a href='https://doi.org/10.17028/rd.lboro.26335771'>https://doi.org/10.17028/rd.lboro.26335771</a> </p>
          ")
                        )
                      )
                    ),
                    # Main panel for displaying outputs
                    mainPanel(
                      # Output: Plot with a spinner
                      withSpinner(plotOutput("heatMapPlot"), type = 3, color = "#6F3092", color.background = "white")
                    )
                  )
))
