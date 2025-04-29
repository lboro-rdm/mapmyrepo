library(shiny)
library(shinycssloaders)
library(lubridate)
library(plotly)
library(colourpicker)

ui <- tags$html(
  lang = "en",
  fluidPage(
    style = "padding: 0px; margin: 0px;",
    tags$head(
      tags$title("Map My Repository"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
  
  # Black banner
  tags$div(
    class = "black-banner",
    tags$div(
      class = "banner-content",
      tags$a(
        href = "https://www.lboro.ac.uk",
        target = "_blank",
        tags$img(src = "logo.png", class = "uni-logo", alt = "University Logo")
      ),
      tags$a(
        href = "https://www.lboro.ac.uk/services/library/",
        target = "_blank",
        class = "return-link",
        "University Library"
      )
    )
  ),
  
  # Blue banner
  tags$div(
    class = "blue-banner",
    tags$div(
      class = "banner-content",
      tags$span("Open Research Services"),
      tags$a(
        href = "https://repository.lboro.ac.uk/",
        class = "return-link",
        "< Return to Research Repository"
      )
    )
  ),
  
  # Your existing UI code
  navbarPage(
    title = NULL,
  
  # First Tab ----
  tabPanel("Map My Repository",
           fluidPage(
             # Application title
             p(HTML("This app maps the downloads and views of a set of items from the <a href='https://repository.lboro.ac.uk'>Loughborough Research Repository</a>.")),
             p("Upload your file, make sure it is a CSV with a single column of item IDs, with a header. 
               (If you aren't sure of the file structure, download a file from the next tab.) Next, select your parameters."),
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
                           value = Sys.Date() - 1,       # Default to today
                           max = Sys.Date() - 1),         # Latest selectable date
                 
                 colourInput("colour1", "Choose a colour for the high end of the scale:", value = "#cc6b00"),
                 colourInput("colour2", "Choose a colour for the low end of the scale:", value = "#ffb74d"),
                 
                 # Download button for the CSV file
                 downloadButton("downloadData", "Download metric data as CSV"),
                 
                 p(),
                 
                 # Download button for the JPEG map
                 downloadButton("downloadMap", "Download map as JPEG"),
                 
                 fluidRow(
                   column(12,
                          HTML("<p></p>
                     <p></p>
                     <p style='font-size: 10px;'>The sample data is from the <a href='https://doi.org/10.17028/rd.lboro.c.7105741'>Open Research Week 2024</a> recordings.</p>")
                   )
                 )
               ),
               # Main panel for displaying outputs
               mainPanel(
                 # Output: Plot with a spinner
                 withSpinner(plotlyOutput("heatMapPlot"), type = 3, color = "#8D9C27", color.background = "white")
               )
             )
           )
  ),
  
  # Second Tab ----
  tabPanel("Get CSV Files",
           p("You can create your own custom CSV files, or you can generate one by searching on a researcher's name, a group, collection or project number."),
           p(HTML("<a href='https://doi.org/10.17028/rd.lboro.28882631' target='_blank'>This presentation</a> shows you how to find the group, collection or project number.")),
           
           sidebarPanel(
             selectInput("searchType", "Search by:",
                         choices = c("Researcher", "Collection", "Project", "Group"),
                         selected = "Researcher"),
             textInput("searchQuery", "Enter search term:"),
             actionButton("searchButton", "Search"),
             p(),
             downloadButton("downloadSearch", "Download item IDs as a CSV")
           ),
             mainPanel(
               withSpinner(DT::dataTableOutput("searchResults"), type = 3, color = "#8D9C27", color.background = "white")
             )
           ),

# Third Tab ---------------------------------------------------------------

  tabPanel("About this app",
           p("The data for this app is pulled from Figshare. While Figshare does exclude known bots, the rise of LLMs has made this more challenging. It is assumed that the vast majority of views and downloads, particularly from USA, are bots."),
           p("This app was created in R, with the following packages:"),
             tags$ul(
               tags$li(
                 "Attali, D. (2023). ",
                 tags$i("colourpicker: A Colour Picker Tool for Shiny and for Selecting Colours in Plots"),
                 ". R package version 1.3.0. ",
                 tags$a(href = "https://CRAN.R-project.org/package=colourpicker", "https://CRAN.R-project.org/package=colourpicker")
               ),
               
               tags$li(
                 "Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y., Allen, J., McPherson, J., Dipert, A., & Borges, B. (2024). ",
                 tags$i("shiny: Web Application Framework for R"),
                 ". R package version 1.9.1. ",
                 tags$a(href = "https://CRAN.R-project.org/package=shiny", "https://CRAN.R-project.org/package=shiny")
               ),
               
               tags$li(
                 "Grolemund, G., & Wickham, H. (2011). Dates and Times Made Easy with ",
                 tags$a(href = "https://www.jstatsoft.org/v40/i03/", "lubridate"),
                 ". Journal of Statistical Software, 40(3), 1-25. ",
                 tags$a(href = "https://www.jstatsoft.org/v40/i03/", "https://www.jstatsoft.org/v40/i03/")
               ),
               
               tags$li(
                 "Massicotte, P., & South, A. (2023). ",
                 tags$em("rnaturalearth: World Map Data from Natural Earth"),
                 ". R package version 1.0.1. ",
                 tags$a(href = "https://CRAN.R-project.org/package=rnaturalearth", "https://CRAN.R-project.org/package=rnaturalearth")
               ),
               
               tags$li(
                 "Ooms, J. (2014). “The jsonlite Package: A Practical and Consistent Mapping Between JSON Data and R Objects.” ",
                 tags$em("arXiv:1403.2805 [stat.CO]"),
                 ". ",
                 tags$a(href = "https://arxiv.org/abs/1403.2805", "https://arxiv.org/abs/1403.2805")
               ),
               
               tags$li(
                 "Ooms, J. (2023). ",
                 tags$em("openssl: Toolkit for Encryption, Signatures and Certificates Based on OpenSSL"),
                 ". R package version 2.1.1. ",
                 tags$a(href = "https://CRAN.R-project.org/package=openssl", "https://CRAN.R-project.org/package=openssl")
               ),
               
               tags$li(
                 "Pebesma, E., & Bivand, R. (2023). Spatial Data Science: With Applications in R. Chapman and Hall/CRC. ",
                 tags$a(href = "https://doi.org/10.1201/9780429459016", "https://doi.org/10.1201/9780429459016")
               ),
               
               tags$li(
                 "Pebesma, E. (2018). Simple Features for R: Standardized Support for Spatial Vector Data. The R Journal, 10(1), 439-446. ",
                 tags$a(href = "https://doi.org/10.32614/RJ-2018-009", "https://doi.org/10.32614/RJ-2018-009")
               ),
               
               tags$li(
                 "Sali, A., & Attali, D. (2020). ",
                 tags$i("shinycssloaders: Add Loading Animations to a 'shiny' Output While It's Recalculating"),
                 ". R package version 1.0.0. ",
                 tags$a(href = "https://CRAN.R-project.org/package=shinycssloaders", "https://CRAN.R-project.org/package=shinycssloaders")
               ),
               
               tags$li(
                 "South, A., Michael, S., & Massicotte, P. (2024). ",
                 tags$em("rnaturalearthdata: World Vector Map Data from Natural Earth Used in 'rnaturalearth'"),
                 ". R package version 1.0.0. ",
                 tags$a(href = "https://CRAN.R-project.org/package=rnaturalearthdata", "https://CRAN.R-project.org/package=rnaturalearthdata")
               ),
               
               tags$li(
                 "Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L. D., François, R., Grolemund, G., Hayes, A., Henry, L., Hester, J., Kuhn, M., Pedersen, T. L., Miller, E., Bache, S. M., Müller, K., Ooms, J., Robinson, D., Seidel, D. P., Spinu, V., Takahashi, K., Vaughan, D., Wilke, C., Woo, K., & Yutani, H. (2019). “Welcome to the tidyverse.” ",
                 tags$em("Journal of Open Source Software"),
                 ", 4(43), 1686. ",
                 tags$a(href = "https://doi.org/10.21105/joss.01686", "https://doi.org/10.21105/joss.01686")
               ),
               
               tags$li(
                 "Wickham, H. (2023). ",
                 tags$i("httr: Tools for Working with URLs and HTTP"),
                 ". R package version 1.4.7. ",
                 tags$a(href = "https://CRAN.R-project.org/package=httr", "https://CRAN.R-project.org/package=httr")
               ),
               
               tags$li(
                 "Xie, Y., Cheng, J., & Tan, X. (2024). ",
                 tags$em("DT: A Wrapper of the JavaScript Library 'DataTables'"),
                 ". R package version 0.33. ",
                 tags$a(href = "https://CRAN.R-project.org/package=DT", "https://CRAN.R-project.org/package=DT")
               ),
               
               tags$li(
                 "Sievert, C. (2020). Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC Florida, 2020."
               ),
               
               tags$li(
                 "Urbanek, S. (2015). ",
                 tags$em("base64enc: Tools for base64 encoding"),
                 ". R package version 0.1-3. ",
                 tags$a(href = "https://CRAN.R-project.org/package=base64enc", "https://CRAN.R-project.org/package=base64enc")
             )
             ),
           p("This app has been created and maintained by Lara Skelly for Loughborough University, copyright: MIT. The code can be found on ", a("GitHub", href = "https://github.com/lboro-rdm/mapmyrepo.git", target = "_blank")),
           
           p(),
          p("To cite this item: Skelly, Lara (2024). Map my repository: a R/Shiny app. Loughborough University. Software. ", 
          tags$a(href = "https://doi.org/10.17028/rd.lboro.26335771", "https://doi.org/10.17028/rd.lboro.26335771"),
             ),
           p(),
  p("Throughout the creation of this Shiny app, ChatGPT acted as a conversation partner and a code checker. This app was developed with code coaching from ",
    tags$a(href = "https://orcid.org/0000-0002-3039-6849", "Charlotte Hadley")),
  
           p(),
           p("This webapp does not use cookies or store any data on your device."),
           p(),
           p("This app was funded by the ", a("Water Engineering and Development Centre", href = "https://www.lboro.ac.uk/research/wedc/"))
  )
  )
  ),          
           
  tags$div(class = "footer", 
           fluidRow(
             column(12, 
                    tags$a(href = 'https://doi.org/10.17028/rd.lboro.28525481', 
                           "Accessibility Statement")
             )
           )
  )
)


