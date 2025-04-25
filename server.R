library(shiny)
library(httr)
library(jsonlite)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(openssl)
library(base64enc)
library(DT)
library(tidyverse)
library(plotly)

# Define server logic
shinyServer(function(input, output, session){
  
  # Map server code ----
  
  # Observe changes to start_date and update end_date limits
  observeEvent(input$start_date, {
    updateDateInput(session, "end_date",
                    min = input$start_date,
                    max = input$start_date + 365)
  })
  
  # Observe changes to end_date and update start_date limits
  observeEvent(input$end_date, {
    updateDateInput(session, "start_date",
                    min = input$end_date - 365,
                    max = input$end_date)
  })
  
    # Reactive expression to read the uploaded file or the default file
  article_ids_df <- reactive({
    if (is.null(input$file1)) {
      # If no file is uploaded, read the default file
      file_path <- "ItemIDs.csv"  # Adjust the path if necessary
      read_csv(file_path, locale = locale(encoding = "UTF-8"))
    } else {
      # If a file is uploaded, read the uploaded file
      read_csv(input$file1$datapath, locale = locale(encoding = "UTF-8"))
    }
  })
  
  # Reactive expression to extract article IDs
  article_ids <- reactive({
    df <- article_ids_df()
    
    # Check if the dataframe has any columns
    if (ncol(df) == 0) {
      return(NULL)  # No columns available
    }
    
    # Identify the first column as the article IDs
    # You may need to adjust this if your article IDs could be in another column
    id_column <- names(df)[1]  # Assumes article IDs are in the first column
    
    # Extract the article IDs from the identified column
    df[[id_column]]
  })
  
  # Define the base URL for Figshare API
  base_url <- "https://stats.figshare.com/lboro/breakdown/total"
  
  # Define your username and password (replace with actual values)
  username <- Sys.getenv("FSusername")
  password <- Sys.getenv("FSpassword")
  
  # Combine the username and password and encode them
  credentials <- paste(username, password, sep = ":")
  encoded_credentials <- base64encode(charToRaw(credentials))
  counter <- 0
  
  # Function to get geolocation data for a single article ID
  get_geolocation <- function(article_id, metric, start_date, end_date) {
    url <- paste0(base_url, "/", metric, "/article/", article_id, "?start_date=", start_date, "&end_date=", end_date)
    response <- GET(url, add_headers(Authorization = paste("Basic", encoded_credentials)))
    if (status_code(response) == 200) {
      content <- content(response, as = "text", encoding = "UTF-8")
      json_data <- fromJSON(content, flatten = TRUE)
      return(json_data)
    } else {
      warning(paste("Failed to get data for article ID:", article_id))
      return(NULL)
    }
  }
  
  # Reactive expression to get aggregated data based on selected metric and dates
  aggregated_data_reactive <- reactive({
    metric <- input$metric  # Get selected metric
    start_date <- as.character(input$start_date)  # Get start date
    end_date <- as.character(input$end_date)  # Get end date
    
    aggregated_data <- list()
    
    # Loop through each article ID and get geolocation data
    for (article_id in article_ids()) {
      geolocation_data <- get_geolocation(article_id, metric, start_date, end_date)
      counter <- counter + 1
      print(counter)
      
      if (!is.null(geolocation_data)) {
        breakdown_total <- geolocation_data$breakdown$total
        
        # Loop through each country in the breakdown_total
        for (country in names(breakdown_total)) {
          if (is.list(breakdown_total[[country]])) {
            # Check if the country already exists in aggregated_data
            if (country %in% names(aggregated_data)) {
              # Add counts to existing entries
              for (location in names(breakdown_total[[country]])) {
                if (location != "total") {
                  aggregated_data[[country]][[location]] <- aggregated_data[[country]][[location]] + breakdown_total[[country]][[location]]
                }
              }
              # Update total count
              aggregated_data[[country]]$total <- aggregated_data[[country]]$total + breakdown_total[[country]]$total
            } else {
              # Initialize country entry if it doesn't exist
              aggregated_data[[country]] <- list()
              for (location in names(breakdown_total[[country]])) {
                if (location != "total") {
                  aggregated_data[[country]][[location]] <- breakdown_total[[country]][[location]]
                }
              }
              # Initialize total count
              aggregated_data[[country]]$total <- breakdown_total[[country]]$total
            }
          }
        }
      }
    }
    
    # Convert aggregated_data to a dataframe
    country_data <- map_df(names(aggregated_data), ~ {
      country <- .x
      downloads <- aggregated_data[[.x]]$total
      tibble(country = country, total_metric = downloads)
    })
    
    # Rename "United States" to "United States of America"
    country_data <- country_data %>%
      mutate(country = ifelse(country == "United States", "United States of America", country))
    
    country_data
  })
  
  # Render the plot
  output$heatMapPlot <- renderPlotly({
    country_data <- aggregated_data_reactive()
    
    world <- ne_countries(scale = "medium", returnclass = "sf")
    world_data <- world %>%
      left_join(country_data, by = c("name" = "country"))
    
    heat_map <- ggplot(world_data) +
      geom_sf(aes(fill = total_metric, text = paste0(name, ": ", total_metric)), color = "black", size = 0.01) +
      scale_fill_gradient(low = "#8D9C27", high = "#008466", na.value = "white") +
      theme_minimal() +
      theme(plot.title = element_text(size = 18, colour = "#008466", face = "bold")) +
      labs(title = paste("Mapped", input$metric), fill = element_blank())
    
    ggplotly(heat_map, tooltip = "text")  # This enables tooltips
  })
  
  
  # Download handler for the CSV file
  output$downloadData <- downloadHandler(
    filename = function() {
      metric <- input$metric
      start_date <- input$start_date
      end_date <- input$end_date
      paste(metric, "_data_", start_date, "_to_", end_date, ".csv", sep = "")
    },
    content = function(file) {
      country_data <- aggregated_data_reactive()
      
      # Rename the total_metric column based on the selected metric
      colnames(country_data)[which(names(country_data) == "total_metric")] <- input$metric
      
      # Order data in descending order based on the selected metric
      country_data <- country_data %>%
        arrange(desc(!!sym(input$metric)))
      
      write_csv(country_data, file)
    }
  )
  
  # Download handler for the JPEG map
  output$downloadMap <- downloadHandler(
    filename = function() {
      metric <- input$metric
      start_date <- input$start_date
      end_date <- input$end_date
      paste(metric, "_map_", start_date, "_to_", end_date, ".jpg", sep = "")
    },
    content = function(file) {
      country_data <- aggregated_data_reactive()
      
      # Load world map data using the rnaturalearth package
      world <- ne_countries(scale = "medium", returnclass = "sf")
      
      # Join the data with the world map
      world_data <- world %>%
        left_join(country_data, by = c("name" = "country"))
      
      # Generate the heatmap using ggplot2
      heat_map <- ggplot(world_data) +
        geom_sf(aes(fill = total_metric), color = "black") +
        scale_fill_gradient(low = "#8D9C27", high = "#008466", na.value = "white") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 8, colour = "#008466", face = "bold"),
          panel.background = element_rect(fill = "white", color = NA), # Explicitly set panel background
          plot.background = element_rect(fill = "white", color = NA)  # Explicitly set plot background
        ) +  # Set title font size to 18pts
        labs(title = paste("Mapped", input$metric), fill = element_blank())
      
      # Save the plot as a JPEG file
      ggsave(file, plot = heat_map, device = "jpeg")
    }
  )
  
  # Get csv server code ----

  # Create a reactiveVal to store fetched data
  search_results <- reactiveVal(NULL)
  
  # Trigger data fetching only when searchButton is clicked
  observeEvent(input$searchButton, {
    req(input$searchQuery, input$searchType)
    
    # Get user inputs
    query <- input$searchQuery
    search_type <- tolower(input$searchType)
    
    # Construct API endpoint
    endpoint <- switch(search_type,
                       "researcher" = "https://api.figshare.com/v2/account/institution/articles",
                       "collection" = paste0("https://api.figshare.com/v2/collections/", query, "/articles"),
                       "project" = paste0("https://api.figshare.com/v2/projects/", query, "/articles"),
                       "group" = paste0("https://api.figshare.com/v2/account/institution/articles?group=", URLencode(query)),
                       NULL)
    
    if (is.null(endpoint)) {
      warning("Invalid search type selected.")
      search_results(NULL)
      return()
    }
    
    headers <- add_headers(Authorization = paste("token", Sys.getenv("APIkey")))
    
    # Append params if researcher
    if (search_type == "researcher") {
      quoted_query <- paste0('"', query, '"')  # wrap in double quotes
      endpoint <- paste0(endpoint, "?search_for=", URLencode(quoted_query), "&limit=1000")
    }
    
    response <- GET(endpoint, headers)
    
    if (status_code(response) != 200) {
      warning(paste("API request failed with status", status_code(response)))
      search_results(NULL)
      return()
    }
    
    articles_data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    
    # Unwrap items if needed
    if (!is.null(articles_data$items)) {
      articles_data <- articles_data$items
    }
    
    article_ids <- articles_data$id
    
    combined_df <- data.frame(Citation = character(), URL = character(), ID = character(), stringsAsFactors = FALSE)
    
    for (article_id in article_ids) {
      full_url <- paste0("https://api.figshare.com/v2/articles/", article_id)
      citation_response <- GET(full_url, headers)
      
      if (status_code(citation_response) == 200) {
        citation_data <- fromJSON(content(citation_response, "text", encoding = "UTF-8"), flatten = TRUE)
        citation_df <- data.frame(
          Citation = citation_data$citation,
          URL = citation_data$figshare_url,
          ID = as.character(article_id),
          stringsAsFactors = FALSE
        )
        combined_df <- bind_rows(combined_df, citation_df)
      } else {
        warning(paste("Failed to get citation for article ID:", article_id))
      }
    }
    
    # Save results
    search_results(combined_df)
  })
  
  
  # Display search results in the main panel
  output$searchResults <- DT::renderDataTable({
    fetched_df <- search_results()  # Fetch data using the reactive function
    if (!is.null(fetched_df) && nrow(fetched_df) > 0) {  # Check if fetched_df is not NULL and has rows
      # Create a data frame with clickable citations
      df <- data.frame(Citation = paste0('<a href="', fetched_df$URL, '" target="_blank">', fetched_df$Citation, '</a>'), stringsAsFactors = FALSE)
      
      # Render the DataTable
      datatable(df, escape = FALSE, options = list(
        dom = 't',  # Only display the table without any other controls
        paging = FALSE,  # Disable paging
        searching = FALSE,  # Disable searching
        columnDefs = list(list(
          targets = 0,
          render = JS(
            'function(data, type, row, meta) {',
            'return type === "display" ? data : data;',  # Prevent escaping HTML
            '}'
          )
        ))
      ))
    } else {
      # Optionally return a message or empty DataTable if no data is available
      datatable(data.frame(Citation = "No results found"), escape = FALSE, options = list(dom = 't', paging = FALSE, searching = FALSE))
    }
  })
  
  # Allow user to download results as a CSV
  output$downloadSearch <- downloadHandler(
    filename = function() {
      paste0("search_results-", input$firstname, "-", input$surname, "-", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Get only the article IDs to write to the CSV
      details <- search_results()
      
      # Create a data frame with a single column for article IDs
      ids_df <- data.frame(Article_IDs = details$ID, stringsAsFactors = FALSE)
      
      # Write only the article IDs to the CSV
      write.csv(ids_df, file, row.names = FALSE)  # Write single column of IDs
    }
  )
  
})
  
