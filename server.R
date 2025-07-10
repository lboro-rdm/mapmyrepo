library(shiny)
library(httr2)
library(jsonlite)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(openssl)
library(base64enc)
library(DT)
library(tidyverse)
library(plotly)
library(rsconnect)

shinyServer(function(input, output, session){
  
  options(shiny.sanitize.errors = FALSE)
  
  search_triggered <- reactiveVal(FALSE)
  
  # Map server code ----
  
    # Reactive expression to read the uploaded file or the default file
  article_ids_df <- reactive({
    if (is.null(input$file1)) {
      # If no file is uploaded, read the default file
      file_path <- "www/ItemIDs.csv"
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
    
    id_column <- names(df)[1] 
    df[[id_column]]
  })
  
  # Define the base URL for Figshare API
  base_url <- "https://stats.figshare.com/lboro/breakdown/total"
  
  # Define your username and password (replace with actual values)
  username <- Sys.getenv("FSusername")
  password <- Sys.getenv("FSpassword")
  print(paste("Username found:", nzchar(Sys.getenv("FSusername"))))
  print(paste("Password found:", nzchar(Sys.getenv("FSpassword"))))
  cat("Username length:", nchar(username), "\n")
  cat("Password length:", nchar(password), "\n")
  
  # req <- request(base_url) %>%
    # req_auth_basic()req_auth_basic(username, password)
  
  # Function to get geolocation data for a single article ID
  get_geolocation <- function(article_id, metric, start_date, end_date) {
    # Construct the full URL
    url <- paste0(base_url, "/", metric, "/article/", article_id,
                  "?start_date=", start_date, "&end_date=", end_date)
    print(url)
    
    # Build and perform the request
    resp <- request(url) %>%
      req_auth_basic(username, password) %>%
      req_headers(`User-Agent` = "httr2 - shinyApp/1.0") %>%
      req_perform()
    
    # Handle response
    if (resp_status(resp) == 200) {
      json_data <- resp_body_json(resp, simplifyVector = TRUE)
      return(json_data)
    } else {
      warning(paste("Failed to get data for article ID:", article_id))
      return(NULL)
    }
  }
  
  
  aggregated_data_reactive <- reactive({
    metric <- input$metric
    start_date <- as.Date(input$start_date)
    end_date <- as.Date(input$end_date)

    # Create start and end dates for each year chunk
    start_dates <- seq.Date(start_date, end_date, by = "year")
    end_dates <- pmin(start_dates + 365, end_date)
    
    # Initialize empty dataframe
    all_data <- tibble(country = character(), total = numeric())
    
    ids <- article_ids()

    # Loop through each article ID
    for (article_id in ids) {
      # For each year chunk
      for (i in seq_along(start_dates)) {
        chunk_start <- start_dates[i]
        chunk_end <- end_dates[i]
        
        geolocation_data <- get_geolocation(article_id, metric, chunk_start, chunk_end)
        if (!is.null(geolocation_data)) {
          breakdown_total <- geolocation_data$breakdown$total
          
          # Flatten the list into a tibble
          country_chunk <- map_dfr(names(breakdown_total), function(country) {
            if (is.list(breakdown_total[[country]]) && !is.null(breakdown_total[[country]]$total)) {
              tibble(country = country, total = breakdown_total[[country]]$total)
            } else {
              NULL
            }
          })
          
          # Combine into full dataset
          all_data <- bind_rows(all_data, country_chunk)
        }
      }
    }
    
    # Summarize total metrics by country
    country_data <- all_data %>%
      group_by(country) %>%
      summarize(total_metric = sum(total), .groups = "drop") %>%
      mutate(country = ifelse(country == "United States", "United States of America", country))
    
    country_data
  })
  
  
  output$heatMapPlot <- renderPlotly({
    country_data <- aggregated_data_reactive()
    country_data$country <- as.character(country_data$country)

    if (is.null(country_data) || nrow(country_data) == 0) {
      return(NULL)
    }
    
    world <- ne_countries(scale = "medium", returnclass = "sf")

    world_data <- world %>%
      mutate(name = as.character(name)) %>%
      left_join(country_data, by = c("name" = "country"))

    
    # If `total_metric` is missing or all NA, abort with a message
    if (!"total_metric" %in% names(world_data)) {
      return(NULL)
    }
    
    heat_map <- ggplot(world_data) +
      geom_sf(aes(fill = total_metric, text = paste0(name, ": ", total_metric)), color = "black", size = 0.01) +
      scale_fill_gradient(low = input$colour2, high = input$colour1, na.value = "white") +
      theme_minimal() +
      theme(plot.title = element_text(size = 18, colour = input$colour1, face = "bold")) +
      labs(title = input$mapTitle, fill = element_blank())
    
    plotly_map <- ggplotly(heat_map, tooltip = "text")

        plotly_map
  })
  
  
  
  
  # Download handler for the CSV file
  output$downloadData <- downloadHandler(
    filename = function() {
      file_title <- input$mapTitle
      start_date <- input$start_date
      end_date <- input$end_date
      paste(file_title, "-data-", start_date, "_to_", end_date, ".csv", sep = "")
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
      file_title <- input$mapTitle
      start_date <- input$start_date
      end_date <- input$end_date
      paste(file_title, "-map-", start_date, "_to_", end_date, ".jpeg", sep = "")
    },
    content = function(file) {
      country_data <- aggregated_data_reactive()
      
      # Load world map data
      world <- ne_countries(scale = "medium", returnclass = "sf")
      
      # Join with country data
      world_data <- world %>%
        left_join(country_data, by = c("name" = "country"))
      
      # Build the heat map with user input for title and colour scale
      heat_map <- ggplot(world_data) +
        geom_sf(
          aes(fill = total_metric),
          color = "black"
        ) +
        scale_fill_gradient(
          low = input$colour2,
          high = input$colour1,
          na.value = "white"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, colour = input$colour1, face = "bold"),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        ) +
        labs(title = input$mapTitle, fill = element_blank())
      
      # Save the map
      ggsave(file, plot = heat_map, device = "jpeg", width = 10, height = 6, units = "in", dpi = 300)
    }
  )
  
  # Get csv server code ----

  # Create a reactiveVal to store fetched data
  search_results <- reactiveVal(NULL)
  
  # Trigger data fetching only when searchButton is clicked
  observeEvent(input$searchButton, {
    
    req(input$searchQuery, input$searchType)
    
    search_triggered(TRUE)
    search_results(NULL)
    
    # Get user inputs
    query <- input$searchQuery
    search_type <- tolower(input$searchType)
    
    # Construct API endpoint
    endpoint <- switch(
      search_type,
      "researcher" = "https://api.figshare.com/v2/account/institution/articles",
      "collection" = paste0("https://api.figshare.com/v2/collections/", query, "/articles?limit=1000"),
      "project" = paste0("https://api.figshare.com/v2/projects/", query, "/articles?limit=1000"),
      "group" = paste0("https://api.figshare.com/v2/account/institution/articles?group_id=", URLencode(query), "&limit=1000"),
      NULL
    )
    
    if (is.null(endpoint)) {
      warning("Invalid search type selected.")
      search_results(NULL)
      return()
    }
    
    # Make the API request using httr2
    req <- request(endpoint) %>%
      req_headers(
        Authorization = paste("token", Sys.getenv("APIkey")),
        `User-Agent` = "httr2 - shinyApp/1.0"
      )
    
    resp <- tryCatch(
      req_perform(req),
      error = function(e) {
        warning("Search request failed: ", e$message)
        return(NULL)
      }
    )
    
    if (!is.null(resp) && resp_status(resp) == 200) {
      data <- resp_body_json(resp, simplifyVector = TRUE)
      search_results(data)
    } else {
      warning("No data returned from search.")
      search_results(NULL)
    }
  # })


   # Append params if researcher
   if (search_type == "researcher") {
     quoted_query <- paste0('"', query, '"')  # wrap in double quotes
     endpoint <- paste0(endpoint, "?search_for=", URLencode(quoted_query), "&limit=1000")
   }


  # Build and perform the request using httr2
  req <- request(endpoint) %>%
    req_headers(Authorization = paste("token", Sys.getenv("APIkey")))

  resp <- tryCatch(
    req_perform(req),
    error = function(e) {
      warning(paste("API request failed:", e$message))
      return(NULL)
    }
  )

  if (is.null(resp) || resp_status(resp) != 200) {
    warning(paste("API request failed with status", if (!is.null(resp)) resp_status(resp) else "NULL"))
    search_results(NULL)
    return()
  }

  # Parse JSON response
  articles_data <- resp_body_json(resp, simplifyVector = TRUE)

  # Unwrap items if present
  if (!is.null(articles_data$items)) {
    articles_data <- articles_data$items
  }

  # Extract article IDs
  article_ids <- articles_data$id

  # Prepare empty result dataframe
  combined_df <- data.frame(
    Citation = character(),
    URL = character(),
    ID = character(),
    stringsAsFactors = FALSE
  )


  for (article_id in article_ids) {
    full_url <- paste0("https://api.figshare.com/v2/articles/", article_id)

    req <- request(full_url) %>%
      req_headers(Authorization = paste("token", Sys.getenv("APIkey")))

    resp <- tryCatch(
      req_perform(req),
      error = function(e) {
        warning(paste("Request failed for article ID", article_id, ":", e$message))
        return(NULL)
      }
    )

    if (!is.null(resp) && resp_status(resp) == 200) {
      citation_data <- resp_body_json(resp, simplifyVector = TRUE)
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
    req(search_triggered())
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