library(httr2)
library(readr)
library(dplyr)
library(purrr)

# Load credentials
FSusername <- Sys.getenv("FSusername")
FSpassword <- Sys.getenv("FSpassword")

# CSV with article IDs
data <- read_csv("search_results---2025-09-19.csv")

# Base URL
base_url <- "https://stats.figshare.com/lboro/total/downloads/article/"

# Full URLs
data <- data %>%
  mutate(full_url = paste0(base_url, Article_IDs))

# Function to get totals using httr2
get_totals_httr2 <- function(url) {
  tryCatch({
    resp <- request(url) %>%
      req_auth_basic(FSusername, FSpassword) %>%
      req_headers(`User-Agent` = "httr2 - shinyApp/1.0") %>%
      req_perform()
    
    if (resp_status(resp) == 200) {
      json_data <- resp_body_json(resp, simplifyVector = TRUE)
      return(json_data$totals)
    } else {
      warning(paste("Failed request for URL:", url, "Status:", resp_status(resp)))
      return(NA)
    }
    
  }, error = function(e) {
    message("Error with URL ", url, ": ", e$message)
    return(NA)
  })
}

# Apply to all URLs
data <- data %>%
  mutate(totals = map_dbl(full_url, get_totals_httr2))

# View results
print(data)
# Find the row with the highest total
top_article <- data %>%
  filter(totals == max(totals, na.rm = TRUE))

# View the result
print(top_article)
