library(httr2)
library(jsonlite)
library(tidyverse)

username <- Sys.getenv("FSusername")
password <- Sys.getenv("FSpassword")

auth_header <- paste("token", Sys.getenv("APIkey"))

searchQuery <- "gareth cole"

query <- searchQuery

quoted_query <- paste0('"', query, '"')

# Initialize request
req <- NULL

# Build request
req <- request("https://api.figshare.com/v2/articles/search") |>
  req_method("POST") |>
  req_headers(`Content-Type` = "application/json") |>
  req_body_json(list(
    search_for = quoted_query,
    page_size = 1000
  ))

# Perform request
resp <- req_perform(req)

# Check status
resp_status(resp)

# Parse response JSON into R object
search_results <- resp_body_json(resp, simplifyVector = TRUE)

article_ids <- search_results$id


is_true_author <- function(article_id, target_name = NULL, target_author_id = NULL) {
  
  art <- request(paste0("https://api.figshare.com/v2/articles/", article_id)) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  
  authors <- art$authors
  
  if (!is.null(target_author_id)) {
    return(any(authors$id == target_author_id))
  }
  
  if (!is.null(target_name)) {
    return(any(str_to_lower(authors$full_name) == str_to_lower(target_name)))
  }
  
  FALSE
}

search_results <- search_results |>
  mutate(
    verified_author = map_lgl(id, is_true_author, target_name = "Lara Skelly")
  )

true_articles <- filter(search_results, verified_author)

