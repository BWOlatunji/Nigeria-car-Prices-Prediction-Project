library(tidyverse)  # data wrangling
library(RSelenium)  # activate Selenium server
library(rvest)      # web scrape tables
library(netstat)    # find unused port
library(data.table) # for the rbindlist function

# Start RSelenium
rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "131.0.6778.204",
                             verbose = F,
                             port = free_port())

remDr <- rs_driver_object$client
remDr$navigate("https://www.cars45.com/listing")

# Define a named function to extract car details, handling missing elements with tryCatch
extract_car_details <- function(car) {
  # Extract car price
  price <- tryCatch({
    car %>%
      html_element(".car-feature__amount") %>%
      html_text(trim = TRUE)
  }, error = function(e) NA)
  
  # Extract car name
  name <- tryCatch({
    car %>%
      html_element(".car-feature__name") %>%
      html_text(trim = TRUE)
  }, error = function(e) NA)
  
  # Extract car region
  region <- tryCatch({
    car %>%
      html_element(".car-feature__region") %>%
      html_text(trim = TRUE)
  }, error = function(e) NA)
  
  # Extract car condition (e.g., Local Used, Foreign Used)
  condition <- tryCatch({
    car %>%
      html_elements(".car-feature__others__item") %>%
      .[1] %>%
      html_text(trim = TRUE)
  }, error = function(e) NA)
  
  # Extract car distance (e.g., 239315 km)
  distance <- tryCatch({
    car %>%
      html_elements(".car-feature__others__item") %>%
      .[2] %>%
      html_text(trim = TRUE)
  }, error = function(e) NA)
  
  # Return as a list
  list(price = price, name = name, region = region, condition = condition, distance = distance)
}

# Initialize empty list to hold all data
all_data <- list()

# Variable to control the loop
cond <- TRUE
total_results <- 0  # Variable to store total number of results

while (cond == TRUE) {
  # Parse the HTML of the current page
  html_parsed <- remDr$getPageSource()[[1]] %>%
    read_html()
  
  # Extract total number of results from the page
  total_results_text <- html_parsed %>%
    html_element("p:contains('results')") %>%
    html_text(trim = TRUE)
  
  # Extract the total number of results (e.g., "2078" from "2071 - 2078 of 2 078 results")
  total_results <- tryCatch({
    str_extract(total_results_text, "\\d+$") %>%
      as.numeric()
  }, error = function(e) {
    print("Failed to extract total results; skipping this check.")
    NA
  })
  
  # If total_results is NA, skip the row count check
  # if (is.na(total_results)) {
  #   print("Total results not available, continuing scraping without stopping condition.")
  # }
  
  # Extract the details for multiple car listings using purrr::map and the named function
  car_listings_ls <- html_parsed %>%
    html_elements(".car-feature") %>%
    map(extract_car_details)
  
  # Convert the list into a data frame
  car_listings_df <- map_dfr(car_listings_ls, as.data.frame)
  
  # Append new data to all_data
  all_data <- rbindlist(list(all_data, car_listings_df), use.names = TRUE, fill = TRUE)
  
  # If total_results is not NA, check if the total number of rows collected matches or exceeds the total results
  if (!is.na(total_results) && nrow(all_data) >= total_results) {
    cond <- FALSE
    print("All results collected!")
    break
  }
  
  # Pause before clicking to avoid overwhelming the server
  Sys.sleep(0.2)
  
  tryCatch(
    {
      # Find the "Next" button and click to go to the next page
      next_button <- remDr$findElement(using = 'xpath', '//a[@aria-label="Next"]')
      # Scroll to the "Next" button using JavaScript
      remDr$executeScript("arguments[0].scrollIntoView(true);", list(next_button))
      next_button$clickElement()
    },
    error=function(e) {
      print("Script Complete - No more pages to navigate!")
      cond <<- FALSE
    }
  )
  
  if (cond == FALSE) {
    break
  }
}

cars_cleaned <- all_data %>% 
  mutate(price= price %>% 
           str_replace_all("[\\D]", "") %>% 
           as.numeric(),
         year_of_manufacture = name %>% str_extract("\\d+"),
         name = name %>% str_remove("\\d+"),
         color = name %>% str_extract("\\s[A-Za-z]+$"),
         mileage = distance %>% str_extract("\\d+")) %>% 
  separate(region, into = c("registered_state", "registered_city"), 
           sep = ", ", fill = "right", remove = FALSE) %>% 
  select(-c(distance, region))


write_rds(all_data, "data/cars_raw_data.rds")
write_csv(all_data, "data/cars_raw_data.csv")
write_rds(cars_cleaned, "data/cars_cleaned_data.rds")
write_csv(cars_cleaned, "data/cars_cleaned_data.csv")

