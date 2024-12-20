# Extract the details for multiple car listings
car_listings_list <- html_parsed %>%
  html_elements(".car-feature") %>%  # Select each car feature element
  lapply(function(car) {
    # Extract car price
    price <- car %>%
      html_element(".car-feature__amount") %>%
      html_text(trim = TRUE)
    
    # Extract car name
    name <- car %>%
      html_element(".car-feature__name") %>%
      html_text(trim = TRUE)
    
    # Extract car region
    region <- car %>%
      html_element(".car-feature__region") %>%
      html_text(trim = TRUE)
    
    # Extract car condition (e.g., Local Used, Foreign Used)
    condition <- car %>%
      html_elements(".car-feature__others__item") %>%
      .[1] %>%
      html_text(trim = TRUE)
    
    # Extract car distance (e.g., 239315 km)
    distance <- car %>%
      html_elements(".car-feature__others__item") %>%
      .[2] %>%
      html_text(trim = TRUE)
    
    # Return as a list
    list(price = price, name = name, region = region, condition = condition, distance = distance)
  })

# Convert the list into a data frame
car_listings_df <- do.call(rbind, lapply(car_listings, as.data.frame))

# Print the data frame
print(car_listings_df)


# Modified to have named function and using map() for iteration
# Parse the HTML
html_parsed <- read_html("https://www.cars45.com/listing")

# Define a named function to extract car details
extract_car_details <- function(car) {
  # Extract car price
  price <- car %>%
    html_element(".car-feature__amount") %>%
    html_text(trim = TRUE)
  
  # Extract car name
  name <- car %>%
    html_element(".car-feature__name") %>%
    html_text(trim = TRUE)
  
  # Extract car region
  region <- car %>%
    html_element(".car-feature__region") %>%
    html_text(trim = TRUE)
  
  # Extract car condition (e.g., Local Used, Foreign Used)
  condition <- car %>%
    html_elements(".car-feature__others__item") %>%
    .[1] %>%
    html_text(trim = TRUE)
  
  # Extract car distance (e.g., 239315 km)
  distance <- car %>%
    html_elements(".car-feature__others__item") %>%
    .[2] %>%
    html_text(trim = TRUE)
  
  # Return as a list
  list(price = price, name = name, region = region, condition = condition, distance = distance)
}

# Extract the details for multiple car listings using purrr::map and the named function
car_listings_ls <- html_parsed %>%
  html_elements(".car-feature") %>%
  map(extract_car_details)

# Convert the list into a data frame
car_listings_df <- map_dfr(car_listings_ls, as.data.frame)

# Print the data frame
print(car_listings_df)

