# Parse the HTML
html_parsed <- read_html("https://www.cars45.com/listing")

# Extract information from each car listing
car_listings <- html_parsed %>%
  html_elements(".car-feature--wide-mobile") %>%
  map_df(~{
    # Extract the price, name, region, usage type, and mileage
    price <- .x %>%
      html_element(".car-feature__amount") %>%
      html_text(trim = TRUE)
    
    name <- .x %>%
      html_element(".car-feature__name") %>%
      html_text(trim = TRUE)
    
    region <- .x %>%
      html_element(".car-feature__region") %>%
      html_text(trim = TRUE)
    
    others <- .x %>%
      html_elements(".car-feature__others__item") %>%
      html_text(trim = TRUE)
    
    # Create a data frame row
    data.frame(
      price = price,
      name = name,
      region = region,
      usage_type = others[1],
      mileage = others[2],
      stringsAsFactors = FALSE
    )
  })

# Print the resulting data frame
print(car_listings)
