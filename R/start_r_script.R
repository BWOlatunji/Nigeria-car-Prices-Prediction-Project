
library(rvest)
library(RSelenium)
library(netstat)
library(tidyverse)
library(tidymodels)
library(janitor)

rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "131.0.6778.204",
                             verbose = F,
                             port = free_port())

remDr <- rs_driver_object$client
remDr$navigate("https://www.cars45.com/listing")

car_section <- remDr$findElement(using = 'class name', 'cars-grid')


next_button <- remDr$findElement(using = 'xpath', '//a[@aria-label="Next"]')
next_button$clickElement()


car_page <- read_html("https://www.cars45.com/listing")

car_feature_amounts <- car_page |>
  html_element('.cars-grid') %>% 
  html_elements("a") %>% 
  html_elements(".car-feature__details") |>
  html_elements(".car-feature__amount") |>
  html_text()

car_feature_name <- car_page |>
  html_element('.cars-grid') %>% 
  html_elements("a") %>% 
  html_elements(".car-feature__details") |>
  html_elements(".car-feature__name") |>
  html_text()

car_feature_region <- car_page |>
  html_element('.cars-grid') %>% 
  html_elements("a") %>% 
  html_elements(".car-feature__details") |>
  html_elements(".car-feature__region") |>
  html_text()

car_feature_others <- car_page |>
  html_element('.cars-grid') %>% 
  html_elements("a") %>% 
  html_elements(".car-feature__details") |>
  html_elements(".car-feature__others") |>
  html_elements(".car-feature__others__item") %>% 
  html_text()

car_feature_region <- car_page |>
  html_element('.cars-grid') %>% 
  html_elements("a") %>% 
  html_elements(".car-feature__details") |>
  html_elements(".car-feature__others") |>
  html_elements(".car-feature__others__item") %>% .[[1]] %>% 
  html_text()


span_values <- car_page |>
  html_element('.cars-grid') %>% 
  html_elements("a") %>% 
  html_elements(".car-feature__details") |>
  html_elements(".car-feature__others") |>
  html_elements(".car-feature__others__item") %>%
  html_text(trim = TRUE)

car_features_df <- data.frame(
  feature = span_values[1],
  distance = span_values[2],
  stringsAsFactors = FALSE
)
