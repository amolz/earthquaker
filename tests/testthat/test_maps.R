library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(grid)
library(leaflet)

context("Testing the creation of our leaflet maps and related functions")

test_that("eq_map runs without error", {

  Map1 <- raw_data %>%
    eq_clean_data() %>%
    select(c("Country", "Location_Name", "Date", "Mag", "Latitude", "Longitude", "Deaths")) %>%
    filter(Country %in% c("India", "China", "Mexico")) %>%
    filter(Date>="2000-01-01" & Date<"2010-12-31") %>%
    mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "popup_text")

})

test_that("eq_create_label runs without error", {
  locations <- c("Loc1", "Loc2")
  magnitude <- c(3.2, 7.5)
  deaths <- c(0, 0)
  df = data.frame(Location_Name=locations,
                  Mag=magnitude, Deaths=deaths)
  lbls <- eq_create_label(df)
  expect_equal(lbls[1], "<b>Location: </b>Loc1</br><b>Magnitude: </b>3.2</br><b>Total Deaths: </b>0")
  expect_equal(lbls[2], "<b>Location: </b>Loc2</br><b>Magnitude: </b>7.5</br><b>Total Deaths: </b>0")
})
