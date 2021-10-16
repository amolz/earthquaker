library(testthat)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

context("Testing that the clean_data function runs correctly")

# For testing, the dataset was connected from local system
# raw_data <- readr::read_delim("D:/DataAnalysis/R/Coursera/5_Capstone/earthquakes_2021.tsv", delim = "\t")

test_that("Data gets cleaned correctly", {
  df <- raw_data %>% eq_clean_data()

  expect_is(df$Date, "Date")         # the date column will be of the class date
  expect_is(df$Latitude, "numeric")  # that each of LATITUDE and LONGITUDE are numeric
  expect_is(df$Longitude, "numeric")
})
