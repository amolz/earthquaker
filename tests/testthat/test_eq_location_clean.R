library(testthat)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# For testing, the dataset was connected from local system
# raw_data <- readr::read_delim("D:/DataAnalysis/R/Coursera/5_Capstone/earthquakes_2021.tsv", delim = "\t")


context("Testing that the eq_location_clean function runs correctly")

test_that("Location gets cleaned correctly", {
  df <- raw_data %>% eq_location_clean
  expect_true(all(df$Location_Name[2] == c("Bab-A-Daraa,Al-Karak")))

})
