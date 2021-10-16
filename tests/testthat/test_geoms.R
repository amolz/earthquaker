library(testthat)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(grid)


context("Testing that the geoms run and return the correct objects")

#-----------------------------------------------------------------------------

test_that("geom_timeline runs correctly", {

  Plot1 <- raw_data %>%
    eq_clean_data() %>%
    select(c("Country", "Location_Name", "Date", "Mag", "Latitude", "Longitude", "Deaths")) %>%
    filter(Country %in% c("India", "China", "Mexico")) %>%
    filter(Date>="2000-01-01" & Date<"2010-12-31") %>%
    ggplot(aes(x=Date, y=Country, color = Deaths, size = Mag, label=Location_Name)) +
    geom_timeline()

    expect_is(Plot1, "gg")
    expect_is(Plot1, "ggplot")

})


test_that("geom_timeline_label runs correctly", {

  Plot2 <- raw_data %>%
    eq_clean_data() %>%
    select(c("Country", "Location_Name", "Date", "Mag", "Latitude", "Longitude", "Deaths")) %>%
    filter(Country %in% c("India", "China", "Mexico")) %>%
    filter(Date>="2000-01-01" & Date<"2010-12-31") %>%
    ggplot(aes(x=Date, y=Country, color = Deaths, size = Mag, label=Location_Name)) +
    geom_timeline() +
    geom_timeline_label(n_max=5) +
    scale_size_continuous(name = "Richter scale value", breaks = c(2,4,6)) +
    scale_color_continuous(name = "# Deaths") +
    theme_bw() +
    theme(legend.position = "right")+
    labs(title = "Earthquakes Summary")

  expect_is(Plot2, "gg")
  expect_is(Plot2, "ggplot")

})






test_that("geom_timeline runs correctly", {
  df <- load_data() %>% eq_clean_data() %>% dplyr::filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
  plt <-   ggplot2::ggplot(df, ggplot2::aes(x = date, y = COUNTRY,
                                            color = as.numeric(TOTAL_DEATHS),
                                            size = as.numeric(EQ_PRIMARY),
                                            label = CLEAN_LOCATION_NAME)) +
    geom_timeline() +
    ggplot2::labs(size = "Richter scale value", color = "# deaths") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.title.y = ggplot2::element_blank()) + ggplot2::xlab("DATE")

  expect_is(plt, "gg")
  expect_is(plt, "ggplot")
})

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
test_that("geom_timeline_label runs correctly", {
  df <- load_data() %>% eq_clean_data() %>% dplyr::filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
  plt <-   ggplot2::ggplot(df, ggplot2::aes(x = date, y = COUNTRY,
                                            color = as.numeric(TOTAL_DEATHS),
                                            size = as.numeric(EQ_PRIMARY),
                                            label = CLEAN_LOCATION_NAME)) +
    geom_timeline() +
    ggplot2::labs(size = "Richter scale value", color = "# deaths") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.title.y = ggplot2::element_blank()) + ggplot2::xlab("DATE") +
    geom_timeline_label(data=df)

  expect_is(plt, "gg")
  expect_is(plt, "ggplot")
})
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
