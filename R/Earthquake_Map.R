# library(readr)
# library(dplyr)
# library(tidyr)
# library(lubridate)
# library(stringr)
# library(ggplot2)
# library(grid)
# library(leaflet)


#' @title Create an earthquake label field for map annotation
#'
#' @description This function takes a dataframe (df) and returns a character vector
#' of HTML column names. Note that the dataframe must have columns
#' Location_Name, Mag (indicating magnitude) and Deaths.
#'
#' @param df The dataframe used to create the annotation column.
#'
#' @return Returns a character vector of earthquake details that are used for
#' map annotations.
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  Map1 <- eq_rawdata %>%
#'  eq_clean_data() %>%
#'  select(c("Country", "Location_Name", "Date", "Mag", "Latitude", "Longitude", "Deaths")) %>%
#'  filter(Country %in% c("India", "China", "Mexico")) %>%
#'  filter(Date>="2000-01-01" & Date<"2010-12-31") %>%
#'  mutate(popup_text = eq_create_label(.))
#'  Map1
#' }

eq_create_label<-function(df){

  # check if the correct columns are present
  all_columns <- colnames(df)

  stopifnot(any("Location_Name" %in% all_columns),
            any("Mag" %in% all_columns),
            any("Deaths" %in% all_columns))

  len <- length(df$Location_Name)
  locations <- df$Location_Name
  magnitude <- df$Mag             # Magnitude is under "Mag field name in the dataset
  deaths <- df$Deaths

  popup_text <- rep("", len)

  for(i in 1:len){

    txt <- paste0("<b>Location: </b>", locations[i], "</br>",
                "<b>Magnitude: </b>", magnitude[i], "</br>",
                "<b>Total Deaths: </b>", deaths[i])
    popup_text[i] <- txt
  }
  return(popup_text)
}

#' @title Plot a leaflet map with annotations
#'
#' @description We plot the earthquake data on map using leaflet and for a subsetted data.
#' See the leaflet docs (?leaflet) for relevant examples.
#' The input dataframe requires columns including Longitude, Latitude and Magnitude.
#' The annotation column ('annot_col') is also a required argument.
#'
#' @param df The dataframe of data that user wants to plot
#' @param annot_col The field from input dataframe to be used for annotation
#'
#' @return Returns an interactive leaflet map
#'
#' @import leaflet, dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  Map1 <- eq_rawdata %>%
#'  eq_clean_data() %>%
#'  select(c("Country", "Location_Name", "Date", "Mag", "Latitude", "Longitude", "Deaths")) %>%
#'  filter(Country %in% c("India", "China", "Mexico")) %>%
#'  filter(Date>="2000-01-01" & Date<"2010-12-31") %>%
#'  mutate(popup_text = eq_create_label(.)) %>%
#'  eq_map(annot_col = "popup_text")
#'  Map1
#' }

eq_map <- function(eq_data_clean = NULL, annot_col = "Date"){

  # check if the correct columns are present
  all_columns <- colnames(eq_data_clean)

  stopifnot(any("Date" %in% all_columns),
            any("Latitude" %in% all_columns),
            any("Longitude" %in% all_columns),
            any("Mag" %in% all_columns))

  # check to see if invalid column are provided - print message and default to DATE
  if(!(any(annot_col %in% all_columns))) {
    warning("Invalid Column - DATE Displayed")
    annot_col = "Date"
  }

  # create a leaflet map
  Map1 <- leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = eq_data_clean,
                              lng = ~ Longitude,
                              lat = ~ Latitude,
                              radius = ~ Mag,
                              weight = 1,
                              fillOpacity = 0.2,
                              popup = ~ paste0(get(annot_col)))

  return(Map1)
}
