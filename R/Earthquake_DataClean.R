# library(readr)
# library(dplyr)
# library(tidyr)
# library(lubridate)
# library(stringr)

#' @title Clean location names of NOAA earthquake dataset
#'
#' @description The NOAA dataset location name has a string containing Country and Location names.
#' \code{eq_location_clean} strips and separates the Country and location names into two fields and
#' also convert them into title case.
#'
#' @param "dataframe" is the NOAA dataframe that has the location name to be cleaned
#'
#' @importFrom dplyr mutate
#' @importFrom tidyr separate
#' @importFrom stringr str_trim, str_to_title
#'
#' @return Returns a dataframe with cleaned location names
#'
#' @export
#'
#' @examples
#' eq_location_clean(df)
#'
#' \dontrun{
#' eq_cleaned <- eq_rawdata %>% eq_location_clean()
#' }

eq_location_clean <- function(dataframe){

  d1 <- dataframe
  d1 <- d1 %>%
    tidyr::separate('Location Name',c("Country", "Location_Name"),": ") %>%
    dplyr::mutate(Country = stringr::str_trim(str_to_title(Country, locale = "en"))) %>%
    dplyr::mutate(Location_Name = stringr::str_trim(str_to_title(Location_Name, locale = "en")))
  return(d1)
}


#' @title Create Date field using Year, Mo and Dy fields in the raw dataset
#'
#' @description NOAA dataset has Year, month and day is separate fields.
#' \code{eq_clean_data} combines and create a new date field. It also calls the
#' \code{eq_location_clean} function that creates a cleaned location name. It also converts the
#' latitude and logitude fields into numeric.
#'
#' @param "dataframe" is the NOAA dataframe that has the Year, month and day fields to be clubbed and lat/long
#'
#' @importFrom dplyr mutate, arrange
#' @importFrom lubridate make_date
#'
#' @return Returns a dataframe with Date field and cleaned location names
#'
#' @export
#'
#' @examples
#' eq_clean_data(df)
#'
#' \dontun{
#' eq_final = eq_rawdata %>% #' eq_clean_data()
#' }

eq_clean_data <- function(dataframe){

    d2 <- dataframe
    d2 <- d2 %>%
      dplyr::mutate(Latitude= as.numeric(Latitude)) %>%
      dplyr::mutate(Longitude= as.numeric(Longitude)) %>%
      dplyr::mutate(Date = lubridate::make_date(Year, Mo, Dy)) %>%
      dplyr::arrange(desc(Mag)) %>%
      eq_location_clean()
    return(d2)
}
