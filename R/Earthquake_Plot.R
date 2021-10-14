# library(readr)
# library(dplyr)
# library(tidyr)
# library(lubridate)
# library(stringr)
# library(ggplot2)
# library(grid)

#' @title Defining the GeomTimeline class
#'
#' @description The ggproto() function is used to construct a new class.
#' The geom_* function is built as a regular function.
#' The built geom named geom_timeline() plots the earthquake intensity on a timeline
#' based on input data range. Optional aesthetics include color, size, and alpha.
#' The x-aesthetic (required) is a date and an optional y aesthetic is a factor indicating
#' some stratification in which case multiple time lines will be plotted for
#' each level of the factor (e.g. country). The points indicate event in time, point size indicate
#' the earthquake magnitude and colour represent the number of deaths.
#'
#' @import ggplot2, grid, scales
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  Plot1 <- eq_rawdata %>%
#'  eq_clean_data() %>%
#'  select(c("Country", "Location_Name", "Date", "Mag", "Latitude", "Longitude", "Deaths")) %>%
#'  filter(Country %in% c("India", "China", "Mexico")) %>%
#'  filter(Date>="2000-01-01" & Date<"2010-12-31") %>%
#'  ggplot(aes(x=Date, y=Country, color = Deaths, size = Mag, label=Location_Name)) +
#'  geom_timeline()
#'  PLot1
#'  }

GeomTimeline <- ggplot2::ggproto("GeomTimeline", Geom,
                               required_aes = c("x"), # optional y aesthetic
                               default_aes = ggplot2::aes(y = 1,
                                                        alpha = 0.5,
                                                        fill = "grey",
                                                        colour = "grey",
                                                        size = 1.5,
                                                        shape = 21,
                                                        stroke = 1),
                               draw_key = ggplot2::draw_key_point,

                               # For points across a line for each level or country
                               draw_group = function(data, panel_scales, coord) {
                                 coords <- coord$transform(data, panel_scales)

                               # Creating a pointsGrob with pch 21 which is a filled circle.
                               points <- grid::pointsGrob(coords$x, coords$y,
                                                        pch = coords$shape,
                                                        size = grid::unit(coords$size/4, "lines"),
                                                        gp = gpar(col = alpha(coords$colour, coords$alpha),
                                                                 fill = alpha(coords$colour, coords$alpha)
                                                          )
                               )

                                # Creating a lineGrob
                                line <- grid::segmentsGrob(
                                 x0 = 0, y0 = coords$y,
                                 x1 = 1, y1 = coords$y,
                                 gp = gpar(col="grey", alpha=0.5, size=1)
                                 )

                               grid::gList(points, line)
                        }
)


#' @title The geom_timeline function
#'
#' @description This function is used in conjunction with GeomTimeline.
#' This geom helps to plot a timeline of earthquakes for a given country / countries
#' with points representing earthquake events, point size representing
#' earthquake magnitude on richter scale and colour representing number of deaths.
#' x (the date) is a required aesthetic whereas y (country) is optional.
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  Plot1 <- eq_rawdata %>%
#'  eq_clean_data() %>%
#'  select(c("Country", "Location_Name", "Date", "Mag", "Latitude", "Longitude", "Deaths")) %>%
#'  filter(Country %in% c("India", "China", "Mexico")) %>%
#'  filter(Date>="2000-01-01" & Date<"2010-12-31") %>%
#'  ggplot(aes(x=Date, y=Country, color = Deaths, size = Mag, label=Location_Name)) +
#'  geom_timeline()
#'  Plot1
#'  }


geom_timeline <- function(mapping=NULL, data=NULL,
                        stat="identity", position="identity", na.rm=FALSE,
                        show.legend=NA,
                        inherit.aes=TRUE, ...) {

  ggplot2::layer(
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm=na.rm, ...)
  )
}


#' @title Stat for creating data subset to be used for labeling
#'
#' @description
#' A Stat is built using ggproto to be used in conjuction with a geom that adds a timeline label layer.
#' This stat helps to subset the data to user provided n_max number of earthquakes by magnitude.
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'  Plot1 <- eq_rawdata %>%
#'  eq_clean_data() %>%
#'  select(c("Country", "Location_Name", "Date", "Mag", "Latitude", "Longitude", "Deaths")) %>%
#'  filter(Country %in% c("India", "China", "Mexico")) %>%
#'  filter(Date>="2000-01-01" & Date<"2010-12-31") %>%
#'  ggplot(aes(x=Date, y=Country, color = Deaths, size = Mag, label=Location_Name)) +
#'  geom_timeline() %>% stat_tll(n_max=5)
#'  PLot1
#'  }


StatTLL <- ggplot2::ggproto("StatTLL", Stat,
                   required_aes = c("x", "y"),
                   compute_group = function(data, scales, n_max) {
                         data <- data %>% head(n_max)
                       }
)

#' @title Stat function for creating data subset to be used for labeling
#'
#' @description
#' This function is a standard stat function used in conjuction with StatTLL that adds a timeline label layer.
#' The geom is used as "segment" and n_max is an additional parameter.
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'  Plot1 <- eq_rawdata %>%
#'  eq_clean_data() %>%
#'  select(c("Country", "Location_Name", "Date", "Mag", "Latitude", "Longitude", "Deaths")) %>%
#'  filter(Country %in% c("India", "China", "Mexico")) %>%
#'  filter(Date>="2000-01-01" & Date<"2010-12-31") %>%
#'  ggplot(aes(x=Date, y=Country, color = Deaths, size = Mag, label=Location_Name)) +
#'  geom_timeline() %>% stat_tll(n_max=5)
#'  Plot1
#'  }

stat_tll <- function(mapping = NULL, data = NULL, geom = "segment",
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, n_max = NULL, ...) {

  ggplot2::layer(
    stat = StatTLL, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n_max = n_max, na.rm = na.rm, ...)

  )
}


#' @title Defines the GeomTimelinelabel class
#'
#' @description The ggproto() function is used to construct a new class corresponding
#' to a new geom. The geom_* function is constructed as a regular function which helps
#' to add annotations to the earthquake data. #' This geom adds a vertical line to
#' each data point with a text annotation (e.g. the location of the earthquake) attached
#' to each line. There is an option for user to subset to n_max number of earthquakes
#' where we take the n_max largest (by magnitude) earthquakes. Aesthetics are x, which
#' is the date of the earthquake and label which takes the column name from which
#' annotations will be obtained. Please note that user must run geom_timeline first so
#' that we have our points to annotate.
#'
#' @import ggplot2, grid
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'  Plot1 <- eq_rawdata %>%
#'  eq_clean_data() %>%
#'  select(c("Country", "Location_Name", "Date", "Mag", "Latitude", "Longitude", "Deaths")) %>%
#'  filter(Country %in% c("India", "China", "Mexico")) %>%
#'  filter(Date>="2000-01-01" & Date<"2010-12-31") %>%
#'  ggplot(aes(x=Date, y=Country, color = Deaths, size = Mag, label=Location_Name)) +
#'  geom_timeline() %>%
#'  geom_timeline_label(n_max=5) +
#'  scale_size_continuous(name = "Richter scale value", breaks = c(2,4,6)) +
#'  scale_color_continuous(name = "# Deaths") +
#'  theme_bw() +
#'  theme(legend.position = "right")+
#'  labs(title = "Earthquakes Summary")
#'  Plot1
#'  }

GeomTimelinelabel <- ggplot2::ggproto("GeomTimelinelabel", Geom,
  required_aes = c("x"),
  default_aes = aes(size = 15, alpha = 0.7, shape = 21, color = "blue"),
  draw_key = draw_key_point,

  draw_panel = function(data, panel_scales, coord) {

    coords <- coord$transform(data, panel_scales)

    # Creating a segmentGrob for the leader line for select points to be labelled
    lines <- grid::segmentsGrob(x0 = coords$x,
                                y0 = coords$y,
                                x1 = coords$x,
                                y1 = coords$y + 0.1,
                                default.units = "npc",
                                gp = grid::gpar(col =      "grey",
                                                alpha =    coords$alpha,
                                                fontsize = coords$size,
                                                lwd =      coords$stroke))

    # Creating a textGrob fto label the selected points
    texts <- textGrob(label = coords$label,
                      x = coords$x,         # X axis coordinates same as point
                      y = coords$y + 0.12,  # position slightly above the line
                      just = "left",        # Left justified text
                      rot = 45,             # Text rotation
                      check.overlap = TRUE, # Only display one of the overlapped text
                      default.units = "npc",# Default units
                      gp = grid::gpar(col =      "steelblue",
                                      fontsize = 10,
                                      lwd =      2))

    grid::gTree(children = grid::gList(lines, texts))

    }
)

#' @title The geom_timeline_label function
#'
#' @description This function is used in conjunction with GeomTimelinelabel.
#' Note that user must run geom_timeline first so that we have our points to annotate.
#' This geom adds a layer to annotate the largest n_max earthquakes with a
#' vertical line and a label tagging these points.
#'
#' @param n_max Integer for maximum number of earthquakes to display.
#'
#' @import ggplot2, dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  Plot1 <- eq_rawdata %>%
#'  eq_clean_data() %>%
#'  select(c("Country", "Location_Name", "Date", "Mag", "Latitude", "Longitude", "Deaths")) %>%
#'  filter(Country %in% c("India", "China", "Mexico")) %>%
#'  filter(Date>="2000-01-01" & Date<"2010-12-31") %>%
#'  ggplot(aes(x=Date, y=Country, color = Deaths, size = Mag, label=Location_Name)) +
#'  geom_timeline() %>%
#'  geom_timeline_label(n_max=5) +
#'  scale_size_continuous(name = "Richter scale value", breaks = c(2,4,6)) +
#'  scale_color_continuous(name = "# Deaths") +
#'  theme_bw() +
#'  theme(legend.position = "right")+
#'  labs(title = "Earthquakes Summary")
#'  PLot1
#'  }

geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, n_max=NULL,
                          ...) {

    ggplot2::layer(
    geom = GeomTimelinelabel, mapping = mapping,
    data = data, stat = StatTLL, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n_max = n_max,  na.rm = na.rm, ...)
  )
}
