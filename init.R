# init.R
#
# Example R code to install packages if not already installed
#
my_packages = c(
  "shiny",
  "tidyverse",
  "rvest",
  "plyr",
  "dplyr",
  "ggplot2",
  "gganimate",
  "sjmisc",
  "gt",
  "gtExtras",
  "magick",
  "leaflet",
  "sf",
  "stringi",
  "geojsonio",
  "shiny.router",
  "shinydashboard",
  "datasets",
  "bslib"
)

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}
invisible(sapply(my_packages, install_if_missing))