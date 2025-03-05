# R -q -e "shiny::runApp('app')"

library(shiny)
library(shinydashboard)
library(shinyFeedback)
library(shinyjs)
library(sf)
library(leaflet)
library(leafpop)
library(waiter)
library(reactable)
library(readxl)
library(terra)
library(markdown)
# library(raster)
# library(rgdal)

## set file size to 25 MB
options(shiny.maxRequestSize = 25 * 1024^2)

waiter_set_theme(html = tagList(
        spin_folding_cube(),
        h4("Loading...")
    ), 
    color = "#cccccc", image = "loading-screen.png")

source("functions-data.R")
source("functions-maps.R")

dat <- readRDS("app-data-regions.rds")
h <- readRDS("app-data-herds.rds")
s <- readRDS("app-data-scrip.rds")

TITLE <- "BC Prioritization Support Tool"
MAP_HEIGHT <- 600
ALPHA <- 0.8
HERD_BORDER <- 2
DOWNLOAD_CRS <- 4269

# used for groups
pal <- colorFactor(
       palette = c("forestgreen","green1", "darkolivegreen2","gray", "gray40"),
       domain = as.factor(1:5),
       ordered = TRUE,
       na.color = NA)
# used for SCRIP
pal2 <- colorFactor(
       palette = c("darkgreen","green", "lightgreen"),
       domain = factor(c("High", "Medium", "Low"), c("High", "Medium", "Low")),
       ordered = TRUE,
       na.color = NA)

ui_formula <- function(region_name, custom_var = FALSE) {
    f1 <- switch(tolower(region_name),
        "boreal" = "<em>Normalized Bang for Buck</em> * <em>Normalized Caribou Use</em>",
        "central" = "<em>Normalized Bang for Buck</em> + <em>Normalized Caribou Use</em>",
        "<em>Normalized Bang for Buck</em> + <em>Proportion Core Caribou Habitat</em> + <em>Proportion Ungulate Winter Range</em> + <em>Proportion Moose Habitat</em>")
    if (tolower(region_name) == "boreal" && custom_var)
        f1 <- "<em>Normalized Bang for Buck</em> + <em>Normalized Caribou Use</em>"
    if (custom_var)
        f1 <- paste0(f1, " + <em>Custom Variable</em>")
    f2 <- switch(tolower(region_name),
        "boreal" = "(<em>% Altered</em> &ndash; <em>% Altered Post Restoration</em>) / <em>Seismic Line Density</em>",
        "central" = "(<em>% Altered</em> &ndash; <em>% Altered Post Restoration</em>) / (<em>Seismic Line Density</em> + <em>Restorable Road Density</em>)",
        "<em>% Altered</em> / <em>Restorable Road Density</em>")
    paste0(
        "<div id = \"", paste0("math_", tolower(region_name)), "\">",
        "<p><center>Zones are 20% percentiles of the weight calculated as:</center></p>",
        sprintf("<p><center>%s</center></p>", f1),
        "<p><center>Where Bang for Buck is:</center></p>",
        sprintf("<p><center>%s</center></p>", f2),
        "</div>")
}
