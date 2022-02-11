# Load packages
library(dplyr)
library(tidyselect)
library(tidyr)
library(readr)
library(here)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(tm)
library(colorspace)

#source('data_clean_lib.R')

#################################
#define some variables used often
#################################

#starting date for date slider and default starting date to show
mindate = as.Date("2020-02-01","%Y-%m-%d")
defaultdate = as.Date("2020-08-01","%Y-%m-%d")


###########################################
# Define server functions
###########################################
server <- function(input, output, session) {
  

} #end server function


#################################
# Define UI
#################################
ui <- fluidPage(
  tags$head(includeHTML(here("www","google-analytics.html"))), #this is for Google analytics tracking.
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  includeCSS(here("www","appstyle.css")),
                            tags$div(
                              id = "bigtext",
                              "We have stopped updating and maintenance of this tracker. 
                              Please find another tracker, such as the one from the NY Times or Our World in Data.
                              ")
) #end fluidpage and UI part of shiny app
#end UI of shiny app
###########################################



# Create Shiny object
shinyApp(ui = ui, server = server)