#!/usr/local/bin/Rscript

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

source('data_clean_lib.R')

#################################
#define some variables used often
#################################

#starting date for date slider and default starting date to show
mindate = as.Date("2020-02-01","%Y-%m-%d")
defaultdate = as.Date("2020-08-01","%Y-%m-%d")

get_data()

