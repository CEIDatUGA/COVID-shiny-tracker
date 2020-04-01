# Load packages
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(shinythemes)
library(shinyWidgets)
library(plotly)
#library(tidyverse)
#library(htmlwidgets)
#library(lubridate)
#library(ggthemes)
#library(tibbletime)
#library(directlabels)

#################################
# Load all data
# should be online so things update automatically
#################################
#this is CEID repo, not using right now
#us_cases <- read_csv("https://raw.githubusercontent.com/CEIDatUGA/COVID-19-DATA/master/US/US_wikipedia_cases_fatalities/UScases_by_state_wikipedia.csv?token=ADLZCO4TEZZM2KBM2RFXYDS6Q76TG")
#world_cases <- read_csv("https://raw.githubusercontent.com/CEIDatUGA/COVID-19-DATA/master/global/global_cases_by_country/worldCases.csv?token=ADLZCO4MU2KCSJXNS6NHOAK6RPPTU")

#Hopkins repo
us_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
us_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

world_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
word_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

#data for population size for each state/country so we can compute cases per 100K
#us_popsize <-  
#World_popsize <- 

#data for testing
#from https://covidtracking.com/
us_testing <- read_csv("http://covidtracking.com/api/states/daily.csv")


#################################
# Clean data
#################################

#is included in us_deaths, could be extracted from there and used
#us_popsize = us_deaths %>% dplyr::select(Province_State, Population) %>%

us_cases_clean <- us_cases %>% dplyr::select(-c(UID,iso2,iso3,code3,FIPS,Admin2,Country_Region,Lat,Long_,Combined_Key)) %>%
                                tidyr::pivot_longer(-Province_State, names_to = 'Date', values_to = 'All_Counts') %>% 
                                group_by(Province_State,Date) %>% summarise(All_Counts = sum(All_Counts)) %>%
                                mutate(Date = as.Date(Date,format="%m/%d/%Y")) %>% arrange(Date) %>%
                                mutate(New_Counts = c(0,diff(All_Counts))) 
  
us_deaths_clean <- us_deaths %>% dplyr::select(-c(UID,iso2,iso3,code3,FIPS,Admin2,Country_Region,Lat,Long_,Combined_Key, Population)) %>%
                                  tidyr::pivot_longer(c(-Province_State), names_to = 'Date', values_to = 'All_Counts') %>% 
                                  group_by(Province_State,Date) %>% summarise(All_Counts = sum(All_Counts)) %>%
                                  mutate(Date = as.Date(Date,format="%m/%d/%Y")) %>% arrange(Date) %>%
                                  mutate(New_Counts = c(0,diff(All_Counts))) 


Province_State = unique(us_cases_clean$Province_State)

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("COVID-19"),
                sidebarLayout(
                  sidebarPanel(
                    withMathJax(),
                    shinyWidgets::pickerInput("state_selector", "Select States", Province_State, multiple = TRUE, 
                                options = list(`actions-box` = TRUE),
                                selected = c("California","Washington", "Georgia")),
                    shiny::selectInput("case_death", "Outcome",c("Cases" = "case", "Deaths" = "death")),
                    shiny::selectInput("daily_tot", "Daily or Total",c("Daily" = "daily", "Total" = "tot")),
                    shiny::selectInput("absolute_scaled", "Absolute or scaled values",c("Absolute number" = "actual", "Per 100K" = "scaled")),
                    shiny::selectInput("xscale", "Use time or days since a certain number of reported total cases/deaths  on x-axis",c("Time" = "x_time", "Cases" = "x_count")),
                    sliderInput("count_limit", "Choose the number of cases/deahts in a state to start graphs", min = 1, max = 500, value = 100),
                    shiny::selectInput("yscale", "Y-scale",c("linear" = "linear", "logarithmic" = "logarithmic")),
                    br(), br()
                  ),
                  
                  # Output:
                  mainPanel(
                    #change to plotOutput if using static ggplot object
                    plotlyOutput(outputId = "linePlot", height = "300px"),
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  #Reactive function to prepare plot data
  get_plot_data <- reactive({  
    
    #choose either cases or deaths to plot
    if (input$case_death == 'case')
    {
      plot_dat <- us_cases_clean
    }
    else
    {
      plot_dat <- us_deaths_clean
    }

    #adjust data to align for plotting by cases on x-axis
    if (input$xscale == 'x_count')
    {
      plot_dat <- plot_dat %>% filter(All_Counts >= count_limit) %>%  
                               mutate(Time = as.numeric(Date)) %>%
                               group_by(Province_State) %>% 
                               mutate(Time = Time - min(Time))
    }
    else
    {
      plot_dat <- plot_dat %>% mutate(Time = Date)
    } 
    
    #choose either actual number or per 100K 
    #not implemented yet
    if (input$absolute_scaled == 'absolute')
    {
    
    }
    else
    {
      
    }
    
  if (input$daily_tot == 'daily')
  {
    plot_dat <- plot_dat %>% mutate(Counts = New_Counts)
  }
  else
  {
    plot_dat <- plot_dat %>% mutate(Counts = All_Counts)
  }
 
  }) #end reactive function that produces the right plot_dat data needed
  
    
  #change to renderPlot if using static ggplot object
  output$linePlot <- renderPlotly({
    scaleparam <- "fixed"
    #if(input$scalesfree) scaleparam <- "free_y"
    p <- get_plot_data() %>% 
      #Filter data for cases >0 and selected states
      filter(Counts > 0) %>% 
      filter(Province_State %in% input$state_selector) %>% 
      #Begin plot
      ggplot(aes(x=Time, y = Counts, color = Province_State))+
      geom_line()+
      geom_point()+
      theme_light()+
      ylab("Cumulative Number of Cases (linear scale)")
    #Flip to logscale if selected
    if(input$yscale == "logarithmic") {
      p <- p + scale_y_log10() + labs(y = "Cumulative Number of Cases (log-scale)")
    }
  
   ggplotly(p)
    
  })
} #end server function
  
# Create Shiny object
shinyApp(ui = ui, server = server)

