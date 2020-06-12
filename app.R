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
library(RColorBrewer)
library(tm)

#prevent shiny from overwriting our error message
#not used right now, using safeError below instead
#options(shiny.sanitize.errors = FALSE)

#################################
# make functions
#################################

#function to reformat datasets to long format following import cleaning
to_long <- function(df){
  long_format <- tidyr::pivot_longer(df, cols = c(-Location, -Population_Size, -Date), names_to = "variable", values_to = "value")
  return(long_format)
}

#function to standardize county names from different datasets then merge with the county_popsize data
clean_counties <- function(df,county_popsize){
  extract_words <- c(" city", " City", " City and Borough", " Census Area"," County", " Borough",  "Municipality of ", " County and City", " Parish", " Municipality")
  df$county_name <- tm::removeWords(df$county_name, extract_words)
  county_complete <- inner_join(df, county_popsize)
  return(county_complete)
}

#function to add all US to data
add_US <- function(df){
  df_new <- df %>% group_by(Date) %>% 
                   summarize_if(is.numeric, sum, na.rm=TRUE) %>%
                   mutate(Location = "US") %>%
                   mutate(Population_Size = max(Population_Size))
  return(df_new)
}
#function to clean usafacts state-level data (part 1)
clean_usafacts_p1 <- function(df, state_df){
  part_one <- df %>% dplyr::group_by(stateFIPS) %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE) %>%
    dplyr::select(-countyFIPS) %>% 
    left_join(state_df)
  return(part_one)
}

#function to clean usafacts state-level data (part 2)
clean_usafacts_p2 <- function(df, us_popsize){
  part_two <- df %>% mutate(Date = as.Date(Date,format="%m/%d/%y")) %>% 
    rename(state_abr = State) %>%
    merge(us_popsize) %>%
    rename(Location = state, Population_Size = pop_size) %>%
    select(-c(state_abr))
  return(part_two)
}

#function to clean usafacts county-level data
clean_usafacts_p3 <- function(df){
  part_three <- df %>% select(-c(countyFIPS, stateFIPS)) %>%
    rename(Location = State, county_name = `County Name`) %>%
    dplyr::filter(county_name != "Statewide Unallocated")
  return(part_three)
}

#function to partially clean us_jhu data
clean_us_jhu <- function(df){
  usjhu <- df %>% filter(iso3 == "USA") %>%
    dplyr::select(c(-Country_Region, -Lat, -Long_, -UID, -iso2, -iso3, -code3, -Combined_Key)) %>%
    rename(state = Province_State)
  return(usjhu)
}

#function to partially clean world_jhu data
clean_world_jhu <- function(df, world_popsize){
  worldjhu <- df %>% dplyr::select(c(-`Province/State`, -Lat, -Long)) %>%
    rename(country= `Country/Region`) %>%
    group_by(country) %>% summarise_if(is.numeric, sum, na.rm = TRUE)%>%
    inner_join(world_popsize)
  return(worldjhu)
}

#################################
# Load all data
# should be online so things update automatically
#for speed, we only get data from the online source if the data is old, otherwise we load locally
#################################

# to ensure data gets refreshed on server, we need this
get_data <- function()
{
  filename = here("data",paste0("clean_data_",Sys.Date(),'.rds')) #if the data file for today is here, load then return from function
  if (file.exists(filename)) {
     all_data <- readRDS(file = filename)    
     return(all_data)  
  }
  #if data file is not here, go through all of the below
  
  #*******************************
  #let's get the popsize files standardized 
  #rename the population size to pop_size in each file
  #rename Location to state for county_popsize and add state_abr
  #all variable names lower case
  #might require a few lines of code adjustment in the cleaning code
  #******************************
  
  #data for population size for each state/country so we can compute cases per 100K
  us_popsize <- readRDS(here("data","us_popsize.rds")) %>% rename(state_abr = state, state = state_full, pop_size = total_pop)
  world_popsize <-readRDS(here("data","world_popsize.rds"))
  county_popsize <- readRDS(here("data", "county_popsize.rds"))
  
  all_data = list() #will save and return all datasets as list
  
   
  #################################
  # pull state level and testing data from Covidtracking and process
  #################################
  print('starting COVIDtracking')
  us_ct_data <- read_csv("https://covidtracking.com/api/v1/states/daily.csv")
  us_ct_clean <- us_ct_data %>% dplyr::select(c(date,state,positive,negative,total,hospitalized,death)) %>%
    mutate(date = as.Date(as.character(date),format="%Y%m%d")) %>% 
    group_by(state) %>% 
    arrange(date) %>%
    mutate(Daily_Test_Positive = c(0,diff(positive))) %>% 
    mutate(Daily_Test_Negative = c(0,diff(negative))) %>% 
    mutate(Daily_Test_All = c(0,diff(total))) %>% 
    mutate(Daily_Hospitalized = c(0,diff(hospitalized))) %>% 
    mutate(Daily_Deaths = c(0,diff(death))) %>% 
    rename(state_abr = state) %>%
    merge(us_popsize) %>%
    rename(Date = date, Location = state, Population_Size = pop_size, Total_Deaths = death, 
           Total_Cases = positive, Total_Hospitalized = hospitalized, 
           Total_Test_Negative = negative, Total_Test_Positive = positive, Total_Test_All = total) %>%
    mutate(Daily_Cases = Daily_Test_Positive, Total_Cases = Total_Test_Positive) %>%
    mutate(Daily_Positive_Prop = Daily_Test_Positive / Daily_Test_All) %>%
    mutate(Total_Positive_Prop = Total_Test_Positive / Total_Test_All) %>%
    select(-c(state_abr,Total_Test_Negative,Daily_Test_Negative))
  
  #add all US by summing over all variables
  #adding is not right approach for proportion test positive, so need to recompute
  all_us <- add_US(us_ct_clean) %>%
            mutate(Daily_Positive_Prop = Daily_Test_Positive / Daily_Test_All) %>%
            mutate(Total_Positive_Prop = Total_Test_Positive / Total_Test_All) 
    
  #combine all US data with rest of data
  us_ct_clean = rbind(us_ct_clean,all_us)
  
  #reformat to long
  us_ct_clean <-  to_long(us_ct_clean)
  us_ct_clean$value[!is.finite(us_ct_clean$value)] <- NA
  us_ct_clean <- na.omit(us_ct_clean)
  
  #################################
  # pull state level data from NYT and process
  #################################
  print('starting NYTimes')
  us_nyt_data <- readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  us_nyt_clean <- us_nyt_data %>% dplyr::select(c(date,state,cases,deaths)) %>%
    group_by(state) %>% 
    arrange(date) %>%
    mutate(Daily_Cases = c(0,diff(cases))) %>% 
    mutate(Daily_Deaths = c(0,diff(deaths))) %>%
    inner_join(us_popsize) %>%
    rename(Date = date, Location = state, Population_Size = pop_size, Total_Deaths = deaths, Total_Cases = cases)  %>%
    select(-state_abr) %>%
    data.frame()
  
  #add all US by summing over all variables
  all_us <- add_US(us_nyt_clean) 
  us_nyt_clean = rbind(us_nyt_clean,all_us)
  #reformat to long
  us_nyt_clean <- to_long(us_nyt_clean)
  
  #################################
  # pull state level data from USAFacts and process
  #################################
  print('starting USAFacts')
  usafct_case_data <- readr::read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv")
  usafct_death_data <- readr::read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv")
  
  state_df = usafct_case_data %>% 
    distinct(stateFIPS, .keep_all = TRUE) %>% 
    select(State,stateFIPS)
  
  usafct_case_clean <- clean_usafacts_p1(usafct_case_data, state_df) %>%
    tidyr::pivot_longer(-State, names_to = "Date", values_to = "Total_Cases") %>%
    clean_usafacts_p2(us_popsize) %>%
    group_by(Location) %>% arrange(Date) %>%
    mutate(Daily_Cases = c(0,diff(Total_Cases)))
  
  usafct_death_clean <- clean_usafacts_p1(usafct_death_data, state_df) %>%  
    tidyr::pivot_longer(-State, names_to = "Date", values_to = "Total_Deaths") %>%
    clean_usafacts_p2(us_popsize) %>%
    group_by(Location) %>% arrange(Date) %>%
    mutate(Daily_Deaths = c(0,diff(Total_Deaths)))
  
  usafct_clean <- left_join(usafct_case_clean, usafct_death_clean) %>%
    group_by(Location) %>% 
    arrange(Date)  %>%
    ungroup()
  
  #add all US by summing over all variables
  all_us <- add_US(usafct_clean)
  usafct_clean = rbind(usafct_clean,all_us)
  #reformat to long
  usafct_clean <- to_long(usafct_clean)

  #################################
  # pull US data from JHU github and process
  #################################
  print('starting JHU')
  us_jhu_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
  us_jhu_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
  # Clean cases
  us_jhu_cases_clean <- clean_us_jhu(us_jhu_cases) %>%
    tidyr::pivot_longer(cols = c(-state, -FIPS, -Admin2), names_to = "Date", values_to = "Cases")
  # Clean deaths
  us_jhu_deaths_clean <-clean_us_jhu(us_jhu_deaths) %>% 
    tidyr::pivot_longer(cols = c(-state, -FIPS, -Admin2), names_to = "Date", values_to = "Deaths")
  #combine cases and deaths
  us_jhu_combined <- inner_join(us_jhu_cases_clean, us_jhu_deaths_clean)
  us_jhu_total <- inner_join(us_jhu_combined, us_popsize) %>%
    mutate(Date = as.Date(as.character(Date),format="%m/%d/%y")) %>%
    group_by(state, Admin2) %>% arrange(Date) %>%
    mutate(Daily_Cases = c(0,diff(Cases))) %>%
    mutate(Daily_Deaths = c(0,diff(Deaths))) %>% 
    ungroup() %>%
    rename(Total_Deaths = Deaths, Total_Cases = Cases, Population_Size = pop_size, county_name = Admin2) %>% 
    select(-state_abr) 
  
  #Use us_jhu_total to create both the county and state level datasets 
  #Pull county data and reformat to long
  county_jhu_clean <- us_jhu_total %>% select(-c(Population_Size, FIPS)) %>%
    pivot_longer(cols = c(-state, -Date, -county_name), names_to = "variable", values_to = "value")
  
  #add county population numbers 
  county_jhu_clean <- inner_join(county_jhu_clean, county_popsize)
  
  #pull state data and aggregate county values
  us_jhu_clean <- us_jhu_total %>% 
                  select(-FIPS, -county_name) %>% 
                  rename(Location = state) %>%
                  group_by(Location, Date, Population_Size) %>% 
                  summarise_if(is.numeric, sum, na.rm=TRUE) %>% 
                  data.frame()
  
  #add total US values
  all_us <- add_US(us_jhu_clean)
  us_jhu_clean = rbind(us_jhu_clean,all_us)
  #reformat state data to long
  us_jhu_clean <- to_long(us_jhu_clean)
  
  #################################
  # pull world data from JHU github and process
  #################################
  world_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  world_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  # clean the data for plotting
  world_cases <- clean_world_jhu(world_cases, world_popsize) %>%
    tidyr::pivot_longer(cols = c(-country, -country_pop), names_to = "date", values_to = "cases")
  world_deaths <- clean_world_jhu(world_deaths, world_popsize) %>%
    tidyr::pivot_longer(cols = c(-country, -country_pop), names_to = "date", values_to = "deaths")
  # join the data
  world_jhu_clean <- inner_join(world_cases, world_deaths) %>% 
    mutate(date = as.Date(as.character(date),format="%m/%d/%y")) %>%
    group_by(country) %>% arrange(date) %>%
    mutate(Daily_Cases = c(0,diff(cases))) %>%
    mutate(Daily_Deaths = c(0,diff(deaths))) %>%
    ungroup() %>%
    rename(Date = date, Total_Deaths = deaths, Total_Cases = cases, Location = country, Population_Size = country_pop) 
  #reformat to long
  world_jhu_clean <- to_long(world_jhu_clean)

  
  #################################
  # pull world data from OWID github and process
  #################################
  message('starting OWID')
  owid_data <- readr::read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
  world_owid_clean <- owid_data %>% dplyr::select(-tests_units, -iso_code, -continent) %>%
    rename(Total_Cases = total_cases, Total_Deaths = total_deaths, Daily_Cases = new_cases, Daily_Deaths = new_deaths, Location = location, Date = date, Daily_Test_All = new_tests, Total_Test_All = total_tests) %>% 
    mutate(Population_Size = Total_Cases / total_cases_per_million * 1000000) %>% #back-calculate population size
    mutate(Location = recode(Location, "United States" = "US")) %>%
    mutate(Daily_Test_Positive = Daily_Cases ) %>% #assuming new cases means new positive tests
    mutate(Total_Test_Positive = Total_Cases ) %>%
    mutate(Daily_Positive_Prop = Daily_Test_Positive / Daily_Test_All) %>%
    mutate(Total_Positive_Prop = Total_Test_Positive / Total_Test_All) %>%
    select( - contains('thousand'), - contains('million'))
  #reformat to long
  world_owid_clean <- to_long(world_owid_clean)

  #################################
  # combine all data 
  #################################
  
  
  message('starting state/county data combining')
  
  # give each US dataset a source label
  us_source_var = c("COVIDTracking","NYTimes","JHU","USAFacts")
  
  us_ct_clean$source = us_source_var[1]
  us_nyt_clean$source = us_source_var[2]
  us_jhu_clean$source = us_source_var[3]
  usafct_clean$source = us_source_var[4]
  
  #combine all US data from different sources
  #also do all variable/column names in lowercase
  us_dat <- rbind(us_ct_clean, us_nyt_clean, us_jhu_clean, usafct_clean) 
  us_dat <- us_dat %>% rename(date = Date, location = Location, populationsize = Population_Size)
  
  #reorder columns
  us_dat <- us_dat[c("source","location","populationsize","date","variable","value")]
  
  
  message('starting world data combining')
  
  # give each world dataset a source label
  world_source_var = c("JHU", "OWID")
  
  world_jhu_clean$source = world_source_var[1]
  world_owid_clean$source = world_source_var[2]
  
  #combine all world data from different sources
  world_dat <- rbind(world_jhu_clean, world_owid_clean) 
  world_dat <- world_dat %>% rename(date = Date, location = Location, populationsize = Population_Size)
  
  world_dat <- world_dat[c("source","location","populationsize","date","variable","value")]
  
  message('starting county data combining')
  
  # give each county dataset a source label
  county_source_var = c("JHU", "USAFacts", "NYTimes")
  
  county_jhu_clean$source = county_source_var[1]
  
  #combine all county data from different sources
  #also do all variable/column names in lowercase
  county_dat <- county_jhu_clean %>%
                rename(date = Date, location = county_name, populationsize = pop_size)
  
  #reorder columns
  county_dat <- county_dat[c("source","location","state", "populationsize","date","variable","value","state_abr")]
  
  #set negative values to zero
  #Comment out the line below to keep negative values in the data for debugging
  ############################################
  us_dat$value[us_dat$value < 0] <- 0
  world_dat$value[world_dat$value < 0] <- 0
  county_dat$value[county_dat$value < 0] <- 0
  ############################################
  
  #combine data in list  
  all_data$us_dat = us_dat
  all_data$world_dat = world_dat
  all_data$county_dat = county_dat
  
  message('Data cleaning done.')
  
  #save the data
  saveRDS(all_data, filename)    
  return(all_data)
  
  
} # end the get-data function which pulls data from the various online sources and processes/saves  

###########################################
# function that re-reads the data every so often
###########################################
all_data <- reactivePoll(intervalMillis = 1000*60*60*3, # pull new data every N hours
                         session = NULL,
                         checkFunc = function() {Sys.time()}, #this will always return a different value, which means at intervals specified by intervalMillis the new data will be pulled
                         valueFunc = function() {get_data()} )

#read data is reactive, doesn't work for rest below 
all_dat = isolate(all_data())

# pull data out of list 
world_dat = all_dat$world_dat 
us_dat = all_dat$us_dat 
county_dat = all_dat$county_dat

#define variables for location and source selectors
state_var = sort(unique(us_dat$location))  
state_var = c("US",state_var[!state_var=="US"]) #move US to front
country_var = sort(unique(world_dat$location))
county_var = sort(unique(county_dat$location))
state_var_county = sort(unique(county_dat$state))

us_source_var = unique(us_dat$source)
world_source_var = unique(world_dat$source)
county_source_var = unique(county_dat$source)

#################################
# Define UI
#################################
ui <- fluidPage(
  tags$head(includeHTML(here("www","google-analytics.html"))), #this is for Google analytics tracking.
  includeCSS(here("www","appstyle.css")),
  #main tabs
  navbarPage( title = "COVID-19 Tracker", id = 'current_tab', selected = "us", header = "",
              tabPanel(title = "US States", value = "us",
                       sidebarLayout(
                         sidebarPanel(
                           shinyWidgets::pickerInput("state_selector", "Select State(s)", state_var, multiple = TRUE,options = list(`actions-box` = TRUE), selected = c("Georgia","California","Washington") ),
                           shiny::div("US is at start of state list."),
                           br(),
                           shinyWidgets::pickerInput("source_selector", "Select Source(s)", us_source_var, multiple = TRUE,options = list(`actions-box` = TRUE), selected = c("COVIDTracking") ),
                           shiny::div("Choose data sources (see 'About' tab for details)."),
                           br(),
                           shiny::selectInput( "case_death",   "Outcome",c("Cases" = "Cases", "Hospitalizations" = "Hospitalized", "Deaths" = "Deaths")),
                           shiny::div("Modify the top plot to display cases, hospitalizations, or deaths."),
                           br(),
                           shiny::selectInput("daily_tot", "Daily or cumulative numbers", c("Daily" = "Daily", "Total" = "Total" )),
                           shiny::div("Modify all three plots to show daily or cumulative data."),
                           br(),
                           shiny::selectInput("show_smoother", "Add trend line", c("No" = "No", "Yes" = "Yes")),
                           shiny::div("Shows a trend line for cases/hospitalizations/deaths plot."),
                           br(),
                           shiny::selectInput( "absolute_scaled","Absolute or scaled values",c("Absolute Number" = "actual", "Per 100,000 persons" = "scaled") ),
                           shiny::div("Modify the top two plots to display total counts or values scaled by the state/territory population size."),
                           br(),
                           shiny::selectInput("xscale", "Set x-axis to calendar date or days since a specified total number of cases/hospitalizations/deaths", c("Calendar Date" = "x_time", "Days since N cases/hospitalizations/deaths" = "x_count")),
                           sliderInput(inputId = "x_limit", "Select a date or outcome value from which to start the plots.", min = as.Date("2020-01-22","%Y-%m-%d"),  max = Sys.Date(), value = as.Date("2020-02-01","%Y-%m-%d") ),
                           shiny::div("Modify all three plots to begin at a specified starting date or outcome value designated in the slider above."),
                           br(),
                           shiny::selectInput(  "yscale", "Y-scale", c("Linear" = "lin", "Logarithmic" = "log")),
                           shiny::div("Modify the top two plots to show data on a linear or logarithmic scale."),
                           br()
                         ),         #end sidebar panel
                         # Output:
                         mainPanel(
                           #change to plotOutput if using static ggplot object
                           plotlyOutput(outputId = "case_death_plot", height = "300px"),
                           #change to plotOutput if using static ggplot object
                           plotlyOutput(outputId = "testing_plot", height = "300px"),
                           #change to plotOutput if using static ggplot object
                           plotlyOutput(outputId = "testing_frac_plot", height = "300px")
                         ) #end main panel
                       ) #end sidebar layout     
              ), #close US tab
              
           
              tabPanel( title = "US Counties", value = "county",
                        sidebarLayout(
                          sidebarPanel(
                            #County selector 
                            shinyWidgets::pickerInput("state_selector_c", "Select state", state_var_county,  multiple = FALSE, options = list(`actions-box` = TRUE), selected = c("Georgia")),
                            shinyWidgets::pickerInput("county_selector", "Select counties", county_var,  multiple = TRUE, options = list(`actions-box` = TRUE), selected = county_var[1]),
                            #shinyWidgets::pickerInput("source_selector_c", "Select Source(s)", county_source_var, multiple = TRUE,options = list(`actions-box` = TRUE), selected = c("JHU") ),
                            #shiny::div("Choose data sources (see 'About' tab for details)."),
                            #br(),
                            shiny::selectInput( "case_death_c", "Outcome", c("Cases" = "Cases", "Deaths" = "Deaths")),
                            shiny::div("Modify the plot to display cases or deaths."),
                            br(),
                            shiny::selectInput("daily_tot_c", "Daily or cumulative numbers", c("Daily" = "Daily", "Total" = "Total")),
                            shiny::div("Modify the plot to reflect daily or cumulative data."),
                            br(),
                            shiny::selectInput("show_smoother_c", "Add trend line", c("No" = "No", "Yes" = "Yes")),
                            shiny::div("Shows a trend line for cases/hospitalizations/deaths plot."),
                            br(),
                            shiny::selectInput("absolute_scaled_c", "Absolute or scaled values", c("Absolute Number" = "actual", "Per 100,000 persons" = "scaled") ),
                            shiny::div("Modify the plot to display total counts or values scaled by the county population size."),
                            br(),
                            sliderInput(inputId = "x_limit_c", "Select a date at which to start the plots.", min = as.Date("2020-01-22","%Y-%m-%d"),  max = Sys.Date(), value = as.Date("2020-02-01","%Y-%m-%d") ),
                            br(),
                            shiny::selectInput(  "yscale_c", "Y-scale", c("Linear" = "lin", "Logarithmic" = "log")),
                            shiny::div("Modify the plot to show data on a linear or logarithmic scale."),
                            br()
                          ), #end sidebar panel
                          
                          mainPanel(
                            #change to plotOutput if using static ggplot object
                            plotlyOutput(outputId = "county_case_death_plot", height = "500px")
                          ) #end main panel
                          
                        ), #close sidebar layout
              ), #close county tab
              
              tabPanel( title = "World", value = "world",
                        sidebarLayout(
                          sidebarPanel(
                            #Country selector coding with US, Italy, and Spain as always selected for a defult setting, will flash an error with none selected
                            shinyWidgets::pickerInput("country_selector", "Select countries", country_var,  multiple = TRUE, options = list(`actions-box` = TRUE), selected = c("US", "United Kingdom", "Germany")),
                            shinyWidgets::pickerInput("source_selector_w", "Select Source(s)", world_source_var, multiple = TRUE,options = list(`actions-box` = TRUE), selected = c("JHU") ),
                            shiny::div("Choose data sources (see 'About' tab for details)."),
                            br(),
                            shiny::selectInput( "case_death_w", "Outcome", c("Cases" = "Cases", "Deaths" = "Deaths")),
                            shiny::div("Modify the plot to display cases or deaths."),
                            br(),
                            shiny::selectInput("daily_tot_w", "Daily or cumulative numbers", c("Daily" = "Daily", "Total" = "Total")),
                            shiny::div("Modify the plot to reflect daily or cumulative data."),
                            br(),
                            shiny::selectInput("show_smoother_w", "Add trend line", c("No" = "No", "Yes" = "Yes")),
                            shiny::div("Shows a trend line for cases/hospitalizations/deaths plot."),
                            br(),
                            shiny::selectInput("absolute_scaled_w", "Absolute or scaled values", c("Absolute Number" = "actual", "Per 100,000 persons" = "scaled") ),
                            shiny::div("Modify the plot to display total counts or values scaled by the country population size."),
                            br(),
                            shiny::selectInput("xscale_w", "Set x-axis to calendar date or days since a specified total number of cases/deaths", c("Calendar Date" = "x_time", "Days since N cases/hospitalizations/deaths" = "x_count")),
                            sliderInput(inputId = "x_limit_w", "Select a date or outcome value from which to start the plots.", min = as.Date("2020-01-22","%Y-%m-%d"),  max = Sys.Date(), value = as.Date("2020-02-01","%Y-%m-%d") ),
                            shiny::div("Modify all three plots to begin at a specified starting date or outcome value designated in the slider above."),
                            br(),
                            shiny::selectInput(  "yscale_w", "Y-scale", c("Linear" = "lin", "Logarithmic" = "log")),
                            shiny::div("Modify the plot to show data on a linear or logarithmic scale."),
                            br()
                          ), #end sidebar panel
                          
                          mainPanel(
                            #change to plotOutput if using static ggplot object
                            plotlyOutput(outputId = "world_case_death_plot", height = "300px"),
                            #change to plotOutput if using static ggplot object
                            plotlyOutput(outputId = "world_testing_plot", height = "300px"),
                            #change to plotOutput if using static ggplot object
                            plotlyOutput(outputId = "world_testing_frac_plot", height = "300px")
                          ) #end main panel
                          
                        ), #close sidebar layout
              ), #close world tab
              
              tabPanel( title = "About", value = "about",
                        tagList(    
                          fluidRow( #all of this is the header
                            tags$div(
                              id = "bigtext",
                              "This COVID-19 tracker is brought to you by the",
                              a("Center for the Ecology of Infectious Diseases",  href = "https://ceid.uga.edu", target = "_blank" ),
                              "and the",
                              a("College of Public Health", href = "https://publichealth.uga.edu", target = "_blank"),
                              "at the",
                              a("University of Georgia.", href = "https://www.uga.edu", target = "_blank"),
                              "It was developed by",
                              a("Robbie Richards,", href = "https://rlrichards.github.io", target =  "_blank"),
                              a("William Norfolk", href = "https://github.com/williamnorfolk", target = "_blank"),
                              "and ",
                              a("Andreas Handel.", href = "https://www.andreashandel.com/", target = "_blank"),
                              'Source code for this project can be found',
                              a( "in this GitHub repository.", href = "https://github.com/CEIDatUGA/COVID-shiny-tracker", target = "_blank" ),
                              'We welcome feedback and feature requests, please send them as a',
                              a( "GitHub Issue", href = "https://github.com/CEIDatUGA/COVID-shiny-tracker/issues", target = "_blank" ),
                              'or contact',
                              a("Andreas Handel.", href = "https://www.andreashandel.com/", target = "_blank")
                            ),# and tag
                            tags$div(
                              id = "bigtext",
                              "We currently include 4 different data sources for US states.", 
                              a("The Covid Tracking Project",  href = "https://covidtracking.com/", target = "_blank" ),
                              "data source reports all and positive tests, hospitalizations (some states) and deaths. We interpret positive tests as corresponding to new cases. The",
                              a("New York Times (NYT),", href = "https://github.com/nytimes/covid-19-data", target = "_blank" ),
                              a("USA Facts", href = "https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/", target = "_blank" ),
                              "and",
                              a("Johns Hopkins University Center for Systems Science and Engineering (JHU)", href = "https://github.com/CSSEGISandData/COVID-19", target = "_blank" ),
                              "sources report cases and deaths."
                              ), 
                            tags$div(
                              id = "bigtext",
                              "For the county level plots, we use JHU data. (NY Times and USA Facts also provide data on the county level, however all 3 data sources are very similar and for speed/memory purposes we decided to only display one county level data source.)"         
                              ), 
                            tags$div(
                              id = "bigtext",
                              "World data comes from 2 different sources. One source is the", 
                              a("Johns Hopkins University Center for Systems Science and Engineering (JHU)", href = "https://github.com/CSSEGISandData/COVID-19", target = "_blank" ),
                              "the other source is",
                              a("Our World in Data (OWID).", href = "https://github.com/owid/covid-19-data/tree/master/public/data", target = "_blank" ),
                              "Both sources provide case and death data, OWID also provides testing data for some countries. For OWID, we assume reported cases correspond to positive tests."
                            ),
                            tags$div(
                              id = "bigtext",
                              "For more details on each data source, see their respective websites. Note that some data sources only report some data. Also, numbers might not be reliable, which can lead to nonsensical graphs (e.g. negative new daily cases/deaths or the fraction of positive tests being greater than 1). We make no attempt at cleaning/fixing the data, we only display it."
                            ),              
                            tags$div(
                              id = "bigtext",
                              a( "The Center for the Ecology of Infectious Diseases", href = "https://ceid.uga.edu", target = "_blank" ),
                              'has several additional projects related to COVID-19, which can be found on the',
                              a( "CEID COVID-19 Portal.", href = "http://2019-coronavirus-tracker.com/", target = "_blank" )
                            ), #Close the bigtext text div
                            tags$div(
                              id = "bigtext",
                              "If you are interested in learning more about infectious disease epidemiology and modeling, check out", 
                              a("our (slightly advanced) interactive modeling software and tutorial.", href = "https://shiny.ovpr.uga.edu/DSAIDE/", target = "_blank" )
                            ) #Close the bigtext text div
                          ), #close fluidrow
                          fluidRow( #all of this is the footer
                            column(3,
                                   a(href = "https://ceid.uga.edu", tags$img(src = "ceidlogo.png", width = "100%"), target = "_blank"),
                            ),
                            column(6,
                                   p('All text and figures are licensed under a ',
                                     a("Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.",
                                       href = "http://creativecommons.org/licenses/by-nc-sa/4.0/", target = "_blank"),
                                     'Software/Code is licensed under ',
                                     a("GPL-3.", href = "https://www.gnu.org/licenses/gpl-3.0.en.html" , target =  "_blank"),
                                     'See source data sites for licenses governing data.',
                                     a("UGA's Privacy Policy.", href = "https://eits.uga.edu/access_and_security/infosec/pols_regs/policies/privacy/" , target =  "_blank"),
                                     align = "center",
                                     style = "font-size:small"
                                   ) #end paragraph
                            ), #end middle column
                            column(3,
                                   a(href = "https://publichealth.uga.edu", tags$img(src = "cphlogo.png", width = "100%"), target = "_blank")
                            ) #end left column
                          ) #end fluidrow
                        ) #end taglist
              ) #close about tab
  ) #close NavBarPage
) #end fluidpage and UI part of shiny app
#end UI of shiny app
###########################################


###########################################
# Define server functions
###########################################
server <- function(input, output, session) {

  #watch the choice for the x-scale and choose what to show underneath accordingly
  observeEvent(input$xscale,
               {
                 if (input$xscale == 'x_count')
                 {
                   #Add a reactive range to slider
                   shiny::updateSliderInput(session, "x_limit", min = 1,  max = 500, step = 10, value = 1 )
                 } else
                 {
                   shiny::updateSliderInput(session, "x_limit", min = as.Date("2020-01-22","%Y-%m-%d"),  max = Sys.Date(), value = as.Date("2020-02-01","%Y-%m-%d") )
                 }
               }) #end observe event  
  
  #watch the choice for the x-scale and choose what to show underneath accordingly
  observeEvent(input$xscale_w,
               {
                 if (input$xscale_w == 'x_count')
                 {
                   #Add a reactive range to slider
                   shiny::updateSliderInput(session, "x_limit_w", min = 1,  max = 500, step = 10, value = 1 )
                 } else
                 {
                   shiny::updateSliderInput(session, "x_limit_w", min = as.Date("2020-01-22","%Y-%m-%d"),  max = Sys.Date(), value = as.Date("2020-02-01","%Y-%m-%d") )
                 }
               }) #end observe event 
  
  #watch the choice for the x-scale and choose what to show underneath accordingly
  observeEvent(input$xscale_c,
               {
                 if (input$xscale_c == 'x_count')
                 {
                   #Add a reactive range to slider
                   shiny::updateSliderInput(session, "x_limit_c", min = 1,  max = 500, step = 10, value = 1 )
                 } else
                 {
                   shiny::updateSliderInput(session, "x_limit_c", min = as.Date("2020-01-22","%Y-%m-%d"),  max = Sys.Date(), value = as.Date("2020-02-01","%Y-%m-%d") )
                 }
               }) #end observe event  
  
  #watch state_selector_c to reduce the picker options in the county dropdown limited to those within the selected state(s)
  observeEvent(input$state_selector_c,
               {
                  #redesignate county_dat to match the state selector input
                   county_dat_sub <- county_dat %>% filter(state %in% input$state_selector_c)
                   county_var_sub = sort(unique(county_dat_sub$location))
                   shinyWidgets::updatePickerInput(session, "county_selector", "Select counties", county_var_sub, selected = county_var_sub[1])
             })
  

  ###########################################
  # function that takes data generated by above function and makes plots
  # uses plotly
  ###########################################
  make_plotly <- function(all_plot_dat, location_selector, source_selector,case_death, daily_tot,
                              xscale, yscale, absolute_scaled, x_limit, current_tab,  
                              show_smoother, ylabel, outtype)
  {

    #outcome to plot/process for non-test
    outcome = paste(daily_tot,case_death,sep='_') #make string from UI inputs that correspond with variable names
    
    if (outtype == "Test_All") 
    {
      outcome = paste(daily_tot,outtype,sep="_")
    }
    if (outtype == "Positive_Prop") 
    {
      outcome = paste(daily_tot,outtype,sep="_") 
    }

    
    if (current_tab == "county")
    {
      #add an additional line of filtering when using the county tab to prevent double stacking of data from counties that share the same name in multiple states
      #sort remaining data as done for us and world plots
      plot_dat <- county_dat %>% filter(state %in% input$state_selector_c) %>%
                                   filter(location %in% location_selector) %>%      
                                   filter(source %in% source_selector) %>%
                                   group_by(source,location) %>%
                                   arrange(date) %>%
                                   ungroup()
    }
    else
    {
      #filter data based on user selections
      #keep all outcomes/variables for now so we can do x-axis adjustment
      #filtering of only the outcome to plot is done after x-scale adjustment
      plot_dat <- all_plot_dat %>%   filter(location %in% location_selector) %>%      
                                     filter(source %in% source_selector) %>%
                                     group_by(source,location) %>%
                                     arrange(date) %>%
                                     ungroup()
    }

    
    #adjust x-axis as needed 
    if (xscale == 'x_count')
    {
      #filter by count limit
      out_type2 = paste0("Total_",case_death) #make string from UI inputs that correspond to total and selected outcome
      start_dates <- plot_dat %>% 
        filter(variable == out_type2) %>% #get the quantity (cases/hosp/death) which is used to define start
        filter( value >= x_limit) %>% #remove all values that are below threshold
        group_by(source,location) %>%   #group by states
        summarize(start_date = first(date))   #get first date for each state after filtering. for this to work right, the data needs to be sorted by date for each source/location combination
      
      plot_dat <-  plot_dat %>% left_join(start_dates, by = c("source", "location")) %>% #add start dates to data
                   filter(variable %in% outcome) %>% #retain only outcome variable
                   filter(date >= start_date)  %>%      
                   mutate(time = as.numeric(date)) %>%
                   group_by(source, location) %>% 
                   mutate(time = time - min(time)) %>%
                   ungroup()
    }
    else
    {
      #filter by date limit
      plot_dat <- plot_dat %>% mutate(time = date) %>%
                  filter(variable %in% outcome) %>%
                  filter(date >= x_limit) 
    }
    
     
    
    
    #set labels and tool tips based on input - entries 2 and 3 are ignored for world plot
    y_labels <- c("Cases", "Tests", "Positive Test Proportion")
    y_labels[1] <- case_death #fill that automatically with either Case/Hosp/Death
    y_labels <- paste(daily_tot, y_labels, sep = " ")
    
    tool_tip <- c("Date", "Cases", "Tests", "Positive Test Proportion")
    tool_tip[2] <- case_death #fill that automatically with either Case/Hosp/Death
    
       
    #if we want scaling by 100K, do extra scaling 
    # don't apply to test proportion
    if ((absolute_scaled == 'scaled') && (outtype != "Positive_Prop"))
    {
      plot_dat <- plot_dat %>% mutate(value = value / populationsize * 100000) 
      y_labels[1] <- paste0(y_labels[1], " per 100K")
      y_labels[2] <- paste0(y_labels[2], " per 100K")
    } #end scaling function
     
    

    p_dat <- plot_dat
    #the US test plots can only be created using the COVIDtracking data
    if (current_tab == "us" && (outtype == "Test_All" || outtype == "Positive_Prop")) 
    {
      p_dat <- plot_dat %>% filter(source == "COVIDTracking")
      
    }
    #the world test plots can only be created using the OWID data
    if (current_tab == "world" && (outtype == "Test_All" || outtype == "Positive_Prop"))
    {
      p_dat <- plot_dat %>% filter(source == "OWID")
    }
       
    linesize = 1.5
    ncols = max(3,length(unique(p_dat$location))) #number of colors for plotting
    
    # create text to show in hover-over tooltip
    tooltip_text = paste(paste0("Location: ", p_dat$location), 
                         paste0(tool_tip[1], ": ", p_dat$date), 
                         paste0(tool_tip[ylabel+1],": ", outcome, sep ="\n")) 
    
    # make plot
    pl <- plotly::plot_ly(p_dat) %>% 
          plotly::add_trace(x = ~time, y = ~value, type = 'scatter', 
                                 mode = 'lines+markers', 
                                 linetype = ~source, symbol = ~location,
                                 line = list(width = linesize), text = tooltip_text, 
                                 color = ~location, colors = brewer.pal(ncols, "Dark2")) %>%
                          layout(yaxis = list(title=y_labels[ylabel], type = yscale, size = 18)) %>%
                          layout(legend = list(orientation = "h", x = 0.2, y = -0.3))

    # if requested by user, apply and show a smoothing function 
    if (show_smoother == "Yes")
    #if (outname == "outcome" && show_smoother == "Yes")
    {
      if (any(location_selector %in% p_dat$location))
      {
        p_dat2 <- p_dat  %>% select(location,source,value,time) %>% drop_na() %>%
          group_by(location) %>%
          filter(n() >= 2) %>%  
          mutate(smoother = loess(value ~ as.numeric(time), span = .4)$fitted) %>%    
          ungroup()
        
        pl <- pl %>% plotly::add_lines(x = ~time, y = ~smoother, 
                                       color = ~location, data = p_dat2, 
                                       line = list( width = 2*linesize),
                                       opacity=0.3,
                                       showlegend = FALSE) 
     }
     else
     {
        stop(safeError("Please select a different data source or location. The selected location(s) is not present in the chosen source"))
     }
    } #end smoother if statement
    return(pl)
  }
  
  ###########################################
  #function that makes case/death plot for US tab
  ###########################################
  output$case_death_plot <- renderPlotly({
    pl <- NULL
    if (!is.null(input$source_selector))
    {
    #create plot
    pl <- make_plotly(us_dat, input$state_selector, input$source_selector, input$case_death, input$daily_tot,
                              input$xscale, input$yscale, input$absolute_scaled, input$x_limit, input$current_tab,
                              input$show_smoother, ylabel = 1, outtype = '')
    }
    return(pl)
  }) #end function making case/deaths plot

  ###########################################
  #function that makes testing plot for US tab
  ###########################################
  output$testing_plot <- renderPlotly({
    pl <- NULL
    if ('COVIDTracking' %in% input$source_selector)
    {
      pl <- make_plotly(us_dat, input$state_selector, input$source_selector, input$case_death, input$daily_tot,
                        input$xscale, input$yscale, input$absolute_scaled, input$x_limit, input$current_tab,
                        input$show_smoother, ylabel = 2, outtype = 'Test_All')
    }
    return(pl)
  }) #end function making testing plot
  
  ###########################################
  #function that makes testing positive fraction plot for US tab
  ###########################################
  output$testing_frac_plot <- renderPlotly({
      pl <- NULL
      if ('COVIDTracking' %in% input$source_selector)
      {
        pl <- make_plotly(us_dat, input$state_selector, input$source_selector, input$case_death, input$daily_tot,
                          input$xscale, input$yscale, input$absolute_scaled, input$x_limit, input$current_tab,
                          input$show_smoother, ylabel = 3, outtype = 'Positive_Prop')
        
      }
      return(pl)
      }) #end function making testing plot

  ###########################################
  #function that makes case/death  for world tab
  ###########################################
  output$world_case_death_plot <- renderPlotly({
    pl <- NULL
    if (!is.null(input$source_selector_w))
    {
      pl <- make_plotly(world_dat, input$country_selector, input$source_selector_w, input$case_death_w, input$daily_tot_w,
                        input$xscale_w, input$yscale_w, input$absolute_scaled_w, input$x_limit_w, input$current_tab,
                        input$show_smoother_w, ylabel = 1, outtype = '')
    }
    return(pl)
  }) #end function making case/deaths plot
  
  ###########################################
  #function that makes testing plot for world tab
  ###########################################
  output$world_testing_plot <- renderPlotly({
    pl <- NULL
    if ('OWID' %in% input$source_selector_w)
    {
      #create plot
      pl <- make_plotly(world_dat, input$country_selector, input$source_selector_w, input$case_death_w, input$daily_tot_w,
                        input$xscale_w, input$yscale_w, input$absolute_scaled_w, input$x_limit_w, input$current_tab,
                        input$show_smoother_w, ylabel = 2, outtype = 'Test_All')
    }
    return(pl)
  }) #end function making testing plot
  
  
  ###########################################
  #function that makes testing positive fraction plot for world tab
  ###########################################
  output$world_testing_frac_plot <- renderPlotly({
    pl <- NULL
    if ('OWID' %in% input$source_selector_w)
    {
      pl <- make_plotly(world_dat, input$country_selector, input$source_selector_w, input$case_death_w, input$daily_tot_w,
                        input$xscale_w, input$yscale_w, input$absolute_scaled_w, input$x_limit_w, input$current_tab,
                        input$show_smoother_w, ylabel = 3, outtype = 'Positive_Prop')
    }
    return(pl)
  }) #end function making testing plot

  ###########################################
  #function that makes case/death  for county tab
  ###########################################
  output$county_case_death_plot <- renderPlotly({
    #JHU data only
    pl <- make_plotly(county_dat, input$county_selector, "JHU", input$case_death_c, input$daily_tot_c, "x_time", input$yscale_c, input$absolute_scaled_c, input$x_limit_c, input$current_tab, input$show_smoother_c, ylabel = 1, outtype = '')
    
    #this code is if we enable different sources for counties. currently disabled.
        #pl <- NULL
    #if (!is.null(input$source_selector_c))
    #{
      #this plot does not give the option to start at N cases/deaths, seems useless
#      pl <- make_plotly(county_dat, input$county_selector, input$source_selector_c, input$case_death_c, input$daily_tot_c, "x_time", input$yscale_c, input$absolute_scaled_c, input$x_limit_c, input$current_tab, input$show_smoother_c, ylabel = 1, outtype = '')
    #}
    
    return(pl)
  }) #end function making case/deaths plot
  
} #end server function

# Create Shiny object
shinyApp(ui = ui, server = server)