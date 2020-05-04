# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(here)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(tidyselect)

#prevent shiny from overwriting our error message
#not used right now, using safeError below instead
#options(shiny.sanitize.errors = FALSE)

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
     all_data <- readRDS(filename)    
     return(all_data)  
  }
  #if data file is not here, go through all of the below
  
  #data for population size for each state/country so we can compute cases per 100K
  us_popsize <- readRDS(here("data","us_popsize.rds")) %>% rename(state_abr = state, state = state_full)
  world_popsize <-readRDS(here("data","world_popsize.rds")) 
  
  all_data = list() #will save and return all datasets as list
  
  #################################
  # pull data from Covidtracking and process
  #################################
  us_ct_data <- read_csv("https://covidtracking.com/api/v1/states/daily.csv")
  us_ct_clean <- us_ct_data %>% dplyr::select(c(date,state,positive,negative,total,hospitalized,death)) %>%
    mutate(date = as.Date(as.character(date),format="%Y%m%d")) %>% 
    group_by(state) %>% arrange(date) %>%
    mutate(Daily_Test_Positive = c(0,diff(positive))) %>% 
    mutate(Daily_Test_Negative = c(0,diff(negative))) %>% 
    mutate(Daily_Test_All = c(0,diff(total))) %>% 
    mutate(Daily_Hospitalized = c(0,diff(hospitalized))) %>% 
    mutate(Daily_Deaths = c(0,diff(death))) %>% rename(state_abr = state) %>%
    merge(us_popsize) %>%
    rename(Date = date, Location = state, Population_Size = total_pop, Total_Deaths = death, 
           Total_Cases = positive, Total_Hospitalized = hospitalized, 
           Total_Test_Negative = negative, Total_Test_Positive = positive, Total_Test_All = total) %>%
    mutate(Daily_Cases = Daily_Test_Positive, Total_Cases = Total_Test_Positive) %>%
    select(-c(state_abr,Total_Test_Negative,Daily_Test_Negative))
  
  #add all US by summing over all variables
  all_us <- us_ct_clean %>% group_by(Date) %>% summarize_if(is.numeric, sum, na.rm=TRUE)
  all_us$Location = "US"
  all_us$Population_Size = max(all_us$Population_Size) #because of na.rm in sum, pop size only right at end
  us_ct_clean = rbind(us_ct_clean,all_us)
  
  #################################
  # pull data from NYT and process
  #################################
  us_nyt_data <- readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  us_nyt_clean <- us_nyt_data %>% dplyr::select(c(date,state,cases,deaths)) %>%
    group_by(state) %>% arrange(date) %>%
    mutate(Daily_Cases = c(0,diff(cases))) %>% 
    mutate(Daily_Deaths = c(0,diff(deaths))) %>%
    merge(us_popsize) %>%
    rename(Date = date, Location = state, Population_Size = total_pop, Total_Deaths = deaths, 
           Total_Cases = cases)  %>%
    select(-state_abr)

  #add all US by summing over all variables
  all_us <- us_nyt_clean %>% group_by(Date) %>% summarize_if(is.numeric, sum, na.rm=TRUE)
  all_us$Location = "US"
  all_us$Population_Size = max(all_us$Population_Size) #because of na.rm in sum, pop size only right at end
  us_nyt_clean = rbind(us_nyt_clean,all_us)

  #################################
  # pull data from USAFacts and process
  #################################
  usafct_case_data <- readr::read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv")
  state_df = usafct_case_data %>% distinct(stateFIPS, .keep_all = TRUE) %>% select(3:4)
  usafct_case_clean <- usafct_case_data %>% 
                       dplyr::group_by(stateFIPS) %>% 
                       summarize_if(is.numeric, sum, na.rm=TRUE) %>%
                       dplyr::select(-countyFIPS) %>% 
                       left_join(state_df) %>%    
    tidyr::pivot_longer(-State, names_to = "Date", values_to = "Total_Cases") %>%
    mutate(Date = as.Date(Date,format="%m/%d/%y")) %>% 
    group_by(State) %>% arrange(Date) %>%
    mutate(Daily_Cases = c(0,diff(Total_Cases))) %>% 
    rename(state_abr = State) %>%
    merge(us_popsize) %>%
    rename(Location = state, Population_Size = total_pop) %>%
    select(-c(state_abr))
  
  usafct_death_data <- readr::read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv")
  state_df = usafct_death_data %>% distinct(stateFIPS, .keep_all = TRUE) %>% select(3:4)
  usafct_death_clean <- usafct_death_data %>% 
    dplyr::group_by(stateFIPS) %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE) %>%
    dplyr::select(-countyFIPS) %>% 
    left_join(state_df) %>%    
    tidyr::pivot_longer(-State, names_to = "Date", values_to = "Total_Deaths") %>%
    mutate(Date = as.Date(Date,format="%m/%d/%y")) %>% 
    group_by(State) %>% arrange(Date) %>%
    mutate(Daily_Deaths = c(0,diff(Total_Deaths))) %>% 
    rename(state_abr = State) %>%
    merge(us_popsize) %>%
    rename(Location = state, Population_Size = total_pop) %>%
    select(-state_abr, -Population_Size)
  
  usafct_clean <- left_join(usafct_case_clean, usafct_death_clean) %>%
                  group_by(Location) %>% arrange(Date)  %>%
                  ungroup() %>%
                  data.frame()
    
  #add all US by summing over all variables
  all_us <- usafct_clean %>% group_by(Date) %>% summarize_if(is.numeric, sum, na.rm=TRUE)
  all_us$Location = "US"
  all_us$Population_Size = max(all_us$Population_Size) #because of na.rm in sum, pop size only right at end
  usafct_clean = rbind(usafct_clean,all_us)
  
  
  #################################
  # pull US data from JHU github and process
  #################################
  us_jhu_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
  us_jhu_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
  # Clean cases
  us_jhu_cases <- us_jhu_cases %>% filter(iso3 == "USA") %>%
    dplyr::select(c(-Country_Region, -Lat, -Long_, -UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Combined_Key)) %>%
    rename(Location = Province_State)
  us_jhu_cases <- aggregate(. ~ Location, us_jhu_cases, FUN = sum)
  us_jhu_cases_clean <- gather(us_jhu_cases, Date, Cases, -Location)
  # Clean deaths
  us_jhu_deaths <- us_jhu_deaths %>% filter(iso3 == "USA") %>%
    dplyr::select(c(-Country_Region, -Lat, -Long_, -UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Combined_Key, -Population)) %>%
    rename(Location = Province_State)
  us_jhu_deaths <- aggregate(. ~ Location, us_jhu_deaths, FUN = sum)
  us_jhu_deaths_clean <- gather(us_jhu_deaths, Date, Deaths, -Location)
  us_jhu_combined <- merge(us_jhu_cases_clean, us_jhu_deaths_clean)
  us_jhu_popsize <- us_popsize %>% rename(Location = state)
  # This merge removes cruise ship cases/death counts
  us_jhu_merge <- merge(us_jhu_combined, us_jhu_popsize)
  us_jhu_clean <- us_jhu_merge %>% mutate(Date = as.Date(as.character(Date),format="%m/%d/%y")) %>%
    group_by(Location) %>% arrange(Date) %>%
    mutate(Daily_Cases = c(0,diff(Cases))) %>%
    mutate(Daily_Deaths = c(0,diff(Deaths))) %>% 
    ungroup() %>%
    rename(Total_Deaths = Deaths, Total_Cases = Cases, Population_Size = total_pop) %>% 
    select(-state_abr) %>%
    data.frame()
  
  #add all US by summing over all variables
  all_us <- us_jhu_clean %>% group_by(Date) %>% summarize_if(is.numeric, sum, na.rm=TRUE)
  all_us$Location = "US"
  all_us$Population_Size = max(all_us$Population_Size) #because of na.rm in sum, pop size only right at end
  us_jhu_clean = rbind(us_jhu_clean,all_us)
  
    
  #################################
  # pull world data from JHU github and process
  #################################
  world_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  world_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  # clean the data for plotting
  world_cases <- world_cases %>% dplyr::select(c(-`Province/State`, -Lat, -Long)) %>%
    rename(country= `Country/Region`)
  world_cases <- aggregate(. ~ country, world_cases, FUN = sum)
  world_deaths <- world_deaths %>% dplyr::select(c(-`Province/State`, -Lat, -Long)) %>%
    rename(country= `Country/Region`)
  world_deaths <- aggregate(. ~ country, world_deaths, FUN = sum)
  #Melt case and death data
  world_cases <- merge(world_popsize, world_cases)
  melt_cases <- gather(world_cases, date, cases, -country, -country_pop)
  world_deaths <- merge(world_deaths, world_popsize)
  melt_deaths <- gather(world_deaths, date, deaths, -country, -country_pop)
  all_merge <- merge(melt_deaths, melt_cases)
  world_jhu_clean <- all_merge %>% mutate(date = as.Date(as.character(date),format="%m/%d/%y")) %>%
    group_by(country) %>% arrange(date) %>%
    mutate(Daily_Cases = c(0,diff(cases))) %>%
    mutate(Daily_Deaths = c(0,diff(deaths))) %>% 
    ungroup() %>%
    rename(Date = date, Total_Deaths = deaths, Total_Cases = cases, Location = country, Population_Size = country_pop) %>% 
    mutate(Location = replace(Location, Location == 'Korea, South','South Korea')) %>%
    mutate(Location = replace(Location, Location == 'Taiwan*','Taiwan')) %>% #fix a few locations
    data.frame()
  
  
  #################################
  # pull world data from OWID github and process
  #################################
  owid_data <- readr::read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
  world_owid_clean <- owid_data %>% dplyr::select(-tests_units, -iso_code) %>%
    rename(Total_Cases = total_cases, Total_Deaths = total_deaths, Daily_Cases = new_cases, Daily_Deaths = new_deaths, Location = location, Date = date, Daily_Test_All = new_tests, Total_Test_All = total_tests) %>% 
    mutate(Population_Size = Total_Cases / total_cases_per_million * 1000000) %>% #back-calculate population size
    mutate(Location = recode(Location, "United States" = "US")) %>%
    mutate(Daily_Test_Positive = Daily_Cases ) %>% #assuming new cases means new positive tests
    mutate(Total_Test_Positive = Total_Cases ) %>%
    select( - contains('thousand'), - contains('million')) %>%
    data.frame()
 
  
  #################################
  # combine all data 
  #################################

  # give each US dataset a source label
  us_source_var = c("COVIDTracking","NYTimes","JHU","USAFacts")
  
  us_ct_clean$source = us_source_var[1]
  us_nyt_clean$source = us_source_var[2]
  us_jhu_clean$source = us_source_var[3]
  usafct_clean$source = us_source_var[4]
  
  #combine all US data from different sources
  us_dat <- dplyr::bind_rows(us_ct_clean, us_nyt_clean, us_jhu_clean, usafct_clean) 
  
  
  # give each world dataset a source label
  world_source_var = c("JHU", "OWID")
  
  world_jhu_clean$source = world_source_var[1]
  world_owid_clean$source = world_source_var[2]
  
  #combine all world data from different sources
  world_dat <- dplyr::bind_rows(world_jhu_clean, world_owid_clean)
  
  #combine data in list  
  all_data$us_dat = us_dat
  all_data$world_dat = world_dat
  
  #save the data
  saveRDS(all_data, filename)    
  return(all_data)
  
} # end the get-data function which pulls data from the various online sources and processes/saves  

###########################################
# function that re-reads the data every so often
###########################################
all_data <- reactivePoll(intervalMillis = 1000*60*60*12, # pull new data every 12 hours
                         session = NULL,
                         checkFunc = function() {Sys.time()}, #this will always return a different value, which means at intervals specified by intervalMillis the new data will be pulled
                         valueFunc = function() {get_data()} )

#read data is reactive, doesn't work for rest below 
all_dat = isolate(all_data())

# pull data out of list 
world_dat = all_dat$world_dat 
us_dat = all_dat$us_dat 


#define variables for location and source selectors
state_var = unique(us_dat$Location)
country_var = sort(unique(world_dat$Location))

us_source_var = unique(us_dat$source)
world_source_var = unique(world_dat$source)


#################################
# Define UI
#################################
ui <- fluidPage(
  tags$head(includeHTML(here("www","google-analytics.html"))), #this is for Google analytics tracking.
  includeCSS(here("www","appstyle.css")),
  #main tabs
  navbarPage( title = "COVID-19 Tracker", id = 'alltabs', selected = "us", header = "",
              tabPanel(title = "US", value = "us",
                       sidebarLayout(
                         sidebarPanel(
                           shinyWidgets::pickerInput("state_selector", "Select State(s)", state_var, multiple = TRUE,options = list(`actions-box` = TRUE), selected = c("Georgia","California","Washington") ),
                           shiny::div("US is at end of state list."),
                           br(),
                           shinyWidgets::pickerInput("us_source_selector", "Select Source(s)", us_source_var, multiple = TRUE,options = list(`actions-box` = TRUE), selected = c("COVIDTracking") ),
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
                           sliderInput(  inputId = "count_limit", "Choose the number of cases/hospitalizations/deaths at which to start graphs", min = 1,  max = 500, value = 10 ),
                           shiny::div("Modify all three plots to show data with x-axis as calender date or days since a state reported a specified total number of cases/hospitalizations/deaths, specified by the slider above. The slider above does not have an impact for calendar date on the x-axis."),
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
              
              tabPanel( title = "World", value = "world",
                        sidebarLayout(
                          sidebarPanel(
                            #Country selector coding with US, Italy, and Spain as always selected for a defult setting, will flash an error with none selected
                            shinyWidgets::pickerInput("country_selector", "Select countries", country_var,  multiple = TRUE, options = list(`actions-box` = TRUE), selected = c("US", "Italy", "Spain")                        ),
                            shinyWidgets::pickerInput("world_source_selector", "Select Source(s)", world_source_var, multiple = TRUE,options = list(`actions-box` = TRUE), selected = c("JHU") ),
                            shiny::div("Choose data sources (see 'About' tab for details)."),
                            br(),
                            shiny::selectInput( "case_death_w", "Outcome", c("Cases" = "Cases", "Deaths" = "Deaths")),
                            shiny::div("Modify the plot to display cases or deaths."),
                            br(),
                            shiny::selectInput("daily_tot_w", "Daily or cumulative numbers", c("Daily" = "Daily", "Total" = "Total")),
                            shiny::div("Modify the plot to reflect daily or cumulative data."),
                            br(),
                            shiny::selectInput("show_smoother_world", "Add trend line", c("No" = "No", "Yes" = "Yes")),
                            shiny::div("Shows a trend line for cases/hospitalizations/deaths plot."),
                            br(),
                            shiny::selectInput("absolute_scaled_w", "Absolute or scaled values", c("Absolute Number" = "actual", "Per 100,000 persons" = "scaled") ),
                            shiny::div("Modify the plot to display total counts or values scaled by the country population size."),
                            br(),
                            shiny::selectInput("xscale_w", "Set x-axis to calendar date or days since a specified total number of cases/deaths", c("Calendar Date" = "x_time", "Days since N cases/hospitalizations/deaths" = "x_count")),
                            sliderInput(  inputId = "count_limit_w", "Choose the number of cases/deaths at which to start graphs", min = 1,  max = 500, value = 10 ),
                            shiny::div("Modify all the plot to show data with x-axis as calender date or days since a country reported a specified total number of cases/deaths, specified by the slider above. The slider above does not have an impact for calendar date on the x-axis."),
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
                              "We currently include 4 different data sources for the US.", 
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

 
  
  ###########################################
  # function that takes UI settings and produces data for each plot
  ###########################################
  set_outcome <- function(all_plot_dat,case_death,daily_tot,absolute_scaled,xscale,count_limit,selected_tab,location_selector,source_selector)
  {
    
    out_type = paste(daily_tot,case_death,sep='_') #make string from UI inputs that correspond with variable names
    plot_dat <- all_plot_dat %>%   filter(Location %in% location_selector) %>%      #Only process data for locations that are  selected
                               filter(source %in% source_selector) %>%
                                mutate(outcome = get(out_type)) #pick output based on variable name created from UI
    # do testing data for US

    if ( ('COVIDTracking' %in% source_selector) || ('OWID' %in% source_selector) )  
    {
      
      test_out_type = paste(daily_tot,'Test_All',sep='_')
      test_pos_type = paste(daily_tot,'Test_Positive',sep='_')
      plot_dat <- plot_dat %>% mutate(test_outcome = get(test_out_type)) 
      plot_dat <- plot_dat %>% mutate(test_frac_outcome = get(test_pos_type)/get(test_out_type))
    }
 
    #set labels and tool tips based on input - entries 2 and 3 are ignored for world plot
    y_labels <- c("Cases", "Tests", "Positive Test Proportion")
    y_labels[1] <- case_death #fill that automatically with either Case/Hosp/Death
    y_labels <- paste(daily_tot, y_labels, sep = " ")
    
    tool_tip <- c("Date", "Cases", "Tests", "Positive Test Proportion")
    tool_tip[2] <- case_death #fill that automatically with either Case/Hosp/Death
    
       
    #if we want scaling by 100K, do extra scaling 
    if (absolute_scaled == 'scaled')
    {
      plot_dat <- plot_dat %>% mutate(outcome = outcome / Population_Size * 100000) 
      y_labels[1] <- paste0(y_labels[1], " per 100K")
      y_labels[2] <- paste0(y_labels[2], " per 100K")
    
      if ( ('COVIDTracking' %in% source_selector) || ('OWID' %in% source_selector) )  
      {
          plot_dat <- plot_dat %>%  mutate(test_outcome = test_outcome / Population_Size * 100000)
      }
    } #end scaling function
     
    #adjust data to align for plotting by cases on x-axis. 
    if (xscale == 'x_count')
    {
      #Takes plot_dat and filters counts by the predetermined count limit from the reactive above
      #Created the time variable (which represents the day number of the outbreak) from the date variable
      #Will plot the number of days since the selected count_limit or the date
      
      out_type2 = paste0("Total_",case_death) #make string from UI inputs that correspond to total and selected outcome
      plot_dat <- plot_dat %>% 
        filter(get(out_type2) >= count_limit) %>%  
        mutate(Time = as.numeric(Date)) %>%
        group_by(Location) %>% 
        mutate(Time = Time - min(Time))
      
    }
    else
    {
      plot_dat <- plot_dat %>% mutate(Time = Date)
    }
    #sort dates for plotting
    plot_dat <- plot_dat %>% group_by(Location) %>% arrange(Time) %>% ungroup()
    
    list(plot_dat, y_labels, tool_tip) #return list
  } #end function that produces output for plots
  
  ###########################################
  # function that takes data generated by above function and makes plots
  # uses plotly
  ###########################################
  make_plotly <- function(plot_list, location_selector, yscale, xscale, ylabel, outname, selected_tab, show_smoother)
  {
    tool_tip <- plot_list[[3]]
    plot_dat <- data.frame(plot_list[[1]]) #need the extra data frame conversion from tibble to get tooltip_text line to work
    linesize = 1.5
    
    p_dat <- plot_dat 
    if (selected_tab == "us" && (outname == 'test_outcome' || outname == 'test_frac_outcome'))
    {
      p_dat <- plot_dat %>% filter(source == "COVIDTracking")
      
    }
    if (selected_tab == "world" && (outname == 'test_outcome' || outname == 'test_frac_outcome'))
    {
      p_dat <- plot_dat %>% filter(source == "OWID")
      
    }
    
    #fit <- loess(apple_data$raw_value ~ apple_data$time, degree=1, span = 0.3, data=apple_data)
    #apple_data <- apple_data %>%    mutate(rel_beta_change = fit$fitted)
    
    ncols = max(3,length(unique(p_dat$Location)))
    tooltip_text = paste(paste0("Location: ", p_dat$Location), 
                         paste0(tool_tip[1], ": ", p_dat$Date), 
                         paste0(tool_tip[ylabel+1],": ", p_dat[,outname]), sep ="\n") 
    pl <- plotly::plot_ly(p_dat) %>% 
          plotly::add_trace(x = ~Time, y = ~get(outname), type = 'scatter', 
                                 mode = 'lines+markers', 
                                 linetype = ~source, symbol = ~Location,
                                 line = list(width = linesize), text = tooltip_text, 
                                 color = ~Location, colors = brewer.pal(ncols, "Dark2")) %>%
                                 layout(yaxis = list(title=plot_list[[2]][ylabel], type = yscale, size = 18)) 

    # if requested by user, apply and show a smoothing function for case/hosp/death data
    # currently working for cases but creates error messages for rest
    if (outname == "outcome" && show_smoother == "Yes")
    {
      
      if (any(location_selector %in% p_dat$Location))
      {
        p_dat2 <- p_dat  %>% select(Location,source,outcome,Time)%>% drop_na() %>%
          group_by(Location) %>%
          filter(n() >= 2) %>%  
          mutate(smoother = loess(outcome ~ as.numeric(Time), span = .4)$fitted) %>%    
          ungroup()
        
        
        pl <- pl %>% plotly::add_lines(x = ~Time, y = ~smoother, 
                                       color = ~Location, data = p_dat2, 
                                       line = list( width = 2*linesize),
                                       opacity=0.3,
                                       showlegend = FALSE) 
      }
      else
      {

      stop(safeError("Please select a different data source or location. The selected location(s) is not present in the chosen source"))
      
      }

    }
    return(pl)
  }
  
  ###########################################
  #function that preps data for US tab
  ###########################################
  plot_dat_us <- reactive({
    set_outcome(us_dat,input$case_death,input$daily_tot,input$absolute_scaled,input$xscale,input$count_limit,input$alltabs,input$state_selector,input$us_source_selector)
  })
  
  ###########################################
  #function that preps data for world tab
  ###########################################
  plot_dat_world <- reactive({
    set_outcome(world_dat,input$case_death_w,input$daily_tot_w,input$absolute_scaled_w,input$xscale_w,input$count_limit_w,input$alltabs,input$country_selector,input$world_source_selector)
  })
  
  ###########################################
  #function that makes case/death plot for US tab
  ###########################################
  output$case_death_plot <- renderPlotly({
    pl <- NULL
    if (!is.null(input$us_source_selector))
    {
    #create plot
    pl <- make_plotly(plot_dat_us(), location_selector = input$state_selector, yscale = input$yscale, xscale = input$xscale, ylabel = 1, outname = 'outcome',selected_tab = input$alltabs, show_smoother = input$show_smoother)
    }
    return(pl)
  }) #end function making case/deaths plot

  ###########################################
  #function that makes testing plot for US tab
  ###########################################
  output$testing_plot <- renderPlotly({
    pl <- NULL
    if ('COVIDTracking' %in% input$us_source_selector)
    {
      #create plot
    pl <- make_plotly(plot_dat_us(), location_selector = input$state_selector, yscale = input$yscale, xscale = input$xscale, ylabel = 2, outname = 'test_outcome',selected_tab = input$alltabs, show_smoother = "no")
    }
    return(pl)
  }) #end function making testing plot
  
  ###########################################
  #function that makes testing positive fraction plot for US tab
  ###########################################
  output$testing_frac_plot <- renderPlotly({
      pl <- NULL
      if ('COVIDTracking' %in% input$us_source_selector)
      {
        #create plot
        pl <- make_plotly(plot_dat_us(), location_selector = input$state_selector, yscale = "identity", xscale = input$xscale, ylabel = 3, outname = 'test_frac_outcome',selected_tab = input$alltabs, show_smoother = "no")
      }
      return(pl)
      }) #end function making testing plot

  ###########################################
  #function that makes case/death  for world tab
  ###########################################
  output$world_case_death_plot <- renderPlotly({
    pl <- NULL
    if (!is.null(input$world_source_selector))
    {
      #create plot
      pl <- make_plotly(plot_dat_world(), location_selector = input$country_selector, yscale = input$yscale_w, xscale = input$xscale_w, ylabel = 1, outname = 'outcome', selected_tab = input$alltabs, show_smoother = input$show_smoother_world)
    }
    return(pl)
  }) #end function making case/deaths plot
  
  ###########################################
  #function that makes testing plot for world tab
  ###########################################
  output$world_testing_plot <- renderPlotly({
    pl <- NULL
    if ('OWID' %in% input$world_source_selector)
    {
      #create plot
      pl <- make_plotly(plot_dat_world(), location_selector = input$country_selector, yscale = input$yscale_w, xscale = input$xscale_w, ylabel = 2, outname = 'test_outcome', selected_tab = input$alltabs, show_smoother = "no")
    }
    return(pl)
  }) #end function making testing plot
  
  
  ###########################################
  #function that makes testing positive fraction plot for world tab
  ###########################################
  output$world_testing_frac_plot <- renderPlotly({
    pl <- NULL
    if ('OWID' %in% input$world_source_selector)
    {
      #create plot
      pl <- make_plotly(plot_dat_world(), location_selector = input$country_selector, yscale = "identity", xscale = input$xscale_w, ylabel = 3, outname = 'test_frac_outcome', selected_tab = input$alltabs, show_smoother = "no")
    }
    return(pl)
  }) #end function making testing plot

  
} #end server function

# Create Shiny object
shinyApp(ui = ui, server = server)