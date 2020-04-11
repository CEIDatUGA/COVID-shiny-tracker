# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
#library(scales)
#library(shinythemes)
#library(htmlwidgets)
#library(ggthemes)
#library(tibbletime)
#library(directlabels)


#################################
# Load all data
# should be online so things update automatically
#for speed, we only get data from the online source if the data is old, otherwise we load locally
#################################

#################################
#US data from https://covidtracking.com/
filename_us_data = paste0("us-cleandata-",Sys.Date(),'.rds')

if (file.exists(filename_us_data)) {
    #################################
    # load already clean data locally
    #################################
    us_clean <- readRDS(filename_us_data)
} else {
    #################################
    # pull data from Covidtracking and process
    #################################
    us_data <- read_csv("https://covidtracking.com/api/states/daily.csv")
    #data for population size for each state/country so we can compute cases per 100K
    us_popsize <- readRDS("us_popsize.rds")
    us_clean <- us_data %>% dplyr::select(c(date,state,positive,total,hospitalized,death)) %>%
        mutate(date = as.Date(as.character(date),format="%Y%m%d")) %>% 
        group_by(state) %>% arrange(date) %>%
        mutate(Daily_Test_Positive = c(0,diff(positive))) %>% 
        mutate(Daily_Test_All = c(0,diff(total))) %>% 
        mutate(Daily_Hospitalized = c(0,diff(hospitalized))) %>% 
        mutate(Daily_Deaths = c(0,diff(death))) %>%
        merge(us_popsize) %>% select(-state) %>%
        rename(Date = date, Location = state_full, Population_Size = total_pop, Total_Deaths = death, 
              Total_Cases = positive, Total_Hospitalized = hospitalized, 
             Total_Test_Positive = positive, Total_Test_All = total) %>%
      mutate(Daily_Test_Positive_Frac = Daily_Test_Positive/Daily_Test_All) %>%
      mutate(Total_Test_Positive_Frac = Total_Test_Positive/Total_Test_All) %>% 
      mutate(Daily_Cases = Daily_Test_Positive, Total_Cases = Total_Test_Positive) %>% select(-c(Daily_Test_Positive,Total_Test_Positive))
    
    us_clean <- us_clean %>% pivot_longer( cols = -c(Date,Population_Size,Location), names_to = "outcome", values_to = "value" ) %>% mutate(source = 'covidtracker')
    
    saveRDS(us_clean,filename_us_data) #save clean file for loading unless it's outdated
}




#################################
#US data from NY Times
filename_us_nyt = paste0("us-nyt-cleandata-",Sys.Date(),'.rds')

if (file.exists(filename_us_nyt)) {
  #################################
  # load already clean data locally
  #################################
  us_nyt_clean <- readRDS(filename_us_nyt)
} else {
  #################################
  # pull data from Covidtracking and process
  #################################
  us_nyt_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  #data for population size for each state/country so we can compute cases per 100K
  us_popsize <- readRDS("us_popsize.rds") %>% select(-state) %>% rename(state = state_full)
  us_nyt_clean <- us_nyt_data %>% dplyr::select(c(date,state,cases,deaths)) %>%
    #mutate(date = as.Date(as.character(date),format="%Y%m%d")) %>% 
    group_by(state) %>% arrange(date) %>%
    mutate(Daily_Cases = c(0,diff(cases))) %>% 
    mutate(Daily_Deaths = c(0,diff(deaths))) %>%
    merge(us_popsize) %>%
    rename(Date = date, Location = state, Population_Size = total_pop, Total_Deaths = deaths, 
           Total_Cases = cases) 
  
  us_nyt_clean <- us_nyt_clean %>% pivot_longer( cols = -c(Date,Population_Size,Location), names_to = "outcome", values_to = "value" ) %>% 
                                    mutate(source = 'nytimes') %>% 
                                  select(Date, everything())
  saveRDS(us_nyt_clean,filename_us_nyt)
}

all_us_clean = rbind(us_clean,us_nyt_clean)

#################################
#Pull and clean world data
filename_world = paste0("world-cleandata-",Sys.Date(),'.rds')
if (file.exists(filename_world)) {
  #################################
  # load already clean data locally
  #################################
  world_clean <- readRDS(filename_world)
} else {
    #################################
    # pull world data from JHU github and process
    #################################
    world_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
    world_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
    world_popsize <-readRDS("./world_popsize.rds") 
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
    world_clean <- all_merge %>% mutate(date = as.Date(as.character(date),format="%m/%d/%y")) %>%
        group_by(country) %>% arrange(date) %>%
        mutate(Daily_Cases = c(0,diff(cases))) %>%
        mutate(Daily_Deaths = c(0,diff(deaths))) %>% 
        ungroup() %>%
        rename(Date = date, Total_Deaths = deaths, Total_Cases = cases, Location = country, Population_Size = country_pop) %>% 
        data.frame()
    
    world_clean <- world_clean %>% pivot_longer( cols = -c(Date,Population_Size,Location), names_to = "outcome", values_to = "value" ) %>% mutate(source = 'jhu')

    saveRDS(world_clean,filename_world)
}

state_var = unique(all_us_clean$Location)
country_var = unique(world_clean$Location)


#################################
# Define UI
#################################


ui <- fluidPage(
  tags$head(includeHTML(("google-analytics.html"))), #this is for Google analytics tracking.
  includeCSS("appstyle.css"),
    #main tabs
    navbarPage( title = "YACT - Yet Another COVID-19 Tracker", id = 'alltabs', selected = "us", header = "",
        
              tabPanel(title = "US", value = "us",
                 sidebarLayout(
                   sidebarPanel(
                     shinyWidgets::pickerInput("state_selector", "Select states", state_var, multiple = TRUE,options = list(`actions-box` = TRUE), selected = c("Georgia") ),
                     shiny::selectInput( "nyt_data", "Show NYT data",c("Yes" = "Yes", "No" = "No"), selected = "No"),
                     shiny::div("Also show data from NY Times (cases and deaths only)."),
                     br(),
                     shiny::selectInput( "case_death",   "Outcome",c("Cases" = "Cases", "Hospitalizations" = "Hospitalized", "Deaths" = "Deaths")),
                     shiny::div("Modify the top plot to display cases, hospitalizations, or deaths."),
                     br(),
                     shiny::selectInput("daily_tot", "Daily or cumulative numbers", c("Daily" = "Daily", "Total" = "Total" )),
                     shiny::div("Modify all three plots to show daily or cumulative data."),
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
                     br(),
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
                          shiny::selectInput( "case_death_w", "Outcome", c("Cases" = "Cases", "Deaths" = "Deaths")),
                          shiny::div("Modify the plot to display cases or deaths."),
                          br(),
                          shiny::selectInput("daily_tot_w", "Daily or cumulative numbers", c("Daily" = "Daily", "Total" = "Total")),
                          shiny::div("Modify the plot to reflect daily or cumulative data."),
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
                          plotlyOutput(outputId = "case_death_plot_world", height = "500px"),
                        ) #close mainpanel
                      ), #close sidebar layout
                   ), #close world tab
        tabPanel( title = "About", value = "about",
                  tagList(    
                    fluidRow( #all of this is the header
                      #tags$div(id = "shinyheadertitle", "YACT - Yet Another COVID-19 Tracker"),
                      #the style 'shinyheadertitle' is defined in the appstyle.css file
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
                        'or contact Andreas Handel.'
                      ),# and tag
                      tags$div(
                        id = "bigtext",
                        "Underlying data for the US is sourced from",
                        a("The Covid Tracking Project.",  href = "https://covidtracking.com/", target = "_blank" ),
                        "This data source reports all and positive tests, hospitalizations and deaths for each state. We interpret positive tests as corresponding to new cases. For more details on the data, see the Covid Tracking Project website. World data is sourced from the",
                        a("Johns Hopkins University Center for Systems Science and Engineering.", href = "https://github.com/CSSEGISandData/COVID-19", target = "_blank" ),
                        "See their website for more details on the data. Note that some data might not be fully reliable, which can lead to nonsensical graphs (e.g. the fraction of positive tests negative or greater than 1). We make no attempt at cleaning/fixing the data, only use it as provided by those two sources."
                      ),              
                      tags$div(
                        id = "bigtext",
                        a( "The Center for the Ecology of Infectious Diseases", href = "https://ceid.uga.edu", target = "_blank" ),
                        'has several additional projects related to COVID-19, which can be found on the',
                        a( "CEID COVID-19 Portal.", href = "http://2019-coronavirus-tracker.com/", target = "_blank" )
                      ), #Close the bigtext text div
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
  set_outcome <- function(plot_dat,outcomevar,daily_tot,absolute_scaled,xscale,count_limit,selected_tab,location_selector,nytdata)
  {
    
    #adjust data to align for plotting by cases on x-axis. 
    #need to do before filtering since we use totals to adjust
    if (xscale == 'x_count' && outcomevar %in% c("Cases","Hospitalizations","Deaths"))
    {
      #Takes plot_dat and filters numbers by the predetermined count limit from the reactive above
      #Created the time variable (which represents the day number of the outbreak) from the date variable
      #Will plot the number of days since the selected count_limit or the date

      plot_dat <- plot_dat %>%
        dplyr::group_by(Location) %>%
        dplyr::mutate(Time = as.numeric(Date)) %>%

        dplyr::filter(value >= count_limit) %>%


        dplyr::mutate(Time = Time - min(Time))
    }
    else
    {
      plot_dat <- plot_dat %>% mutate(Time = Date)
    }
    
    
    out_type = paste(daily_tot,outcomevar,sep='_') #make string from UI inputs that correspond with variable names
    plot_dat <- plot_dat %>%   filter(Location %in% location_selector) %>%      #Only process data for locations that are  selected
                               filter(outcome == out_type) #pick output based on variable name created from UI
    if (nytdata == "No" && selected_tab == "us")
    {
      plot_dat <- plot_dat %>% filter(source == "covidtracker" ) #remove NYT data from US plots if not selected
    }
    #if we want scaling by 100K, do extra scaling 
    if (absolute_scaled == 'scaled')
    {
      plot_dat <- plot_dat %>% mutate(value = value / Population_Size * 100000) 
    }
  
    #sort dates for plotting
    plot_dat <- plot_dat %>% group_by(Location) %>% arrange(Time) %>% ungroup()
    
    return(data.frame(plot_dat))
  } #end function that produces output for plots
  
  ###########################################
  # function that takes data generated by above function and makes plots
  # uses plotly
  ###########################################
  make_plotly <- function(plot_dat, yscale)
  {
    
    
    #tool_tip <- plot_list[[3]]
    #plot_dat <- data.frame(plot_list[[1]]) #need the extra data frame conversion from tibble to get tooltip_text line to work
    
   linesize = 2
    # if (nytimes == "Yes" && ( outtype == "Cases" || outtype == "Deaths") && outname == "outcome")  #add NYT lines to case/death plot
    # {
    #   p_dat <- dplyr::filter(plot_dat,source == 'Covid Tracker')
    #   tooltip_text = paste(paste0("Location: ", p_dat$Location), paste0(tool_tip[1], ": ", p_dat$Date), paste0(tool_tip[ylabel+1],": ", p_dat[,outname]), sep ="\n")
    #   pl1 <- plotly::plot_ly(p_dat)
    #   pl2 <- plotly::add_trace(pl1, data = p_dat,
    #                            x = ~Time, y = ~get(outname), type = 'scatter', mode = 'lines+markers', color = ~Location,
    #                            line = list( width = linesize), text = tooltip_text)
    # 
    #   p_dat <- dplyr::filter(plot_dat,source == 'NY Times')
    #   tooltip_text = paste(paste0("Location: ", p_dat$Location), paste0(tool_tip[1], ": ", p_dat$Date), paste0(tool_tip[ylabel+1],": ", p_dat[,outname]), sep ="\n")
    #   pl3 <- plotly::add_trace(pl2, data = p_dat,
    #                            x = ~Time, y = ~get(outname), type = 'scatter', mode = 'lines+markers', color = ~Location,
    #                            line = list(dash = "dash", width = linesize), text = tooltip_text) %>%
    #                   layout(yaxis = list(title=plot_list[[2]][ylabel], type = ytrans, size = 18))
    #   pl <- pl3
    #} else {
      p_dat <- plot_dat
      #tooltip_text = paste(paste0("Location: ", p_dat$Location), paste0("Date: ", ": ", p_dat$Date), paste0(p_dat$outcome,": ", p_dat$value), sep ="\n") 
      pl <- plotly::plot_ly(p_dat) %>%  
           add_trace(x = ~Time, y = ~value, type = 'scatter', mode = 'lines+markers', linetype = ~Location, 
                    line = list(color = ~Location, width = linesize)) %>%
          layout(  yaxis = list(title=unique(p_dat$outcome), type = yscale, size = 18)) 
    #}
    return(pl)
  }
  

  ###########################################
  #function that checks if us tab is selected 
  ###########################################
  observeEvent( input$alltabs == 'us', 
  {
    
    #make the plot for cases/deaths for US data
    output$case_death_plot <- renderPlotly({
      #create plot
      plot_dat <- set_outcome(us_clean,outcomevar = input$case_death,input$daily_tot,input$absolute_scaled,input$xscale,input$count_limit,input$alltabs,input$state_selector, input$nyt_data)
      pl <- make_plotly(plot_dat,  yscale = input$yscale)
    }) #end function making case/deaths plot
    
    #make the testing plot 
    output$testing_plot <- renderPlotly({
      plot_dat <- set_outcome(us_clean,outcomevar = "Test_All",input$daily_tot,input$absolute_scaled,input$xscale,input$count_limit,input$alltabs,input$state_selector, input$nyt_data)
      #create plot
      pl <- make_plotly(plot_dat,  yscale = input$yscale)
    }) #end function making testing plot
    
    #make the fraction positive testing plot 
    output$testing_frac_plot <- renderPlotly({
      plot_dat <- set_outcome(us_clean,outcomevar = "Test_Positive_Frac",input$daily_tot,input$absolute_scaled,input$xscale,input$count_limit,input$alltabs,input$state_selector, input$nyt_data)
      #create plot
      pl <- make_plotly(plot_dat,  yscale = input$yscale)
    }) #end function making testing plot
  }) #end observer listening to US tab choice

  
  ###########################################
  #function that checks if world tab is selected and generates UI
  ###########################################
  observeEvent( input$alltabs == 'world', 
  {
        #make the plot for cases/deaths for world data
        output$case_death_plot_world <- renderPlotly({
          plot_dat <- set_outcome(world_clean,input$case_death_w,input$daily_tot_w,input$absolute_scaled_w,input$xscale_w,input$count_limit_w,input$alltabs,input$country_selector,nytdata = "No")
      #create plot
          pl <- make_plotly(plot_dat,  yscale = input$yscale)
        }) #end function making case/deaths plot          
  }) #end world observe event 
 
} #end server function

# Create Shiny object
shinyApp(ui = ui, server = server)