# Load packages
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(shinyWidgets)
library(plotly)
#library(shinythemes)
#library(htmlwidgets)
#library(ggthemes)
#library(tibbletime)
#library(directlabels)

#################################
# Load all data
# should be online so things update automatically
#################################
#US data
#from https://covidtracking.com/

#for speed, we should do it such that it only gets it from the API if the data is old, otherwise it should load locally
if (file.exists('cleandata-us.rds') && as.Date(file.mtime('cleandata-us.rds')) ==  Sys.Date()) {
    #################################
    # load already clean data locally
    #################################
    us_clean <- readRDS('us_cleandata.rds')
} else {
    #################################
    # pull data from Covidtracking and process
    #################################
    us_data <- read_csv("http://covidtracking.com/api/states/daily.csv")
    #data for population size for each state/country so we can compute cases per 100K
    us_popsize <- readRDS("us_popsize.rds")
    
    
    us_clean <- us_data %>% dplyr::select(c(date,state,positive,negative,total,hospitalized,death)) %>%
        mutate(date = as.Date(as.character(date),format="%Y%m%d")) %>% 
        group_by(state) %>% arrange(date) %>%
        mutate(daily_test_positive = c(0,diff(positive))) %>% 
        mutate(daily_test_negative = c(0,diff(negative))) %>% 
        mutate(daily_test_all = c(0,diff(total))) %>% 
        mutate(daily_hospitalized = c(0,diff(hospitalized))) %>% 
        mutate(daily_deaths = c(0,diff(death))) %>%
        merge(us_popsize) %>%
        rename(location = state, pop_size = total_pop, total_deaths = death, total_cases = positive, total_hospitalized = hospitalized, total_test_negative = negative, total_test_positive = positive, total_test_all = total) %>%
       mutate(daily_cases = daily_test_positive, total_cases = total_test_positive)
    
    saveRDS(us_clean,'us_cleandata.rds')
}


#Pull world data
#for speed, we should do it such that it only gets it from the API if the data is old, otherwise it should load locally
if (file.exists('cleandata-world.rds') && as.Date(file.mtime('cleandata-world.rds')) ==  Sys.Date()) {
    #################################
    # load already clean data locally
    #################################
    world_clean <- readRDS('world_cleandata.rds')
} else {
    #################################
    # pull world data from JHU github and process
    #################################
    world_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
    world_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
    
    world_cases <- world_cases %>% dplyr::select(c(-`Province/State`, -Lat, -Long)) %>%
        rename(country= `Country/Region`)
    world_cases <- aggregate(. ~ country, world_cases, FUN = sum)
    
    world_deaths <- world_deaths %>% dplyr::select(c(-`Province/State`, -Lat, -Long)) %>%
        rename(country= `Country/Region`)
    world_deaths <- aggregate(. ~ country, world_deaths, FUN = sum)
    #Melt case and death data
    world_popsize <-readRDS("./world_popsize.rds") 
    world_cases <- merge(world_popsize, world_cases)
    melt_cases <- gather(world_cases, date, cases, -country, -country_pop)
    world_deaths <- merge(world_deaths, world_popsize)
    melt_deaths <- gather(world_deaths, date, deaths, -country, -country_pop)
    all_merge <- merge(melt_deaths, melt_cases)
    
    world_clean <- all_merge %>% mutate(date = as.Date(as.character(date),format="%m/%d/%y")) %>%
        group_by(country) %>% arrange(date) %>%
        mutate(daily_cases = c(0,diff(cases))) %>%
        mutate(daily_deaths = c(0,diff(deaths))) %>% 
        ungroup() %>%
        rename(total_deaths = deaths, total_cases = cases, location = country, pop_size = country_pop) %>% 
        data.frame()
    
    saveRDS(world_clean,"world_cleandata.rds")
}


state_var = unique(us_clean$location)
country_var = unique(world_clean$location)

#################################
# Define UI
#################################
ui <- fluidPage(
    includeCSS("appstyle.css"),
    fluidRow(
        column(3,
               a(
                   href = "https://ceid.uga.edu",
                   tags$img(src = "ceidlogo.png", width = "100%"),
                   target = "_blank"
               )
        ),
        column(6, #text in middle
            tags$div(id = "shinyheadertitle", "YACT - Yet Another COVID-19 Tracker"),
            #the style 'shinyheadertitle' is defined in the appstyle.css file
            tags$div(
                id = "bigtext",
                "This tracker is brought to you by the",
                a(
                    "Center for the Ecology of Infectious Diseases",
                    href = "https://ceid.uga.edu",
                    target = "_blank"
                ),
                "and the",
                a("College of Public Health", href = "https://publichealth.uga.edu", target =
                      "_blank"),
                "at the",
                a("University of Georgia.", href = "https://www.uga.edu", target = "_blank"),
                "It was developed by",
                a("Robbie Richards,", href = "https://github.com/rlrichards", target =
                      "_blank"),
                a("William Norfolk", href = "https://github.com/williamnorfolk", target =
                      "_blank"),
                "and ",
                a("Andreas Handel.", href = "https://www.andreashandel.com/", target = "_blank"),
                "Underlying data for the US is sourced from",
                a(
                    "The Covid Tracking Project,",
                    href = "https://covidtracking.com/",
                    target = "_blank"
                ),
                "world data is sourced from the",
                a(
                    "Johns Hopkins University Center for Systems Science and Engineering.",
                    href = "https://github.com/CSSEGISandData/COVID-19",
                    target = "_blank"
                ),
                'Source code for this project can be found',
                a(
                    "In this GitHub repository.",
                    href = "https://github.com/CEIDatUGA/COVID-shiny-tracker",
                    target = "_blank"
                ),
                'We welcome feedback and feature requests, please send them as a',
                a(
                    "GitHub Issue",
                    href = "https://github.com/CEIDatUGA/COVID-shiny-tracker/issues",
                    target = "_blank"
                ),
                'or contact',
                a("Andreas Handel.", 
                  href = "https://www.andreashandel.com/", 
                  target = "_blank")
            ), #Close the bigtext text div
            tags$div(
                id = "bigtext",
              a(
                    "The Center for the Ecology of Infectious Diseases",
                    href = "https://ceid.uga.edu",
                    target = "_blank"
                ),
                'has several additional projects related to COVID-19, which can be found on the',
                a(
                    "CEID Coronavirus tracker website.",
                    href = "http://2019-coronavirus-tracker.com/",
                    target = "_blank"
                )
            ) #Close the bigtext text div
        ), #end text in middle
        column(3,         #logo on left
               a(
                   href = "https://publichealth.uga.edu",
                   tags$img(src = "cphlogo.png", width = "100%"),
                   target = "_blank"
               )
        ) #close right column
    ), #closes header fluid row
    
    navbarPage(
        title = "YACT",
        id = 'alltabs',
        selected = "us",
        tabPanel(
            title = "US",
            value = "us",
            sidebarLayout(
                sidebarPanel(
                    #Picker input = drop down bar
                    shinyWidgets::pickerInput(
                        "state_selector",
                        "Select States",
                        state_var,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE),
                        selected = c("CA", "WA", "GA")
                    ),
                    #Shiny selectors below major picker input
                    shiny::selectInput(
                        "case_death",
                        "Outcome",
                        c("Cases" = "cases", "Hospitalized" = "hospitalized", "Deaths" = "deaths"),
                        selected = "Cases"
                    ),
                    shiny::selectInput(
                        "daily_tot",
                        "Daily Count or Cumulative Total Count",
                        c("Daily" = "daily", "Total" = "total"),
                        selected = "Total"
                    ),
                    
                    shiny::selectInput(
                        "absolute_scaled",
                        "Absolute or scaled values",
                        c("Absolute number" = "actual", "Per 100K" = "scaled")
                    ),
                    
                    # It would be nice if we could get the X Cases to auto-change to match the selector below
                    shiny::selectInput(
                        "xscale",
                        "Set x-axis to calendar date or days since a specified total number of cases",
                        c("Calendar Date" = "x_time", "Days Since X Cases" = "x_count")
                    ),
                    sliderInput(
                        inputId = "count_limit",
                        "Choose the total number of cases at which to start graphs",
                        min = 1,
                        max = 500,
                        value = 10
                    ),
                    shiny::selectInput(
                        "yscale",
                        "Y-scale",
                        c("linear" = "linear", "logarithmic" = "logarithmic"),
                        selected = "logarithmic"
                    ),
                    br(),
                    br()
                ),
                #end sidebar panel
                
                # Output:
                mainPanel(
                    #change to plotOutput if using static ggplot object
                    plotlyOutput(outputId = "case_death_plot", height = "300px"),
                    #change to plotOutput if using static ggplot object
                    plotlyOutput(outputId = "testing_plot", height = "300px"),
                    #change to plotOutput if using static ggplot object
                    plotlyOutput(outputId = "testing_frac_plot", height = "300px")
                ) #end main panel
            )
        ),
        #close US tab
        
        
        tabPanel("World",  value = "world",
                 sidebarLayout(
                     sidebarPanel(
                         #Country selector coding with US, Italy, and Spain as awlays selected for a defult setting, will flash an error with none selected
                         #Picker input = drop down bar
                         shinyWidgets::pickerInput(
                             "country_selector",
                             "Select Countries",
                             country_var,
                             multiple = TRUE,
                             options = list(`actions-box` = TRUE),
                             selected = c("US", "Italy", "Spain")
                         ),
                         #Shiny selectors below major picker input
                         shiny::selectInput(
                             "case_death_w",
                             "Outcome",
                             c("Cases" = "cases", "Deaths" = "deaths"),
                             selected = "Cases"
                         ),
                         shiny::selectInput(
                             "daily_tot_w",
                             "Daily Count or Cumulative Total Count",
                             c("Daily" = "daily", "Total" = "total"),
                             selected = "Total"
                         ),
                         
                         shiny::selectInput(
                             "absolute_scaled_w",
                             "Absolute or scaled values",
                             c("Absolute number" = "actual", "Per 100K" = "scaled")
                         ),
                         
                         # It would be nice if we could get the X Cases to auto-change to match the selector below
                         shiny::selectInput(
                             "xscale_w",
                             "Set x-axis to calendar date or days since a specified total number of cases",
                             c("Calendar Date" = "x_time", "Days Since X Cases" = "x_count")
                         ),
                         sliderInput(
                             inputId = "count_limit_w",
                             "Choose the total number of cases at which to start graphs",
                             min = 1,
                             max = 500,
                             value = 10
                         ),
                         shiny::selectInput(
                             "yscale_w",
                             "Y-scale",
                             c("linear" = "linear", "logarithmic" = "logarithmic"),
                             selected = "logarithmic"
                         ),
                         br(),
                         br()
                     ),
                     
                     # Output:
                     mainPanel(
                         #change to plotOutput if using static ggplot object
                         plotlyOutput(outputId = "case_death_plot_world", height = "500px"),
                         
                     )
                 )) #close "World" tab
        
    ),
    #close NavBarPage
    tagList(
        hr(),
        p(
            'All text and figures are licensed under a ',
            a(
                "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.",
                href = "http://creativecommons.org/licenses/by-nc-sa/4.0/",
                target = "_blank"
            ),
            'Software/Code is licensed under ',
            a("GPL-3.", href = "https://www.gnu.org/licenses/gpl-3.0.en.html" , target =
                  "_blank"),
            'See source data sites for licenses governing data.'
            ,
            align = "center",
            style = "font-size:small"
        ) #end paragraph
    )
) #end fluidpage



# Define server function
server <- function(input, output) {
    

    #################################
    #Reactive function to prepare world data for plot 
    #################################
    get_plot_data_world <- reactive({  
        
        plot_dat <- world_clean %>% mutate(outcome = paste(isolate(input$daily_tot), isolate(input$case_death),sep='_')) #set outcome based on UI values
            
            if (input$absolute_scaled_w == 'scaled') #do extra scaling 
            {
                plot_dat <- plot_dat %>% mutate(outcome = outcome / pop_size * 100000)  
            } 
        
        #adjust data to align for plotting by cases on x-axis. 
        #Takes the plot_dat object created above to then designate further functionality
        if (input$xscale_w == 'x_count')
        {
            #Takes plot_dat and filters counts by the predetermined count limit from the reactive above
            #Created the tme variable (which represents the day number of the outbreak) from the date variable
            #Groups data by state/province
            #Will plot the number of days since the selected count_limit or the date
            plot_dat <- plot_dat %>% mutate(count_limit = input$count_limit_w) %>%
                filter(cases >= count_limit) %>%  
                mutate(Time = as.numeric(date)) %>%
                group_by(country) %>% 
                mutate(Time = Time - min(Time))
            tool_tip_w[1] <- "Days Since X Cases"
            list(plot_dat, y_labels, tool_tip_w)
        }
        else
        {
            plot_dat <- plot_dat %>% mutate(Time = date)
            list(plot_dat, y_labels, tool_tip_w)
        }
        
    }) #end reactive for world data 
                
    
    
            
    get_plot_data <- reactive({  
        
        #choose either cases or deaths to plot US DATA
        if (input$case_death == 'case' && input$daily_tot == 'daily' && input$absolute_scaled == 'actual')
        {
            plot_dat <- us_clean %>% mutate(outcome = daily_positive) %>%  
                mutate(test_outcome = daily_total) %>%
                mutate(test_frac_outcome = daily_positive/daily_total) 
            y_labels <- c("Daily New Case Count", "Daily Number of Tests", "Daily Positive Test Proportion")
            tool_tip <- c("Date", "Cases", "Tests", "Positive Test Proportion")
        }
        if (input$case_death == 'death' && input$daily_tot == 'daily' && input$absolute_scaled == 'actual')
        {
            plot_dat <- us_clean %>% mutate(outcome = daily_death)  %>%
                mutate(test_outcome = daily_total) %>%
                mutate(test_frac_outcome = daily_positive/daily_total)
            y_labels <- c("Daily Fatality Count", "Daily Number of Tests", "Daily Positive Test Proportion")
            tool_tip <- c("Date","Fatalities", "Tests", "Positive Test Proportion")
        }
        if (input$case_death == 'case' && input$daily_tot == 'tot' && input$absolute_scaled == 'actual')
        {
            plot_dat <- us_clean %>% mutate(outcome = positive) %>%  
                mutate(test_outcome = total)%>%
                mutate(test_frac_outcome = positive/total)
            y_labels <- c("Cumulative Case Count", "Cumulative Test Count", "Cumulative Positive Test Proportion")
            tool_tip <- c("Date","Cases", "Tests", "Positive Test Proportion")
            
        }
        if (input$case_death == 'death' && input$daily_tot == 'tot' && input$absolute_scaled == 'actual')
        {
            plot_dat <- us_clean %>% mutate(outcome = death) %>% 
                mutate(test_outcome = total) %>%
                mutate(test_frac_outcome = positive/total)
            y_labels <- c("Cumulative Fatality Count", "Cumulative Test Count", "Cumulative Positive Test Proportion")
            tool_tip <- c("Date","Fatalities", "Tests", "Positive Test Proportion")
        }
        
        #choose either cases or deaths to plot for 100k US DATA
        if (input$case_death == 'case' && input$daily_tot == 'daily' && input$absolute_scaled == 'scaled')
        {
            plot_dat <- us_clean %>% mutate(outcome = (daily_positive / total_pop) * 100000) %>%  
                mutate(test_outcome = (daily_total / total_pop) * 100000) %>%
                mutate(test_frac_outcome = daily_positive/daily_total) 
            y_labels <- c("Daily New Case Count", "Daily Number of Tests", "Daily Positive Test Proportion")
            tool_tip <- c("Date", "Cases", "Tests", "Positive Test Proportion")
        }
        if (input$case_death == 'death' && input$daily_tot == 'daily' && input$absolute_scaled == 'scaled')
        {
            plot_dat <- us_clean %>% mutate(outcome = (daily_death / total_pop) * 100000)  %>%
                mutate(test_outcome = (daily_total / total_pop) * 100000) %>%
                mutate(test_frac_outcome = daily_positive/daily_total) 
            y_labels <- c("Daily Fatality Count", "Daily Number of Tests", "Daily Positive Test Proportion")
            tool_tip <- c("Date","Fatalities", "Tests", "Positive Test Proportion")
        }
        if (input$case_death == 'case' && input$daily_tot == 'tot' && input$absolute_scaled == 'scaled')
        {
            plot_dat <- us_clean %>% mutate(outcome = (positive / total_pop) * 100000) %>%  
                mutate(test_outcome = (total / total_pop) * 100000) %>%
                mutate(test_frac_outcome = positive/total)
            y_labels <- c("Cumulative Case Count", "Cumulative Test Count", "Cumulative Positive Test Proportion")
            tool_tip <- c("Date","Cases", "Tests", "Positive Test Proportion")
            
        }
        if (input$case_death == 'death' && input$daily_tot == 'tot' && input$absolute_scaled == 'scaled')
        {
            plot_dat <- us_clean %>% mutate(outcome = (death / total_pop) * 100000) %>% 
                mutate(test_outcome = (total / total_pop) * 100000) %>%
                mutate(test_frac_outcome = positive/total)
            y_labels <- c("Cumulative Fatality Count", "Cumulative Test Count", "Cumulative Positive Test Proportion")
            tool_tip <- c("Date","Fatalities", "Tests", "Positive Test Proportion")
        }
        
        
        #adjust data to align for plotting by cases on x-axis. 
        #Takes the plot_dat object created above to then designate further functionality
        if (input$xscale == 'x_count')
        {
            #Takes plot_dat and filters counts by the predetermined count limit from the reactive above
            #Created the tme variable (which represents the day number of the outbreak) from the date variable
            #Groups data by state/province
            #Will plot the number of days since the selected count_limit or the date
            plot_dat <- plot_dat %>% mutate(count_limit = input$count_limit) %>%
                filter(positive >= count_limit) %>%  
                mutate(Time = as.numeric(date)) %>%
                group_by(state) %>% 
                mutate(Time = Time - min(Time))
            tool_tip[1] <- "Days Since X Cases"
            list(plot_dat, y_labels, tool_tip)
        }
        else
        {
            plot_dat <- plot_dat %>% mutate(Time = date)
            list(plot_dat, y_labels, tool_tip)
        } 
    }) #end reactive function that produces the right plot_dat data needed

   #Start World Data
   get_plot_data_world2 <- reactive({

    if (input$case_death_w == 'case' && input$daily_tot_w == 'daily' && input$absolute_scaled_w == 'actual')
    {
        plot_dat <- world_clean %>% mutate(outcome = daily_cases)

        y_labels <- c("Daily New Case Count", "Daily Number of Tests", "Daily Positive Test Proportion")
        tool_tip_w <- c("Date", "Cases", "Tests", "Positive Test Proportion")
    }
    if (input$case_death_w == 'death' && input$daily_tot_w == 'daily' && input$absolute_scaled_w == 'actual')
    {
        plot_dat <- world_clean %>% mutate(outcome = daily_deaths)

        y_labels <- c("Daily Fatality Count", "Daily Number of Tests", "Daily Positive Test Proportion")
        tool_tip_w <- c("Date","Fatalities", "Tests", "Positive Test Proportion")
    }
    if (input$case_death_w == 'case' && input$daily_tot_w == 'tot' && input$absolute_scaled_w == 'actual')
    {
        plot_dat <- world_clean %>% mutate(outcome = cases)  

        y_labels <- c("Cumulative Case Count", "Cumulative Test Count", "Cumulative Positive Test Proportion")
        tool_tip_w <- c("Date","Cases", "Tests", "Positive Test Proportion")
        
    }
    if (input$case_death_w == 'death' && input$daily_tot_w == 'tot' && input$absolute_scaled_w == 'actual')
    {
        plot_dat <- world_clean %>% mutate(outcome = deaths)

        y_labels <- c("Cumulative Fatality Count", "Cumulative Test Count", "Cumulative Positive Test Proportion")
        tool_tip_w <- c("Date","Fatalities", "Tests", "Positive Test Proportion")
    }
       
       
       #choose either cases or deaths to plot for 100k WORLD DATA
       if (input$case_death_w == 'case' && input$daily_tot_w == 'daily' && input$absolute_scaled_w == 'scaled')
       {
           plot_dat <- world_clean %>% mutate(outcome = (daily_cases / country_pop) * 100000)  

           y_labels <- c("Daily New Case Count", "Daily Number of Tests", "Daily Positive Test Proportion")
           tool_tip_w <- c("Date", "Cases", "Tests", "Positive Test Proportion")
       }
       if (input$case_death_w == 'death' && input$daily_tot_w == 'daily' && input$absolute_scaled_w == 'scaled')
       {
           plot_dat <- world_clean %>% mutate(outcome = (daily_deaths / country_pop) * 100000)
 
           y_labels <- c("Daily Fatality Count", "Daily Number of Tests", "Daily Positive Test Proportion")
           tool_tip_w <- c("Date","Fatalities", "Tests", "Positive Test Proportion")
       }
       if (input$case_death_w == 'case' && input$daily_tot_w == 'tot' && input$absolute_scaled_w == 'scaled')
       {
           plot_dat <- world_clean %>% mutate(outcome = (cases / country_pop) * 100000) 

           y_labels <- c("Cumulative Case Count", "Cumulative Test Count", "Cumulative Positive Test Proportion")
           tool_tip_w <- c("Date","Cases", "Tests", "Positive Test Proportion")
           
       }
       if (input$case_death_w == 'death' && input$daily_tot_w == 'tot' && input$absolute_scaled_w == 'scaled')
       {
           plot_dat <- world_clean %>% mutate(outcome = (deaths / country_pop) * 100000)
     
           y_labels <- c("Cumulative Fatality Count", "Cumulative Test Count", "Cumulative Positive Test Proportion")
           tool_tip_w <- c("Date","Fatalities", "Tests", "Positive Test Proportion")
       }
       
       #adjust data to align for plotting by cases on x-axis. 
       #Takes the plot_dat object created above to then designate further functionality
       if (input$xscale_w == 'x_count')
       {
           #Takes plot_dat and filters counts by the predetermined count limit from the reactive above
           #Created the tme variable (which represents the day number of the outbreak) from the date variable
           #Groups data by state/province
           #Will plot the number of days since the selected count_limit or the date
           plot_dat <- plot_dat %>% mutate(count_limit = input$count_limit_w) %>%
               filter(cases >= count_limit) %>%  
               mutate(Time = as.numeric(date)) %>%
               group_by(country) %>% 
               mutate(Time = Time - min(Time))
           tool_tip_w[1] <- "Days Since X Cases"
           list(plot_dat, y_labels, tool_tip_w)
       }
       else
       {
           plot_dat <- plot_dat %>% mutate(Time = date)
           list(plot_dat, y_labels, tool_tip_w)
       }
   })
   
   
    #make the plot for cases/deaths for US data
    output$case_death_plot <- renderPlotly({
        tool_tip <- get_plot_data()[[3]]
        scaleparam <- "fixed"
        p1 <- get_plot_data()[[1]] %>% 
            #Filter data for cases >0 and selected states
            filter(outcome > 0) %>% 
            filter(state %in% input$state_selector) %>% 
            #Begin plot
            ggplot(aes(x=Time, y = outcome, color = state))+
            geom_line()+
            geom_point(aes(text = paste(paste0("State: ", state), paste0(tool_tip[1], ": ", Time),paste0(tool_tip[2],": ", outcome),sep ="\n")))+
            theme_light() + 
            ylab(get_plot_data()[[2]][1])
        #Flip to logscale if selected
        if(input$yscale == "logarithmic") {
            p1 <- p1 + scale_y_log10() 
        }
        if(input$xscale =="x_time"){
            p1 <- p1 +   scale_x_date(date_labels = "%b %d")
        }
        ggplotly(p1, tooltip = "text")
    }) #end function making case/deaths plot
    
    #make the plot for cases/deaths for world data
    output$case_death_plot_world <- renderPlotly({
        tool_tip_w <- get_plot_data_world()[[3]]
        scaleparam <- "fixed"
        p4 <- get_plot_data_world()[[1]] %>% 
            #Filter data for cases >0 and selected states
            filter(outcome > 0) %>% 
            filter(country %in% input$country_selector) %>% 
            #Begin plot
            ggplot(aes(x=Time, y = outcome, color = country))+
            geom_line() +
            geom_point(aes(text = paste(paste0("Country: ", country), paste0(tool_tip_w[1], ": ", Time),paste0(tool_tip_w[2],": ", outcome),sep ="\n")))+
            theme_light() + 
            ylab(get_plot_data_world()[[2]][1])
        #Flip to logscale if selected
        if(input$yscale_w == "logarithmic") {
            p4 <- p4 + scale_y_log10() 
        }
        if(input$xscale =="x_time"){
            p4 <- p4 +   scale_x_date(date_labels = "%b %d")
        }
        ggplotly(p4, tooltip = "text")
    }) #end function making case/deaths plot
    
    #make the testing plots 
    output$testing_plot <- renderPlotly({
        scaleparam <- "fixed"
        tool_tip <- get_plot_data()[[3]]
        p2 <- get_plot_data()[[1]] %>% 
            #Filter data for cases >0 and selected states
            filter(test_outcome > 0) %>% 
            filter(state %in% input$state_selector) %>% 
            #Begin plot
            ggplot(aes(x=Time, y = test_outcome, color = state))+
            geom_line()+
            geom_point(aes(text = paste(paste0("State: ", state), paste0(tool_tip[1], ": ", Time),paste0(tool_tip[3],": ", test_outcome),sep ="\n")))+
            theme_light()+
            ylab(get_plot_data()[[2]][2])
        #Flip to logscale if selected
        if(input$yscale == "logarithmic") {
            p2 <- p2 + scale_y_log10() 
        }
        if(input$xscale =="x_time"){
            p2 <- p2 +   scale_x_date(date_labels = "%b %d")
        }
        ggplotly(p2, tooltip = "text")
    }) #end function making testing plot
    
    #make the fraction positive testing plots 
    output$testing_frac_plot <- renderPlotly({
        scaleparam <- "fixed"
        tool_tip <- get_plot_data()[[3]]
        p3 <- get_plot_data()[[1]] %>% 
            #Filter data for cases >0 and selected states
            filter(test_frac_outcome > 0) %>% 
            filter(state %in% input$state_selector) %>% 
            #Begin plot
            ggplot(aes(x=Time, y = test_frac_outcome, color = state))+
            geom_line()+
            geom_point(aes(text = paste(paste0("State: ", state), paste0(tool_tip[1], ": ", Time),paste0(tool_tip[4],": ", round(test_frac_outcome, digits =3)),sep ="\n")))+
            theme_light() +
            ylab(get_plot_data()[[2]][3])
        
        if(input$xscale =="x_time"){
            p3 <- p3 +   scale_x_date(date_labels = "%b %d")
        }
        ggplotly(p3, tooltip = "text")
    }) #end function making testing plot
    
} #end server function

# Create Shiny object
shinyApp(ui = ui, server = server)
