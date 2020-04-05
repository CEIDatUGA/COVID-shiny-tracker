# Load packages
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(shinyWidgets)
library(plotly)
library(scales)
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
if (file.exists('us_cleandata.rds') && as.Date(file.mtime('us_cleandata.rds')) ==  Sys.Date()) {
    #################################
    # load already clean data locally
    #################################
    us_clean <- readRDS('us_cleandata.rds')
} else {
    #################################
    # pull data from Covidtracking and process
    #################################
    us_data <- read_csv("https://covidtracking.com/api/states/daily.csv")
    #data for population size for each state/country so we can compute cases per 100K
    us_popsize <- readRDS("us_popsize.rds")
    
    
    us_clean <- us_data %>% dplyr::select(c(date,state,positive,negative,total,hospitalized,death)) %>%
        mutate(date = as.Date(as.character(date),format="%Y%m%d")) %>% 
        group_by(state) %>% arrange(date) %>%
        mutate(Daily_Test_Positive = c(0,diff(positive))) %>% 
        mutate(Daily_Test_Negative = c(0,diff(negative))) %>% 
        mutate(Daily_Test_All = c(0,diff(total))) %>% 
        mutate(Daily_Hospitalized = c(0,diff(hospitalized))) %>% 
        mutate(Daily_Deaths = c(0,diff(death))) %>%
        merge(us_popsize) %>%
        rename(Date = date, Location = state, Population_Size = total_pop, Total_Deaths = death, Total_Cases = positive, Total_Hospitalized = hospitalized, Total_Test_Negative = negative, Total_Test_Positive = positive, Total_Test_All = total) %>%
        mutate(Daily_Cases = Daily_Test_Positive, Total_Cases = Total_Test_Positive)
    #Change NA hospitalizations to zero
     us_clean$Total_Hospitalized[is.na(us_clean$Total_Hospitalized)] <- 0
     us_clean$Daily_Hospitalized[is.na(us_clean$Daily_Hospitalized)] <- 0
    
    saveRDS(us_clean,'us_cleandata.rds')
}


#Pull world data
#for speed, we should do it such that it only gets it from the API if the data is old, otherwise it should load locally
if (file.exists('world_cleandata.rds') && as.Date(file.mtime('world_cleandata.rds')) ==  Sys.Date()) {
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
        mutate(Daily_Cases = c(0,diff(cases))) %>%
        mutate(Daily_Deaths = c(0,diff(deaths))) %>% 
        ungroup() %>%
        rename(Date = date, Total_Deaths = deaths, Total_Cases = cases, Location = country, Population_Size = country_pop) %>% 
        data.frame()
    
    saveRDS(world_clean,"world_cleandata.rds")
}

state_var = unique(us_clean$Location)
country_var = unique(world_clean$Location)

#################################
# Define UI
#################################
ui <- fluidPage(
    includeCSS("appstyle.css"),
  
    fluidRow(
        column(4,
               a(
                   href = "https://ceid.uga.edu",
                   tags$img(src = "ceidlogo.png", width = "100%"),
                   target = "_blank"
               ),
               a(
                          href = "https://publichealth.uga.edu",
                          tags$img(src = "cphlogo.png", width = "100%"),
                          target = "_blank"
               )
        ),
        column(8, #text in middle
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
                       "in this GitHub repository.",
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
        ) #close right column
    ), #closes header fluid row
    
    navbarPage( title = "YACT", id = 'alltabs', selected = "us",
        tabPanel(
          title = "US",
          value = "us",
                 fluidRow(
                   column(12,
                          uiOutput('us_ui')
                   ),
                   class = "mainmenurow"
                 )
                 
        ), #close US tab
        tabPanel(
          title = "World",
          value = "world",
          fluidRow(
            column(12,
                   uiOutput('world_ui')
            ),
            class = "mainmenurow"
          )
        ) #close world tab
     ),     #close NavBarPage
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
server <- function(input, output, session) {
    

  observeEvent( input$alltabs == 'world', 
                {
                  output$world_ui <- renderUI({
                    
                    sidebarLayout(
                      sidebarPanel(
                        #Country selector coding with US, Italy, and Spain as always selected for a defult setting, will flash an error with none selected
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
                          c("Cases" = "Cases", "Deaths" = "Deaths"),
                          selected = "Cases"
                        ),
                        shiny::selectInput(
                          "daily_tot_w",
                          "Daily Count or Cumulative Total Count",
                          c("Daily" = "Daily", "Total" = "Total"),
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
                          "Set x-axis to calendar date or days since a specified Total number of cases",
                          c("Calendar Date" = "x_time", "Days Since X Cases" = "x_count")
                        ),
                        sliderInput(
                          inputId = "count_limit_w",
                          "Choose the Total number of cases at which to start graphs",
                          min = 1,
                          max = 500,
                          value = 10
                        ),
                        shiny::selectInput(
                          "yscale_w",
                          "Y-scale",
                          c("Linear" = "linear", "Logarithmic" = "logarithmic"),
                          selected = "logarithmic"
                        ),
                        br(),
                        br()
                      ),
                      
                      # Output:
                      mainPanel(
                        #change to plotOutput if using static ggplot object
                        plotlyOutput(outputId = "case_death_plot_world", height = "500px"),
                          ) #close mainpanel
                    ) #close sidebar layout
                  }) #end render UI
                  
        #reactive function to generate world data for plot
        get_plot_data_world <- reactive({
          
          plot_list <- set_outcome(world_clean,input$case_death_w,input$daily_tot_w,input$absolute_scaled_w,input$xscale_w,input$count_limit_w,input$alltabs)
          
        })
                  
        #make the plot for cases/deaths for world data
        output$case_death_plot_world <- renderPlotly({
          w_plot_dat <- get_plot_data_world()
          tool_tip_w <- w_plot_dat[[3]]
          scaleparam <- "fixed"
          p4 <- w_plot_dat[[1]] %>% 
            #Filter data for cases >0 and selected states
            filter(outcome > 0) %>% 
            filter(Location %in% input$country_selector) %>% 
            #Begin plot
            ggplot(aes(x=Time, y = outcome, color = Location))+
            geom_line() +
            geom_point(aes(text = paste(paste0("Country: ", Location), paste0(tool_tip_w[1], ": ", Date),paste0(tool_tip_w[2],": ", outcome),sep ="\n")))+
            theme_light() + 
            scale_y_continuous(labels = comma)+
            ylab(w_plot_dat[[2]][1])
          #Flip to logscale if selected
          if(input$yscale_w == "logarithmic") {
            p4 <- p4 + scale_y_log10(labels = comma) 
          }
          if(input$xscale_w =="x_time"){
            p4 <- p4 +   scale_x_date(date_labels = "%b %d")
          }
          ggplotly(p4, tooltip = "text") #this current doesn't work, produces an error message 
        }) #end function making case/deaths plot          
        
        
    }) #end world observe event 
  
      
  observeEvent( input$alltabs == 'us', 
  {
    output$us_ui <- renderUI({
      
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
            c("Cases" = "Cases", "Deaths" = "Deaths", "Hospitalizations" = "Hospitalized"),
            selected = "Cases"
          ),
          shiny::selectInput(
            "daily_tot",
            "Daily Count or Cumulative Total Count",
            c("Daily" = "Daily", "Total" = "Total"),
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
            "Set x-axis to calendar date or days since a specified Total number of cases",
            c("Calendar Date" = "x_time", "Days Since X Cases" = "x_count")
          ),
          sliderInput(
            inputId = "count_limit",
            "Choose the Total number of cases at which to start graphs",
            min = 1,
            max = 500,
            value = 10
          ),
          shiny::selectInput(
            "yscale",
            "Y-scale",
            c("Linear" = "linear", "Logarithmic" = "logarithmic"),
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
      ) #end sidebar layout
    }) #end render UI
    
    
    
    #Reactive function to prepare US plot data
    get_plot_data_us <- reactive({  
      
      plot_list <- set_outcome(us_clean,input$case_death,input$daily_tot,input$absolute_scaled,input$xscale,input$count_limit,input$alltabs)
    }) #end reactive function that produces the right plot_dat data needed
    
    #make the plot for cases/deaths for US data
    output$case_death_plot <- renderPlotly({
      tool_tip <- get_plot_data_us()[[3]]
      scaleparam <- "fixed"
      p1 <- get_plot_data_us()[[1]] %>% 
        #Filter data for cases >0 and selected states
        filter(outcome > 0) %>% 
        filter(Location %in% input$state_selector) %>% 
        #Begin plot
        ggplot(aes(x=Time, y = outcome, color = Location))+
        geom_line()+
        geom_point(aes(text = paste(paste0("State: ", Location), paste0(tool_tip[1], ": ", Date),paste0(tool_tip[2],": ", outcome),sep ="\n")))+
        theme_light() + 
        scale_y_continuous(labels = comma)+
        ylab(get_plot_data_us()[[2]][1])
      #Flip to logscale if selected
      if(input$yscale == "logarithmic") {
        p1 <- p1 + scale_y_log10(labels = comma) 
      }
      if(input$xscale =="x_time"){
        p1 <- p1 +   scale_x_date(date_labels = "%b %d")
      }
      ggplotly(p1, tooltip = "text")
    }) #end function making case/deaths plot
    
    
    
    #make the testing plots 
    output$testing_plot <- renderPlotly({
      scaleparam <- "fixed"
      tool_tip <- get_plot_data_us()[[3]]
      p2 <- get_plot_data_us()[[1]] %>% 
        #Filter data for cases >0 and selected states
        filter(test_outcome > 0) %>% 
        filter(Location %in% input$state_selector) %>% 
        #Begin plot
        ggplot(aes(x=Time, y = test_outcome, color = Location))+
        geom_line()+
        geom_point(aes(text = paste(paste0("State: ", Location), paste0(tool_tip[1], ": ", Date),paste0(tool_tip[3],": ", test_outcome),sep ="\n")))+
        theme_light()+
        scale_y_continuous(labels = comma)+
        ylab(get_plot_data_us()[[2]][2])
      #Flip to logscale if selected
      if(input$yscale == "logarithmic") {
        p2 <- p2 + scale_y_log10(labels = comma) 
      }
      if(input$xscale =="x_time"){
        p2 <- p2 +   scale_x_date(date_labels = "%b %d")
      }
      ggplotly(p2, tooltip = "text")
    }) #end function making testing plot
    
    #make the fraction positive testing plots 
    output$testing_frac_plot <- renderPlotly({
      scaleparam <- "fixed"
      tool_tip <- get_plot_data_us()[[3]]
      p3 <- get_plot_data_us()[[1]] %>% 
        #Filter data for cases >0 and selected states
        filter(test_frac_outcome > 0) %>% 
        filter(Location %in% input$state_selector) %>% 
        #Begin plot
        ggplot(aes(x=Time, y = test_frac_outcome, color = Location))+
        geom_line()+
        geom_point(aes(text = paste(paste0("State: ", Location), paste0(tool_tip[1], ": ", Date),paste0(tool_tip[4],": ", round(test_frac_outcome, digits =3)),sep ="\n")))+
        theme_light() +
        ylab(get_plot_data_us()[[2]][3])
      
      if(input$xscale =="x_time"){
        p3 <- p3 +   scale_x_date(date_labels = "%b %d")
      }
      ggplotly(p3, tooltip = "text")
    }) #end function making testing plot
    
    
  }) #end observer listening to US tab choice
  

    
  
    #Function that does axis shift for data
    shift_x_axis <- function(plot_dat,count_limit)
    {
        #Takes plot_dat and filters counts by the predetermined count limit from the reactive above
        #Created the tme variable (which represents the day number of the outbreak) from the date variable
        #Groups data by state/province
        #Will plot the number of days since the selected count_limit or the date
        plot_dat <- plot_dat %>% 
            filter(Total_Cases >= count_limit) %>%  
            mutate(Time = as.numeric(Date)) %>%
            group_by(Location) %>% 
            mutate(Time = Time - min(Time))
        
    }
    
    #function that takes UI settings and produces data for plot accordingly
    set_outcome <- function(plot_dat,case_death,daily_tot,absolute_scaled,xscale,count_limit,selected_tab)
    {
      out_type = paste(daily_tot,case_death,sep='_') #make string from UI inputs that correspond with variable names
      plot_dat <- plot_dat %>% mutate(outcome = get(out_type)) #pick output based on variable name created from UI
      
       
      # # do testing data for US 
       if (selected_tab == "us")  
       {
         test_out_type = paste(daily_tot,'Test_All',sep='_')
         test_pos_type = paste(daily_tot,'Test_Positive',sep='_')
         plot_dat <- plot_dat %>% mutate(test_outcome = get(test_out_type)) 
         plot_dat <- plot_dat %>% mutate(test_frac_outcome = get(test_pos_type)/get(test_out_type))
       }
      
        
      #if we want scaling by 100K, do extra scaling 
       if (absolute_scaled == 'scaled')
       {
         plot_dat <- plot_dat %>% mutate(outcome = outcome / Population_Size * 100000) 
          if (selected_tab == "us" )  
          {
            plot_dat <- plot_dat %>%  mutate(test_outcome = test_outcome / Population_Size * 100000)
            
          }
         
       }

      #set labels and tool tips based on input - entries 2 and 3 are ignored for world plot

      y_labels <- paste(daily_tot, c("Cases", "Tests", "Positive Test Proportion"), sep = " ")
      
      
      tool_tip <- c("Date","Cases", "Tests", "Positive Test Proportion")
      tool_tip[2] <- case_death
      
      #adjust data to align for plotting by cases on x-axis. 
      #Takes the plot_dat object created above to then designate further functionality
      if (xscale == 'x_count')
      {
        plot_dat <- shift_x_axis(plot_dat,count_limit)
      }
      else
      {
        plot_dat <- plot_dat %>% mutate(Time = Date)
      }
        
      list(plot_dat, y_labels, tool_tip) #return list
    } #end function that produces output for plots
    
   
   
  
    
} #end server function

# Create Shiny object
shinyApp(ui = ui, server = server)
