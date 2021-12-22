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

source('data_clean_lib.R')

#################################
#define some variables used often
#################################

#starting date for date slider and default starting date to show
mindate = as.Date("2020-01-01","%Y-%m-%d")
defaultdate = as.Date("2021-01-01","%Y-%m-%d")


###########################################
# Define server functions
###########################################
server <- function(input, output, session) {
  
  
  ###########################################
  # function that re-reads the data every so often
  ###########################################
  all_data <- reactivePoll(intervalMillis = 1000*60*60*3, # pull new data every N hours
                           session = NULL,
                           checkFunc = function() {Sys.time()}, #this will always return a different value, which means at intervals specified by intervalMillis the new data will be pulled
                           valueFunc = function() {
                             withProgress(message = 'Loading data, please be patient.',
                                          detail = "", value = 0,
                                          {
                                            get_data()
                                          } ) #end with-progress wrapper
                           })   
  
  #read data is reactive, doesn't work for rest below 
  all_dat = isolate(all_data())
  #all_dat = all_data
  
  # pull data out of list 
  world_dat = all_dat$world_dat 
  us_dat = all_dat$us_dat 
  county_dat = all_dat$county_dat
  
  #define variables for location and source selectors
  state_var = sort(unique(us_dat$location))  
  state_var <- c("US",state_var[!state_var=="US"]) #move US to front
  country_var = sort(unique(world_dat$location))
  county_var = sort(unique(county_dat$county))
  state_var_county = sort(unique(county_dat$state))
  
  us_source_var = unique(us_dat$source)
  world_source_var = unique(world_dat$source)
  county_source_var = unique(county_dat$source)
  
  
  
  ################################################################################################
  #create the following UI elements on server and then add to UI sincc they depend on the variables above 
  #those variables are only defined on server
  ################################################################################################
  output$state_selector = renderUI({
    shinyWidgets::pickerInput("state_selector", "Select States (all US at top)", state_var, multiple = TRUE,options = list(`actions-box` = TRUE), selected = c("Georgia","Texas","Arizona") )
  })
  output$source_selector = renderUI({
    shinyWidgets::pickerInput("source_selector", "Select Sources (see 'About' tab for details)", us_source_var, multiple = TRUE,options = list(`actions-box` = TRUE), selected = c("COVIDTracking") )
  })
  output$state_selector_c = renderUI({
    shinyWidgets::pickerInput("state_selector_c", "Select state", state_var_county,  multiple = FALSE, options = list(`actions-box` = TRUE), selected = c("Georgia"))
  })
  output$county_selector = renderUI({
    shinyWidgets::pickerInput("county_selector", "Select counties", county_var,  multiple = TRUE, options = list(`actions-box` = TRUE), selected = county_var[1])
  })
  output$country_selector = renderUI({
    shinyWidgets::pickerInput("country_selector", "Select countries", country_var,  multiple = TRUE, options = list(`actions-box` = TRUE), selected = c("US", "India", "Brazil"))
  })
  output$source_selector_w = renderUI({
    shinyWidgets::pickerInput("source_selector_w", "Select Sources (see 'About' tab for details)", world_source_var, multiple = TRUE,options = list(`actions-box` = TRUE), selected = c("JHU") )
  })
  
  
  #watch the choice for the x-scale and choose what to show underneath accordingly
  observeEvent(input$xscale,
               {
                 if (input$xscale == 'x_count')
                 {
                   #Add a reactive range to slider
                   shiny::updateSliderInput(session, "x_limit", min = 1,  max = 500, step = 10, value = 1 )
                 } else
                 {
                   shiny::updateSliderInput(session, "x_limit", min = mindate,  max = Sys.Date(), value = defaultdate )
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
                   shiny::updateSliderInput(session, "x_limit_w", min = mindate,  max = Sys.Date(), value = defaultdate )
                 }
               }) #end observe event 
  
  #watch the choice for the x-scale and choose what to show underneath accordingly
  observeEvent(input$xscale_c_low,
               {
                 if (input$xscale_c == 'x_count')
                 {
                   #Add a reactive range to slider
                   shiny::updateSliderInput(session, "x_limit_c_low", min = 1,  max = 500, step = 10, value = 1 )
                 } else
                 {
                   shiny::updateSliderInput(session, "x_limit_c_low", min = mindate,  max = Sys.Date(), value = defaultdate )
                 }
               }) #end observe event  

  #watch the choice for the x-scale and choose what to show underneath accordingly
  observeEvent(input$xscale_c_high,
               {
                 if (input$xscale_c == 'x_count')
                 {
                   #Add a reactive range to slider
                   shiny::updateSliderInput(session, "x_limit_c_high", min = 1,  max = 500, step = 10, value = 1 )
                 } else
                 {
                   shiny::updateSliderInput(session, "x_limit_c_high", min = mindate,  max = Sys.Date(), value = defaultdate )
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
                          xscale, yscale, absolute_scaled, 
                          x_limit_low, x_limit_high, current_tab,  
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
    
    
    #get only the selected state for county, so we can do color assignment
    if (current_tab == "county")
    {
      plot_dat <- all_plot_dat %>% filter(state %in% input$state_selector_c) 
    }    
    else
    {
      plot_dat <- all_plot_dat
    }
    
    #set colors before filtering out locations so they remain the same
    set.seed(1234)#keep the colors the same
    n_colors <- length(unique(plot_dat$location))
    add_palette <- colorRampPalette(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666"))
    static_colors <- add_palette(n_colors) %>% sample()
    plotcolors <<- stats::setNames(
      object = static_colors,
      #object = colorspace::rainbow_hcl(n_colors,start = 0, end = 360*(n_colors-1)/n_colors, alpha = 1, fixup = TRUE), 
      nm = unique(plot_dat$location) %>% unique()
    )
    
    #filter data based on user selections
    #keep all outcomes/variables for now so we can do x-axis adjustment
    #filtering of only the outcome to plot is done after x-scale adjustment
    plot_dat <- all_plot_dat %>%   filter(location %in% location_selector) %>%      
        filter(source %in% source_selector) %>%
        group_by(source,location) %>%
        arrange(date) %>%
        ungroup()
    
    
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
        filter((date >= x_limit_low) & (date <= x_limit_high)) 
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
    pl <- plotly::plot_ly(p_dat, type = NULL, colors = ~plotcolors) %>% 
      plotly::add_trace(x = ~time, y = ~value, type = 'scatter', 
                        mode = 'lines', 
                        linetype = ~source, color = ~location,
                        line = list(width = linesize), 
                        text = tooltip_text) %>%
      layout(yaxis = list(title=y_labels[ylabel], type = yscale, size = 18)) %>%
      layout(legend = list(orientation = "h", x = 0.2, y = -0.3))
    
    # if requested by user, apply and show a smoothing function 
    if (show_smoother == "Yes")
    {
      if (any(location_selector %in% p_dat$location))
      {
        p_dat2 <- p_dat  %>% select(location,source,value,time) %>% drop_na() %>%
          group_by(location) %>%
          filter(n() >= 2) %>%  
          mutate(smoother = loess(value ~ as.numeric(time), span = .4)$fitted) %>%    
          ungroup()
        
        pl <- pl %>% plotly::add_lines(x = ~time, y = ~smoother, 
                                       data = p_dat2, colors = ~plotcolors, color = ~location,
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
    if (!is.null(input$source_selector) && input$current_tab == "us")
    {
      #create plot
      pl <- make_plotly(us_dat, input$state_selector, input$source_selector, input$case_death, input$daily_tot,
                        input$xscale, input$yscale, input$absolute_scaled, 
                        x_limit_low = input$x_limit_low, x_limit_high = input$x_limit_high, 
                        input$current_tab,
                        input$show_smoother, ylabel = 1, outtype = '')
    }
    return(pl)
  }) #end function making case/deaths plot
  
  ###########################################
  #function that makes testing plot for US tab
  ###########################################
  output$testing_plot <- renderPlotly({
    pl <- NULL
    if ('COVIDTracking' %in% input$source_selector && input$current_tab == "us")
    {
      pl <- make_plotly(us_dat, input$state_selector, input$source_selector, input$case_death, input$daily_tot,
                        input$xscale, input$yscale, input$absolute_scaled, 
                        x_limit_low = input$x_limit_low, x_limit_high = input$x_limit_high, 
                        input$current_tab,
                        input$show_smoother, ylabel = 2, outtype = 'Test_All')
    }
    return(pl)
  }) #end function making testing plot
  
  ###########################################
  #function that makes testing positive fraction plot for US tab
  ###########################################
  output$testing_frac_plot <- renderPlotly({
    pl <- NULL
    if ('COVIDTracking' %in% input$source_selector && input$current_tab == "us")
    {
      pl <- make_plotly(us_dat, input$state_selector, input$source_selector, input$case_death, input$daily_tot,
                        input$xscale, input$yscale, input$absolute_scaled,
                        x_limit_low = input$x_limit_low, x_limit_high = input$x_limit_high, 
                        input$current_tab,
                        input$show_smoother, ylabel = 3, outtype = 'Positive_Prop')
      
    }
    return(pl)
  }) #end function making testing plot
  
  ###########################################
  #function that makes case/death  for world tab
  ###########################################
  output$world_case_death_plot <- renderPlotly({
    pl <- NULL
    if (!is.null(input$source_selector_w) && input$current_tab == "world")
    {
      pl <- make_plotly(world_dat, input$country_selector, input$source_selector_w, input$case_death_w, input$daily_tot_w,
                        input$xscale_w, input$yscale_w, input$absolute_scaled_w, 
                        x_limit_low = input$x_limit_w_low, x_limit_high = input$x_limit_w_high, 
                        input$current_tab,
                        input$show_smoother_w, ylabel = 1, outtype = '')
    }
    return(pl)
  }) #end function making case/deaths plot
  
  ###########################################
  #function that makes testing plot for world tab
  ###########################################
  output$world_testing_plot <- renderPlotly({
    pl <- NULL
    if ('OWID' %in% input$source_selector_w && input$current_tab == "world")
    {
      #create plot
      pl <- make_plotly(world_dat, input$country_selector, input$source_selector_w, input$case_death_w, input$daily_tot_w,
                        input$xscale_w, input$yscale_w, input$absolute_scaled_w, 
                        x_limit_low = input$x_limit_w_low, x_limit_high = input$x_limit_w_high, 
                        input$current_tab,
                        input$show_smoother_w, ylabel = 2, outtype = 'Test_All')
    }
    return(pl)
  }) #end function making testing plot
  
  
  ###########################################
  #function that makes testing positive fraction plot for world tab
  ###########################################
  output$world_testing_frac_plot <- renderPlotly({
    pl <- NULL
    if ('OWID' %in% input$source_selector_w && input$current_tab == "world")
    {
      pl <- make_plotly(world_dat, input$country_selector, input$source_selector_w, input$case_death_w, input$daily_tot_w,
                        input$xscale_w, input$yscale_w, input$absolute_scaled_w, 
                        x_limit_low = input$x_limit_w_low, x_limit_high = input$x_limit_w_high, 
                        input$current_tab,
                        input$show_smoother_w, ylabel = 3, outtype = 'Positive_Prop')
    }
    return(pl)
  }) #end function making testing plot
  
  ###########################################
  #function that makes case/death  for county tab
  ###########################################
  output$county_case_death_plot <- renderPlotly({
    #JHU data only
    pl <- NULL
    if (input$current_tab == "county")
    {
      pl <- make_plotly(county_dat, input$county_selector, "JHU", input$case_death_c, input$daily_tot_c, 
                        "x_time", input$yscale_c, input$absolute_scaled_c, 
                        x_limit_low = input$x_limit_c_low, x_limit_high = input$x_limit_c_high, 
                        input$current_tab, input$show_smoother_c, ylabel = 1, outtype = '')
    }
    return(pl)
  }) #end function making case/deaths plot
  
} #end server function


#################################
# Define UI
#################################
ui <- fluidPage(
  tags$head(includeHTML(here("www","google-analytics.html"))), #this is for Google analytics tracking.
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  includeCSS(here("www","appstyle.css")),
  #main tabs
  navbarPage( title = "COVID-19 Tracker", id = 'current_tab', selected = "us", header = "",
              tabPanel(title = "US States", value = "us",
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput('state_selector'),
                           uiOutput('source_selector'),
                           shiny::radioButtons("case_death", label = h4("Outcome"), choices = list("Cases" = "Cases", "Hospitalizations" = "Hospitalized", "Deaths" = "Deaths"), selected = "Cases"),
                           shiny::radioButtons("daily_tot", label = h4("Show daily or cumulative numbers"), choices = list("Daily" = "Daily", "Cumulative" = "Total"), selected = "Daily"),
                           shiny::radioButtons("show_smoother", label = h4("Add trend line"), choices = list("No" = "No", "Yes" = "Yes"), selected = "No"),
                           shiny::radioButtons("absolute_scaled", label = h4("Absolute or scaled values"), choices = list("Absolute Number" = "absolute", "Per 100,000 persons" = "scaled"), selected = "absolute"),
                           shiny::radioButtons("xscale", label = h4("Set x-axis to calendar date or days since a specified total number of cases/hospitalizations/deaths"), choices = list("Calendar Date" = "x_time", "Days since N cases/hospitalizations/deaths" = "x_count"), selected = "x_time"),       
                           sliderInput(inputId = "x_limit_low", "Select a date or outcome value from which to start the plots.", min = mindate,  max = Sys.Date(), value = defaultdate ),
                           sliderInput(inputId = "x_limit_high", "Select a date or outcome value at which to end the plots.", min = mindate,  max = Sys.Date(), value = Sys.Date() ),
                           shiny::radioButtons("yscale", label = h4("Y-scale"), choices = list("Linear" = "lin", "Logarithmic" = "log"), selected = "lin"),
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
                            uiOutput('state_selector_c'),
                            uiOutput('county_selector'),
                            shiny::radioButtons("case_death_c", label = h4("Outcome"), choices = list("Cases" = "Cases", "Deaths" = "Deaths"), selected = "Cases"),
                            shiny::radioButtons("daily_tot_c", label = h4("Show daily or cumulative numbers"), choices = list("Daily" = "Daily", "Cumulative" = "Total"), selected = "Daily"),
                            shiny::radioButtons("show_smoother_c", label = h4("Add trend line"), choices = list("No" = "No", "Yes" = "Yes"), selected = "No"),
                            shiny::radioButtons("absolute_scaled_c", label = h4("Absolute or scaled values"), choices = list("Absolute Number" = "absolute", "Per 100,000 persons" = "scaled"), selected = "absolute"),
                            sliderInput(inputId = "x_limit_c_low", "Select a date or outcome value from which to start the plots.", min = mindate,  max = Sys.Date(), value = defaultdate ),
                            sliderInput(inputId = "x_limit_c_high", "Select a date or outcome value at which to end the plots.", min = mindate,  max = Sys.Date(), value = Sys.Date() ),
                            shiny::radioButtons("yscale_c", label = h4("Y-scale"), choices = list("Linear" = "lin", "Logarithmic" = "log"), selected = "lin")
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
                            uiOutput('country_selector'),
                            uiOutput('source_selector_w'),
                            shiny::radioButtons("case_death_w", label = h4("Outcome"), choices = list("Cases" = "Cases", "Deaths" = "Deaths"), selected = "Cases"),
                            shiny::radioButtons("daily_tot_w", label = h4("Show daily or cumulative numbers"), choices = list("Daily" = "Daily", "Cumulative" = "Total"), selected = "Daily"),
                            shiny::radioButtons("show_smoother_w", label = h4("Add trend line"), choices = list("No" = "No", "Yes" = "Yes"), selected = "No"),
                            shiny::radioButtons("absolute_scaled_w", label = h4("Absolute or scaled values"), choices = list("Absolute Number" = "absolute", "Per 100,000 persons" = "scaled"), selected = "absolute"),
                            shiny::radioButtons("xscale_w", label = h4("Set x-axis to calendar date or days since a specified total number of cases/hospitalizations/deaths"), choices = list("Calendar Date" = "x_time", "Days since N cases/hospitalizations/deaths" = "x_count"), selected = "x_time"),       
                            sliderInput(inputId = "x_limit_w_low", "Select a date or outcome value from which to start the plots.", min = mindate,  max = Sys.Date(), value = defaultdate ),
                            sliderInput(inputId = "x_limit_w_high", "Select a date or outcome value at which to end the plots.", min = mindate,  max = Sys.Date(), value = Sys.Date() ),
                            shiny::radioButtons("yscale_w", label = h4("Y-scale"), choices = list("Linear" = "lin", "Logarithmic" = "log"), selected = "lin"),
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
                              "For more details on each data source, see their respective websites. Note that some data sources only report some data. Also, numbers might not be reliable. We do light cleaning (e.g. remove negative values), but otherwise just take the data from the sources and display it."
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



# Create Shiny object
shinyApp(ui = ui, server = server)