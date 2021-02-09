


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

#function to make static colors for datasets
make_colors <- function(df){
  #designate colors and list length 
  location <- unique(df$location)
  color_tag = hcl(h=seq(1,360,length.out=length(location)),c=100,l=100)
  #make color dataframe
  color_frame <- data.frame(location, color_tag)
  #color_frame$color_tag <- as.character(as.factor(color_frame$color_tag))
  #color_frame$location <- as.character(as.factor(color_frame$location))
  #merge colors with tracker data by location
  add_colors <- left_join(df, color_frame, by = "location")
  return(add_colors)
}

#################################
# Function to get and clean all data
# for speed, we only get data from the online source if the data is old, otherwise we load locally
#################################

# to ensure data gets refreshed on server, we need this
get_data <- function(check_for_data = T)
{
  filename = here("data",paste0("clean_data_",Sys.Date(),'.rds')) #if the data file for today is here, load then return from function
  if (file.exists(filename) & check_for_data) {
    all_data <- readRDS(file = filename)    
    return(all_data)  
  }
  #if data file is not here, go through all of the below
  
  #data for population size for each state/country so we can compute cases per 100K
  us_popsize <- readRDS(here("data","us_popsize.rds")) %>%
    rename(state_abr = state, state = state_full, pop_size = total_pop)
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
  # print('starting USAFacts')
  #  usafct_case_data <- readr::read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv")
  #  usafct_death_data <- readr::read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv")
  
  #  state_df = usafct_case_data %>% 
  #    distinct(stateFIPS, .keep_all = TRUE) %>% 
  #    select(State,stateFIPS)
  
  #  usafct_case_clean <- clean_usafacts_p1(usafct_case_data, state_df) %>%
  #   tidyr::pivot_longer(-State, names_to = "Date", values_to = "Total_Cases") %>%
  #    clean_usafacts_p2(us_popsize) %>%
  #    group_by(Location) %>% arrange(Date) %>%
  #    mutate(Daily_Cases = c(0,diff(Total_Cases)))
  
  #  usafct_death_clean <- clean_usafacts_p1(usafct_death_data, state_df) %>%  
  #    tidyr::pivot_longer(-State, names_to = "Date", values_to = "Total_Deaths") %>%
  #    clean_usafacts_p2(us_popsize) %>%
  #   group_by(Location) %>% arrange(Date) %>%
  #    mutate(Daily_Deaths = c(0,diff(Total_Deaths)))
  
  #  usafct_clean <- left_join(usafct_case_clean, usafct_death_clean) %>%
  #  group_by(Location) %>% 
  # arrange(Date)  %>%
  #  ungroup()
  
  #add all US by summing over all variables
  #  all_us <- add_US(usafct_clean)
  # usafct_clean = rbind(usafct_clean,all_us)
  #reformat to long
  #  usafct_clean <- to_long(usafct_clean)
  
  #################################
  # pull US data from JHU github and process
  #################################
  print('starting JHU')
  us_jhu_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
  us_jhu_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
  # Clean cases
  us_jhu_cases_clean <- clean_us_jhu(us_jhu_cases) %>%
    tidyr::pivot_longer(cols = c(-state, -FIPS, -Admin2), names_to = "Date", values_to = "Cases") %>%
    select(-FIPS)
  # Clean deaths
  us_jhu_deaths_clean <-clean_us_jhu(us_jhu_deaths) %>% 
    tidyr::pivot_longer(cols = c(-state, -FIPS, -Admin2), names_to = "Date", values_to = "Deaths") %>%
    select(-FIPS)
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
  county_jhu_clean <- us_jhu_total %>% select(-c(Population_Size)) %>%
    pivot_longer(cols = c(-state, -Date, -county_name), names_to = "variable", values_to = "value")
  
  #add county population numbers 
  county_jhu_clean <- inner_join(county_jhu_clean, county_popsize)
  
  #pull state data and aggregate county values
  us_jhu_clean <- us_jhu_total %>% 
    select(-county_name) %>% 
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
  world_owid_clean <<- owid_data %>% dplyr::select(total_cases, total_deaths, new_cases, new_deaths, location, date, new_tests, total_tests, total_cases_per_million) %>%
    rename(Total_Cases = total_cases, Total_Deaths = total_deaths, Daily_Cases = new_cases, Daily_Deaths = new_deaths, Location = location, Date = date, Daily_Test_All = new_tests, Total_Test_All = total_tests) %>% 
    mutate(Population_Size = Total_Cases / total_cases_per_million * 1000000) %>% #back-calculate population size
    mutate(Location = dplyr::recode(Location, "United States" = "US")) %>%
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
  #  usafct_clean$source = us_source_var[4]
  
  #combine all US data from different sources
  #also do all variable/column names in lowercase
  us_dat <- rbind(us_ct_clean, us_nyt_clean, us_jhu_clean) #usafct_clean) 
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
    rename(date = Date, county = county_name, populationsize = pop_size) %>%
    mutate(location = paste0(county,'_',state))
  
  #reorder columns
  county_dat <- county_dat[c("source","county", "state", "location", "populationsize", "date","variable","value","state_abr")]
  
  #add static colors
  #us_dat <- make_colors(us_dat)
  #world_dat <- make_colors(world_dat)
  #county_dat <- make_colors(county_dat)
  
  
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
  
  #compare all_data to previous data file
  if(file.exists(filename)){
    if(identical(all_data, readRDS(filename))) {
      
      message('Data is unchanged.')
      return(NULL)
      
    } else {
      
      message('Data has changed. Moving previous data file.')
      Sys.time() %>% 
        format(format = "%H:%M") %>% 
        paste0(tools::file_path_sans_ext(filename), '-', ., '.rds') %>% 
        file.rename(filename, .)
    }
  }
  
  #save the data
  saveRDS(all_data, filename)    
  return(all_data)
  
} # end the get-data function which pulls data from the various online sources and processes/saves  

