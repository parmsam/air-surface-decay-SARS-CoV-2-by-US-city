library(shiny)
library(tidyverse)
library(rio)
library(jsonlite)
library(httr)
library(DT)
library(lubridate)
library(glue)
# Define functions ----
#surface decay equation based on most DHS gov model available as of June 15, 2020
#https://www.dhs.gov/science-and-technology/sars-calculator
logWithBase <- function(x, base){
  return(log(x)/log(base))
}

convertToFahr <- function(temp_c){
  return((temp_c*9/5)+32)
}

sfcHalfLifeCalc <- function(temp_f_value, relative_humidity, perc_decay){
  #model constraints
  temp_f_value_orig = temp_f_value
  relative_humidity_orig = relative_humidity
  temp_f_value = ifelse(temp_f_value < 74, 74, temp_f_value)
  temp_f_value = ifelse(temp_f_value  > 95, 95, temp_f_value)
  
  relative_humidity = ifelse(relative_humidity < 20, 20, relative_humidity)
  relative_humidity = ifelse(relative_humidity > 60, 60, relative_humidity)
  # if (temp_f_value < 74){
  #  temp_f_value = 74
  # }
  # if (temp_f_value > 95){
  #   temp_f_value = 95
  # }
  # if (relative_humidity < 20){
  #   relative_humidity = 20
  # }
  # if (relative_humidity > 60){
  #   relative_humidity = 60
  # }
  
  #test cases commented out
  #temp_f_value = 74 
  #relative_humidity = 20
  constant = 32.426272
  temperature_coefficient = -0.622108
  relative_humidity_coefficient =  -0.153707
  temp_c_value = round((temp_f_value - 32) * 5/9,1)
  temp = temp_c_value
  half_life = constant + 
    (temperature_coefficient * temp) + 
    (relative_humidity_coefficient * relative_humidity)
  
  covid_half_life_hours = round(half_life,2)
  covid_half_life_days = round(half_life / 24,2)
  
  covid_perc_decay = half_life * logWithBase((1 - perc_decay), .5)
  covid_decay_hours = round(covid_perc_decay,2)
  #covid_99_99_days = round(covid_perc_decay/24,2)
  
  # covid_99_99 = half_life * logWithBase((1- .9999), .5)
  # covid_99_99_hours = round(covid_99_99,2)
  # covid_99_99_days = round(covid_99_99/24,2)
  # 
  # covid_99_9999 = half_life * logWithBase((1- .999999), .5)
  # covid_99_9999_hours = round(covid_99_9999,2)
  # covid_99_9999_days = round(covid_99_9999/24,2)
  # 
  # covid_99_999999 = half_life * logWithBase((1- .99999999), .5)
  # covid_99_999999_hours = round(covid_99_999999,2)
  # covid_99_999999_days = round(covid_99_999999/24,2)
  # 
  # df <- data.frame(
  #   `Perc Virus Decay` = c("50% (half-life)","99.99%","99.9999%","99.999999%"),
  #   Hours = c(covid_half_life_hours, covid_99_99_hours, covid_99_9999_hours, covid_99_999999_hours),
  #   Days = c(covid_half_life_days, covid_99_99_days, covid_99_9999_days, covid_99_999999_days))
  
  covid_decay_hours = ifelse(temp_f_value_orig < 74, paste(">",covid_decay_hours, sep=""),
         ifelse(temp_f_value_orig > 95, paste("<",covid_decay_hours, sep=""),
                ifelse(relative_humidity_orig < 20, paste(">",covid_decay_hours, sep=""),
                       ifelse(relative_humidity_orig > 60, paste(">",covid_decay_hours, sep=""),
                              covid_decay_hours))))
  return(covid_decay_hours)
}
#sfcHalfLifeCalc(74,57,0.5) #50% decay example w/output in hours

#airborne decay equation based on most DHS gov model available as of June 15, 2020
#https://www.dhs.gov/science-and-technology/sars-airborne-calculator
airHalfLifeCalc <- function(temp_f_value, relative_humidity_value, uv_index_value, perc_decay){
  #model constraints
  temp_f_value_orig = temp_f_value
  relative_humidity_value_orig = relative_humidity_value
  uv_index_value_orig = uv_index_value
  
  temp_f_value = ifelse(temp_f_value < 50, 50, temp_f_value)
  temp_f_value = ifelse(temp_f_value  > 86, 86, temp_f_value)
  relative_humidity_value = ifelse(relative_humidity_value < 20, 20, relative_humidity_value)
  relative_humidity_value = ifelse(relative_humidity_value > 70, 70, relative_humidity_value)
  uv_index_value = ifelse(uv_index_value == 0, 1, uv_index_value)
  
  # if (temp_f_value < 50){
  #   temp_f_value = 50
  # }
  # if (temp_f_value > 86){
  #   temp_f_value = 86
  # }
  # if (relative_humidity_value < 20){
  #   relative_humidity = 20
  # }
  # if (relative_humidity_value > 70){
  #   relative_humidity = 70
  # }
  # if (uv_index_value == 0){
  #   uv_index_value = 1
  # }
  
  #test cases commented out
  # temp_f_value = 50 
  # relative_humidity_value = 20
  # uv_index_value = 1
  solar_const = 0.000281
  solar_scaler = 5.4
  temp_rc_const = 20.54
  temp_rc_scaler = 10.66
  rh_rc_const = 45.235
  rh_rc_scaler = 28.665
  solar_rc_const = 50
  solar_rc_scaler = 50
  intercept = -7.679348
  temp_coef = -1.338432
  rh_coef = -0.017835
  solar_coef = -7.666331
  temp_solar_coef = -1.323633
  temp_rh_coef = 0
  rh_solar_coef = 0
  temp_rh_solar_coef = 0
  convert_to_time_numerator = -0.693
  
  temp_c_value = round((temp_f_value - 32) * 5/9,1)
  temp = temp_c_value
  
  rh = relative_humidity_value
  uv_index = uv_index_value
  
  solar = (uv_index + solar_const) / solar_scaler
  
  temp_rc = (temp - temp_rc_const) / temp_rc_scaler
  rh_rc = (rh - rh_rc_const) /rh_rc_scaler
  solar_rc = (solar - solar_rc_const) / solar_rc_scaler
  
  intercept_factor = intercept
  temp_factor = temp_coef * temp_rc
  rh_factor = rh_coef * rh_rc
  solar_factor = solar_coef * solar_rc
  temp_rh_factor = temp_rh_coef * temp_rc * rh_rc
  temp_solar_factor = temp_solar_coef * temp_rc * solar_rc
  rh_solar_factor = rh_solar_coef * rh_rc * solar_rc
  temp_rh_solar_factor = temp_rh_solar_coef * temp_rc * rh_rc * solar_rc
  
  k_min_denom = intercept_factor + temp_factor + rh_factor + solar_factor + temp_rh_factor + temp_solar_factor + rh_solar_factor + temp_rh_solar_factor
  
  half_life = convert_to_time_numerator/k_min_denom
  
  covid_half_life_minutes = round(half_life,2)
  covid_half_life_hours = round(half_life/60,2)
  
  
  covid_perc_decay= half_life * logWithBase((1- perc_decay), .5)
  covid_decay_minutes = round(covid_perc_decay,2)
  #covid_decay_hours = round(covid_perc_decay/60,2)
  
  # covid_90 = half_life * logWithBase((1- .90), .5)
  # covid_90_minutes = round(covid_90,2)
  # covid_90_hours = round(covid_90/60,2)
  # 
  # covid_99 = half_life * logWithBase((1- .99), .5)
  # covid_99_minutes = round(covid_99,2)
  # covid_99_hours = round(covid_99/60,2)
  
  # df <- data.frame(
  #     `Perc Virus Decay` = c("50% (half-life)","90%","99%"),
  #     Minutes = c(covid_half_life_minutes, covid_90_minutes, covid_99_minutes),
  #     Hours = c(covid_half_life_hours, covid_90_hours, covid_99_hours))
  
  covid_decay_minutes = ifelse(temp_f_value_orig < 50, paste(">",covid_decay_minutes, sep=""),
                             ifelse(temp_f_value_orig > 86, paste("<",covid_decay_minutes, sep=""),
                                    ifelse(relative_humidity_value_orig < 20, paste(">",covid_decay_minutes, sep=""),
                                           ifelse(relative_humidity_value_orig > 70, paste(">",covid_decay_minutes, sep=""),
                                                  ifelse(uv_index_value_orig == 0, paste(">",covid_decay_minutes, sep=""),
                                                  covid_decay_minutes)))))
  # if (temp_f_value_orig < 50){
  #   covid_decay_minutes = paste(">",covid_decay_minutes, sep="")
  #   #return(covid_decay_minutes)
  # }
  # else if (temp_f_value_orig > 86){
  #   covid_decay_minutes = paste("<",covid_decay_minutes, sep="")
  #   #return(covid_decay_minutes)
  # }
  # else if (relative_humidity_value_orig < 20){
  #   covid_decay_minutes = paste(">",covid_decay_minutes, sep="")
  #   #return(covid_decay_minutes)
  # }
  # else if (relative_humidity_value_orig > 70){
  #   covid_decay_minutes = paste("<",covid_decay_minutes, sep="")
  #   #return(covid_decay_minutes)
  # }
  # else if (uv_index_value_orig == 0){
  #   covid_decay_minutes = paste(">",covid_decay_minutes, sep="")
  #   #return(covid_decay_minutes)
  # }
  
  return(covid_decay_minutes)
}
#airHalfLifeCalc(30,27,1,0.5)
#sfcHalfLifeCalc(74,40,0.5)

# Get data ----
yesterday <- as.character(Sys.Date()-1)
today <- as.character(Sys.Date())
tomorrow <- as.character(Sys.Date()+1)
day_after_tmrw <-as.character(Sys.Date()+2)

state_abbrev <- read.csv("https://raw.githubusercontent.com/parmsam/air-surface-decay-SARS-CoV-2-by-US-city/master/state-abbreviations.csv", 
                         header=FALSE) %>%
  mutate(state = as.character(V1), abbrev = as.character(V2)) %>% 
  select(-V1,-V2)
  
city_list <- 
  import("https://raw.githubusercontent.com/parmsam/national-weather-service-forecasts/master/1000-largest-us-cities-by-population-with-geographic-coordinates.csv") %>% 
  separate(Coordinates, into=c("Lat","Long"),sep=",")  %>% 
  mutate(city_state=paste0(City,", ",State)) %>% 
  inner_join(.,state_abbrev, by=c("State"="state"))

####
#get timezone data for 20 states
timezone_Df <- read.csv("https://raw.githubusercontent.com/parmsam/air-surface-decay-SARS-CoV-2-by-US-city/master/tz_city_list.csv") %>% 
  mutate(city=as.character(city), state=as.character(state), tz= as.character(tz))

# UVI_data <- read.delim("https://www.cpc.ncep.noaa.gov/products/stratosphere/uv_index/bulletin.txt",
#                        row.names=NULL)
# df_UVI <- slice(UVI_data,-(1:21))
# df1_UVI <- str_split_fixed(df_UVI$row.names,regex("\\s\\s+"), n=6)
# colnames(df1_UVI) <- df1_UVI[1,]
# df1_UVI <- df1_UVI[-1,]
# df2_UVI <- rbind(df1_UVI[,1:3], df1_UVI[,4:6]) %>% data.frame() %>%
#   mutate(CITY = str_to_title(CITY), UVI = as.numeric(as.character(UVI)))

#city_select_list <- city_list %>% inner_join(df2_UVI, by = c("City"="CITY","abbrev"="STATE"))
city_select_list <- city_list %>% inner_join(timezone_Df, by = c("City"="city","abbrev"="state"))

# Define UI for application that ouptuts table of weather forecast

ui <- fluidPage(
  
  # Application title
  titlePanel("Surface and Air Decay Estimates for SARS-CoV-2 in Major Cities based on Current Weather"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("variable_1", "Select city of interest:",
                  #choices=list("",name = c("Cho"="",unique(Dat$Resource_name))),
                  choices=c("Choose one"="",sort(unique(city_select_list$city_state))),
                  #options = list(placeholder = 'Please select an option below'),
                  selected = "Indianapolis, Indiana"
                  #verbatimTextOutput("selected")
      ),
      selectInput("variable_2", "Select percent virus decay:",
                     choices=c(50,90,99,99.99,99.9999,99.999999)
      ),
      strong("Purpose:"),
      "This app is designed to make it easier for public health and public safety workers to get current location-specific SARS-CoV-2 airborne or surface decay estimates.",
      "It is based based on current weather forecasts and recent DHS models (as of June 15th, 2020).",
      br(),
      strong("Sources:"),
      "National Weather Service (NWS) API used to obtain current temperature and relative humidity estimates.",
      "EPA Envirofacts Data Service API was used to obtain hourly UV index data.",
      "Airborne and Surface Decay based on models published by Department of Homeland Security (DHS) as of June 15th, 2020.",
      "Time of day in Coordinated Universal Time (UTC).",
      br(),
      strong("Time Zone Convertor Tool:"),
      "See time zone convertor to get your city time in UTC.",
      br(),
      tags$li(a("The Time Zone Convertor", href="https://www.thetimezoneconverter.com/")),
      strong("Reference models:"),
      "See links below for more info on DHS models (background and caveats for each model).",
      br(),
      tags$li(a("SARS-CoV-2 Airborne Decay Calculator", href="https://www.dhs.gov/science-and-technology/sars-airborne-calculator")),
      "DHS Airborne Model: The tool is valid for the following ranges of conditions: 50-86°F, 20-70% relative humidity, and UV indices of 1-10. 
              The model currently doesn’t allow for a UV index of 0.",
      tags$li(a("SARS-CoV-2 Surface Decay Calculator", href="https://www.dhs.gov/science-and-technology/sars-calculator")),
      "DHS Surface Model: Model can estimate virus decay at certain conditions: temperature (room temperature or 74°F to 95°F) 
      and relative humidity from 20-60%, without exposure to direct sunlight.",
      br(),
      strong("Github repo:"),
      "See Sam's Github for more info and source code.",
      tags$li(a("Sam's Github Repo",href="https://github.com/parmsam/air-surface-decay-SARS-CoV-2-by-US-city"))
    ),
    
    # Show a data table of the weather forecast prediction for weather.gov
    mainPanel(
      DT::dataTableOutput("data")
    )
  )
)

# Define server logic required to output forecast
server <- function(input, output) {
  datPull <- reactive({
    city_data <- city_list %>% 
      filter(grepl(input$variable_1,city_state))
    
    #city_data_join <- city_data %>% inner_join(df2_UVI, by = c("City"="CITY","abbrev"="STATE"))
    city_data_join <- city_data %>% inner_join(timezone_Df, by = c("City"="city","abbrev"="state"))
    
    city_data <- city_data_join %>% select(City,State, abbrev, Lat,Long) %>% data.frame() %>% 
      select(City,State, abbrev, Lat,Long) %>% top_n(1) %>% data.frame()
    
    #today's estimated UVI
    #city_UVI <- city_data_join$UVI
    
    
    base_url = "https://api.weather.gov/points/"
    lat_long = paste0(city_data$Lat,",", city_data$Long)
    frst_url <- paste0(base_url,lat_long);

    #first call to get unique forecast api request 
    req<-httr::GET(frst_url)
    json <- httr::content(req, as = "text")
    weather_dat <- jsonlite::fromJSON(json)
    
    forecast_url <- weather_dat$properties$forecast
    forecastGridData_url <- weather_dat$properties$forecastGridData
    
    #second api call to get forecast data for city of interest
    req<-httr::GET(forecastGridData_url)
    json <- httr::content(req, as = "text")
    weather_dat <- jsonlite::fromJSON(json)
    
    #third api call for UVI data
    # epa_base_url = "https://enviro.epa.gov/enviro/efservice/getEnvirofactsUVHOURLY/CITY/"
    # city_abbrev_end = paste0(str_to_lower(city_data$City),glue("/{city_data$State}"),"/",str_to_lower(city_data$abbrev),"/JSON")
    # uvi_url <- paste0(epa_base_url,city_abbrev_end);frst_url
    uvi_url = glue::glue("https://data.epa.gov/efservice/getEnvirofactsUVHOURLY/CITY/{city_data$City}/STATE/{str_to_lower(city_data$abbrev)}/JSON")
    print(uvi_url)
    uvi_req<-httr::GET(uvi_url)
    uvi_json <- httr::content(uvi_req, as = "text")
    uvi_dat <- jsonlite::fromJSON(uvi_json)
    
    timezone_Df_filt <- timezone_Df %>% filter(str_detect(city,city_data$City))
    timezone_str = timezone_Df_filt$tz
    b <- uvi_dat %>% 
      mutate(DATE_TIME = mdy_h(DATE_TIME))
    tz(b$DATE_TIME) = timezone_str 
    #b %>% glimpse()
    c <- b %>% mutate(DATE_TIME = with_tz(DATE_TIME, "UTC")) %>% 
      rename(cityUVI=UV_VALUE)
    
    #pull tonight/tmrw/rest week weather data from rest data
    temperature <- weather_dat$properties$temperature$values %>% mutate(validTime = str_remove(validTime,"/\\w*"))
    temperature_unit <- weather_dat$properties$temperature$uom
    relativeHumidity <- weather_dat$properties$relativeHumidity$values %>% mutate(validTime = str_remove(validTime,"/\\w*"))
    relativeHumidity_unit <- weather_dat$properties$relativeHumidity$sourceUnit
    
    #only have most current UVI so will need to limit historical or future UVI declaration 
    
    #mutate(cityUVI = city_UVI) %>% 
    temp_and_humid <- relativeHumidity %>% 
      inner_join(temperature, by='validTime') %>% 
      mutate(relativeHumidity = value.x,temperature = value.y) %>% 
      mutate(temperature_unit = temperature_unit) %>% 
      select(-value.x,-value.y) %>% select(validTime, relativeHumidity, relativeHumidity_unit, 
                                           temperature, temperature_unit) %>% 
      mutate(temperature_F = convertToFahr(temperature)) %>% 
      filter(str_detect(validTime,paste(today,tomorrow,yesterday,day_after_tmrw, sep="|"))) %>%
      mutate(temperature_F = round(convertToFahr(.$temperature),1)) %>%
      mutate(validTime = as.character(ymd_hms(str_remove(validTime,"/\\w*")))) %>%
      mutate(validTime = mdy_h(strftime(validTime, format="%m-%d-%y %H"))) %>%
      inner_join(c, by=c('validTime'='DATE_TIME'))
    
    temp_and_humid <- temp_and_humid %>% 
      mutate(`Airborne Percent Virus Decay (minutes)`= airHalfLifeCalc(.$temperature_F, 
                                                                   .$relativeHumidity, 
                                                                   .$cityUVI,
                                                                   perc_decay = as.numeric(input$variable_2)/100)) %>%
      mutate(`Surface Percent Virus Decay (hours)`= sfcHalfLifeCalc(.$temperature_F, 
                                                                  .$relativeHumidity,
                                                                  perc_decay = as.numeric(input$variable_2)/100)) %>%
      mutate(validTime = as.character(validTime))
    
    temp_and_humid <- temp_and_humid %>% select(`Time (UTC)`=validTime, 
                                                `Surface Percent Virus Decay (hours)`, 
                                                `Airborne Percent Virus Decay (minutes)`,
                                                `Temperature (Fahrenheit)`= temperature_F,
                                                `Relative Humidity` = `relativeHumidity`,
                                                `UV Index` = cityUVI) 
    
    newname1 = paste("Time (hours) to breakdown of",input$variable_2,"percent of virus on surfaces",sep=" ")
    newname2 = paste("Time (minutes) to breakdown of",input$variable_2,"percent of airborne virus",sep=" ")
    
    temp_and_humid <- temp_and_humid %>% rename(!!newname1 := `Surface Percent Virus Decay (hours)`,
                                                !!newname2 := `Airborne Percent Virus Decay (minutes)`
                                                )
   
})
  
  output$data <- DT::renderDataTable({ 
    data<-datPull()
    DT::datatable(data,list(mode = "single", target = "cell"),escape = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)