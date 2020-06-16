# air and surface decay-of-SARS-CoV-2-by-US-city
Significance: Make it easier for public health practioners or members of public to get location-specific airborne or surface decay (50%, 90%, or 99%) time estimates for COVID-19 based on current weather forecasts and recent DHS models

Purpose: Develop R shiny app to provide airborne and surface decay estimates for specific US cities based on latest weather forecasts

## Data Sources:
### Estimated SARS-CoV-2 (COVID-19) Airborne Decay from DHS
https://www.dhs.gov/science-and-technology/sars-airborne-calculator
### Estimated SARS-CoV-2 (COVID-19) Surface Decay from DHS
https://www.dhs.gov/science-and-technology/sars-calculator
### National Weather Service (NWS) API
https://www.weather.gov/documentation/services-web-api
### National Weather Service Ultraviolet (UV) Index 
https://www.weather.gov/rah/uv

https://www.cpc.ncep.noaa.gov/products/stratosphere/uv_index/bulletin.txt

## Programs in repo:
* `percent_decay_basescript.R` - base script to pull data from relevant API and implement functions originally described by DHS
* `app.R` - implement Shiny app that performs basescript actions and shows data to user in single table
