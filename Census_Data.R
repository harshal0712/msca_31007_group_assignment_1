###############################################################################################################################

#Reference links

#https://walker-data.com/census-r/mapping-census-data-with-r.html
#https://api.census.gov/data/2019/acs/acs5/profile/variables.html
#https://walker-data.com/isds-webinar/#21
  
###############################################################################################################################

##########################################STEP 1 & 2#####################################################################

install.packages("tidycensus")
library(tidycensus)

#install the key for use in future R sessions
census_api_key("0c4a2a2815a8d526966f2490024ef157e19478db", overwrite = TRUE, install = TRUE)

##########################################STEP 3#####################################################################

#define ACS variables
acs_var <- c('DP05_0001E','DP05_0018E','DP03_0062E','DP02_0065PE','DP03_0096PE','DP03_0128PE','DP04_0047PE')

#bring in tract-level data from the 2015-2019 American Community Survey (ACS) 5-year estimates for Cook County, IL
dc_income_2015 <- get_acs(
  geography = "tract", 
  variables = acs_var,
  county = "Cook",
  state = "IL", 
  year = 2015,
  geometry = TRUE,
)

dc_income_2015

#format your output as a ‘wide’ table, not a ‘tidy’ table
reshape(dc_income_2015, idvar = "GEOID", timevar = "GEOID", direction = "wide")


plot(dc_income_2015["estimate"])