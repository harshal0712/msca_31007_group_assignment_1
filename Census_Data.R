###############################################################################################################################

#Reference links

#https://walker-data.com/census-r/mapping-census-data-with-r.html
#https://api.census.gov/data/2019/acs/acs5/profile/variables.html
#https://walker-data.com/isds-webinar/#21
  
###############################################################################################################################

##########################################STEP 1 & 2#####################################################################

install.packages("tidycensus")
install.packages("tidyverse")
library(tidycensus)
library(tidyverse)

#install the key for use in future R sessions
census_api_key("0c4a2a2815a8d526966f2490024ef157e19478db", overwrite = TRUE, install = TRUE)

##########################################STEP 3#####################################################################

#*********** a) Define ACS variables *************************************
acs_var <- c('DP05_0001E','DP05_0018E','DP03_0062E','DP02_0065PE','DP03_0096PE','DP03_0128PE','DP04_0047PE')

#bring in tract-level data from the 2015-2019 American Community Survey (ACS) 5-year estimates for Cook County, IL
census_tidy_2015 <- get_acs(
  geography = "tract", variables = acs_var, county = "Cook", state = "IL", year = 2015, geometry = TRUE
)

census_tidy_2016 <- get_acs(
  geography = "tract", variables = acs_var, county = "Cook",state = "IL", year = 2016,geometry = TRUE
)

census_tidy_2017 <- get_acs(
  geography = "tract", variables = acs_var, county = "Cook", state = "IL", year = 2017, geometry = TRUE
)

census_tidy_2018 <- get_acs(
  geography = "tract", variables = acs_var, county = "Cook", state = "IL", year = 2018, geometry = TRUE
)

census_tidy_2019 <- get_acs(
  geography = "tract",   variables = acs_var,  county = "Cook",  state = "IL",   year = 2019,  geometry = TRUE
)

census_tidy_2015_2019 <- rbind(census_tidy_2015, census_tidy_2016, census_tidy_2017, census_tidy_2018, census_tidy_2019)

#str(census_tidy_2015)

#*********** c) Drop the columns which report margin of error *************************************
drop_columns <- c("moe")
census_tidy_cleaned_2015_2019 <- census_tidy_2015_2019[,!(names(census_tidy_2015_2019) %in% drop_columns)]


#format your output as a ‘wide’ table, not a ‘tidy’ table
census_wide_cleaned_2015_2019 <- census_tidy_cleaned_2015_2019 %>%
  pivot_wider(
    names_from = 'variable', 
    values_from = c('estimate')
  )

#str(dc_income_wide_2015)

#*********** d) Rename the remaining columns *************************************
#*DP02_0065P -> propbac  (Bachelor's degree)
#*DP03_0062  -> medhhinc (Median household income)
#*DP03_0096P -> propcov  (Health insurance coverage)
#*DP03_0128P -> proppov  (PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL)
#*DP04_0047P -> proprent (Renter-occupied)
#*DP05_0001  -> totpop   (Total population)
#*DP05_0018  -> medage   (Median age)

census_wide_renamed_2015_2019 <- census_wide_cleaned_2015_2019 %>%  
  rename('propbac' = 'DP02_0065P', 'medhhinc' = 'DP03_0062', 
         'propcov' = 'DP03_0096P', 'proppov' = 'DP03_0128P', 
         'proprent' = 'DP04_0047P', 'totpop' = 'DP05_0001', 
         'medage' = 'DP05_0018')


##########################################STEP 4#####################################################################
#Create a map of tract-level baccalaureate attainment rates within Cook County. Feel free to
#use either base:: R commands such as plot() or tidyverse:: commands such as
#ggplot(). Make the graph as close to publication-ready as you can.


#plot(census_2015["estimate"])
#census_2015[census_2015$variable=='DP02_0065P',]
