---
title: "MSCA 31007 Statistical Analysis - Group Assignment 1"
author: "Aashish Singh, Alexander Saucedo, Prinu Mathew, Nyckeisha' Sam, Qingwei Zhang"
date: "10/07/2022"
output:
pdf_document: default
html_document: default
word_document: default
---

#### Reference links

#### https://walker-data.com/census-r/mapping-census-data-with-r.html
#### https://api.census.gov/data/2019/acs/acs5/profile/variables.html
#### https://walker-data.com/isds-webinar/#21

##########################################STEP 1 & 2#####################################################################

install.packages(c("tidycensus", "tidyverse", "gridExtra"))
install.packages("lmtest")
library(tidycensus)
library(tidyverse)
library(sf)           # Objects and functions for geospatial data
library(rgdal)        # Functions for spatial data input/output
library(ggplot2)      # Graphing functions
library(dplyr)        # Functions for processing tabular data
library(tidyr)        # Functions for processing tabular data
library(scales)       # Additional graphics functions
library(RColorBrewer) # Color ramps for graphs and maps
library(gridExtra)    # Functions for arranging multiple plots on a page
library(readr)        # Functions for reading data
library(lmtest)

#install the key for use in future R sessions
census_api_key("55b53f404b474d711439ed9420212277bb70f1b1", overwrite = TRUE, install = TRUE)

##########################################STEP 3#####################################################################

#Bring in tract-level data from the 2015-2019 American Community Survey 5-year estimates for Cook County, IL. 
#Include the shapefile geometries, and format your output as a ‘wide’ table, not a ‘tidy’ table.

#*********** a) Define ACS variables *************************************
acs_var <- c('DP05_0001E','DP05_0018E','DP03_0062E','DP02_0065PE','DP03_0096PE','DP03_0128PE','DP04_0047PE')

#bring in tract-level data from the 2015-2019 American Community Survey (ACS) 5-year estimates for Cook County, IL
#5-year ACS with the argument survey = "acs5", starting from 2015 till 2019

census_tidy_2015_2019 <- get_acs(
  geography = "tract", variables = acs_var, county = "Cook", state = "IL", 
  year = 2019, geometry = TRUE, survey = "acs5")

#*********** c) Drop the columns which report margin of error *************************************

drop_columns <- c("moe")
census_tidy_dropcols_2015_2019 <- census_tidy_2015_2019[,!(names(census_tidy_2015_2019) %in% drop_columns)]

#remove the rows from datafranme that contains at least one NA
census_tidy_final_2015_2019 <- na.omit(census_tidy_dropcols_2015_2019)

#format your output as a ‘wide’ table, not a ‘tidy’ table
census_wide_2015_2019 <- census_tidy_final_2015_2019 %>% pivot_wider(names_from = 'variable', values_from = c('estimate'))

#*********** d) Rename the remaining columns *************************************
#*DP02_0065P -> propbac  (Bachelor's degree)
#*DP03_0062  -> medhhinc (Median household income)
#*DP03_0096P -> propcov  (Health insurance coverage)
#*DP03_0128P -> proppov  (PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL)
#*DP04_0047P -> proprent (Renter-occupied)
#*DP05_0001  -> totpop   (Total population)
#*DP05_0018  -> medage   (Median age)

census_wide_final_2015_2019 <- census_wide_2015_2019 %>%rename(
  'geoid' = 'GEOID', 'name' = 'NAME', 'propbac' = 'DP02_0065P',
  'medhhinc' = 'DP03_0062', 'propcov' = 'DP03_0096P', 'proppov' = 'DP03_0128P', 
  'proprent' = 'DP04_0047P', 'totpop' = 'DP05_0001', 'medage' = 'DP05_0018')

#remove the rows from datafrane that contains at least one NA
census_wide_final_2015_2019 <- na.omit(census_wide_final_2015_2019)

##########################################STEP 4#####################################################################
#Create a map of tract-level baccalaureate attainment rates within Cook County. Feel free to
#use either base:: R commands such as plot() or tidyverse:: commands such as
#ggplot(). Make the graph as close to publication-ready as you can.

#*********** Using plot command *************************************
plot(census_wide_final_2015_2019["propbac"], 
     main = "Tract-level baccalaureate attainment rates", 
     sub ="Cook County, Illinois")

#*********** Using ggplot command *************************************
ggplot(
  data = census_wide_final_2015_2019, aes(fill = propbac)) +
    geom_sf() + 
    scale_fill_distiller(
      palette = "YlGnBu", 
      direction = 1, 
      breaks = pretty_breaks()) +
      labs(title="Tract-level baccalaureate attainment rates",
      subtitle = "Cook County, Illinois",
      caption = "Data: 2015-2019 5-year ACS, US Census Bureau",
      fill = "Percentage") +
    theme_bw() + 
    theme(
      panel.border = element_blank(), panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "grey", color = NA)
)

##########################################STEP 5#####################################################################
#Create a linear model object in which median household income explains baccalaureate
#attainment rates at the tract level, using the lm() command. Summarize the model object
#using the summary() command

census_wide_data_2015_2019.lm <- lm(propbac ~ medhhinc, data = census_wide_final_2015_2019)
summary(census_wide_data_2015_2019.lm)

# Create an x-y plot showing how median household income can help to explain 
# baccalaureate attainment at the tract-level. 
# Add the trend line suggested by your model. Feel free to use either base::
# R commands such as plot() or tidyverse::commands such as ggplot(). 
# Make the graph as close to publication-ready as you can.

#*********** Using plot command *************************************
plot(census_wide_final_2015_2019$medhhinc, census_wide_final_2015_2019$propbac, 
     pch = 16, cex = 0.8, col='steelblue',
     main = "MEDIAN HOUSEHOLD INCOME AGAINST\nBACCALAUREATE ATTAINMENT RATE", 
     xlab = "Median Household Income ($)", 
     ylab = "Baccalaureate Attainment Rate (%)")

abline(census_wide_data_2015_2019.lm)

#*********** Using ggplot command *************************************
ggplot(census_wide_final_2015_2019, aes(x=medhhinc, y=propbac)) +
  geom_point(color='steelblue',) +
  geom_smooth(method='lm', formula= y~x, se=FALSE, color='turquoise4')  +
  theme_minimal() +
  labs(x='Median Household Income ($)', 
       y='Baccalaureate Attainment Rate (%)', 
       title='MEDIAN HOUSEHOLD INCOME AGAINST\nBACCALAUREATE ATTAINMENT RATE') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 

##########################################STEP 6#####################################################################

#6) Restore the dataset, code, and linear model that you made last week. Test the residuals of the linear model for:
#a) Normality

#Check the normality assumption is by creating a Q-Q plot
plot(census_wide_data_2015_2019.lm)

#Create a Histogram of the Residuals I\
hist(census_wide_data_2015_2019.lm$residuals, main = "Residual Histogram of Baccalaureate Attainment Rate Model")

#Normality assumption is violated because residual are not normally distributed because it's more skewed towards right
#More observations of lower baccalaureate attainment rate towards the residual zero for particular median household incomes

#b) Serial correlation
# H0 -> There is no first order serial correlation among the residuals
# H1 -> There is first order serial correlation in residuals
dwtest(formula = census_wide_data_2015_2019.lm,  alternative = "two.sided")

# Since p-value = 0.00559 is less the significance threshold (alpha) of 0.05, 
# we reject NULL hypothesis and accept the alternative hypothesis 

#c) Heteroskedasticity
#Heteroscedasticity is the situation in which the variance of the residuals of a regression model 
#is not the same across all values of the predicted variable
#H0 -> Residuals are distributed with equal variance (i.e homoscedasticity)
#H1 -> Residuals are distributed with unequal variance (i.e heteroskedasticity)
lmtest::bptest(census_wide_data_2015_2019.lm)

#Since p-value < 2.2e-16 is less the significance threshold (alpha) of 0.05, we reject NULL hypothesis and accept the alternative hypothesis 
#BASICALLY normality, serial correlation, heteroskedasticityassumptions are violated, hence the linear regression model is not the best fit for this data points

##########################################STEP 7#####################################################################
#Generate 10,000 samples of simulated tract-level household incomes, each the same size as
#your Cook County dataset. 
samples <- sample(x=census_wide_final_2015_2019$medhhinc, size=10000, replace=TRUE)
samples


#Perform a ‘non-parametric bootstrap’ in which the values are
#sampled randomly with replacement from your data
BT <- np.boot(x = samples, statistic = median)
BT

#a) Determine what proportion of the 10,000 samples show a stronger link between the
#(simulated) tract-level incomes and th
