# ---
# title: "MSCA 31007 Statistical Analysis - Group Assignment 1"
# author: "Aashish Singh, Alexander Saucedo, Prinu Mathew, Nyckeisha' Sam, Qingwei Zhang"
# date: "10/24/2022"
# ---

# Reference links
# https://walker-data.com/census-r/mapping-census-data-with-r.html
# https://api.census.gov/data/2019/acs/acs5/profile/variables.html
# https://walker-data.com/isds-webinar/#21

# ---
# Perform Step 1 & Step 2
# Install all required packages and install census api key
# ---

#install.packages(c("tidycensus", "tidyverse", "gridExtra"))
#install.packages("lmtest")
#install.packages("nptest")
#install.packages("MASS")
#install.packages("olsrr")
library("olsrr")
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
library(nptest)
library(MASS)

# Enter Census API Key
census_api_key("55b53f404b474d711439ed9420212277bb70f1b1", overwrite = TRUE, install = TRUE)

# ---
# Perform Step 3
# Bring in tract-level data from the 2015-2019 American Community Survey 5-year estimates for Cook County, IL. 
# Include the shapefile geometries, and format your output as a ‘wide’ table, not a ‘tidy’ table.
# ---

#*********** a) Define ACS variables *************************************
acs_var <- c('DP05_0001E','DP05_0018E','DP03_0062E','DP02_0065PE','DP03_0096PE','DP03_0128PE','DP04_0047PE')

#bring in tract-level data from the 2015-2019 American Community Survey (ACS) 5-year estimates for Cook County, IL
#5-year ACS with the argument survey = "acs5", starting from 2015 till 2019
census_tidy_2015_2019 <- get_acs(
  geography = "tract", variables = acs_var, county = "Cook", state = "IL", year = 2019, geometry = TRUE, survey = "acs5"
)

#str(census_tidy_2015)

#*********** c) Drop the columns which report margin of error *************************************
drop_columns <- c("moe")
census_tidy_dropcols_2015_2019 <- census_tidy_2015_2019[,!(names(census_tidy_2015_2019) %in% drop_columns)]

#remove the rows from datafranme that contains at least one NA
census_tidy_final_2015_2019 <- na.omit(census_tidy_dropcols_2015_2019)

#format your output as a ‘wide’ table, not a ‘tidy’ table
census_wide_2015_2019 <- census_tidy_final_2015_2019 %>% pivot_wider(names_from = 'variable', values_from = c('estimate'))

#str(dc_income_wide_2015)

#*********** d) Rename the remaining columns *************************************
#*DP02_0065P -> propbac  (Bachelor's degree)
#*DP03_0062  -> medhhinc (Median household income)
#*DP03_0096P -> propcov  (Health insurance coverage)
#*DP03_0128P -> proppov  (PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL)
#*DP04_0047P -> proprent (Renter-occupied)
#*DP05_0001  -> totpop   (Total population)
#*DP05_0018  -> medage   (Median age)

census_wide_final_2015_2019 <- census_wide_2015_2019 %>%  
  rename('geoid' = 'GEOID', 'name' = 'NAME', 'propbac' = 'DP02_0065P', 'medhhinc' = 'DP03_0062', 'propcov' = 'DP03_0096P', 'proppov' = 'DP03_0128P', 
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

#census_2015[census_wide_final_2015_2019$variable=='medhhinc',]


#*********** Using ggplot command *************************************
ggplot(data = census_wide_final_2015_2019, aes(fill = propbac)) +
  geom_sf() + 
  scale_fill_distiller(palette = "YlGn", 
                       direction = 1, 
                       breaks = pretty_breaks()) +
  labs(title="Tract-level baccalaureate attainment rates",
       subtitle = "Cook County, Illinois",
       caption = "Data: 2015-2019 5-year ACS, US Census Bureau",
       fill = "Percentage") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", color = NA))
theme_minimal()

##########################################STEP 5#####################################################################
#Create a linear model object in which median household income explains baccalaureate
#attainment rates at the tract level, using the lm() command. Summarize the model object
#using the summary() command

census_wide_data_2015_2019.lm <- lm(propbac ~ medhhinc, data = census_wide_final_2015_2019)
summary(census_wide_data_2015_2019.lm)


#Create an x-y plot showing how median household income can help to explain baccalaureate attainment at the tract-level. 
#Add the trend line suggested by your model. Feel free to use either base:: R commands such as plot() or tidyverse::
#commands such as ggplot(). Make the graph as close to publication-ready as you can.

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
       title='Tract-level Baccalaureate Attainment Rate') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 


# ---
# Perform Step 6
# Restore the dataset, code, and linear model that you made last week. Test the residuals of the linear model for:
# a) Normality
# b) Serial correlation
# c) Heteroskedasticity
# Use both plots and hypothesis tests to evaluate these assumptions and be prepared to discuss 
# your findings. 
# ---

# a) Normality

# Check the normality assumption by creating a Q-Q plot:
ols_plot_resid_qq(census_wide_data_2015_2019.lm)
# If the Q-Q plot forms a diagonal line, you can assume that the residuals follow a normal distribution.
# And from the plot we can see that our linear model does not follow a diagonal line properly.

# Let's plot a histogram of the residuals to observe this further:
hist(census_wide_data_2015_2019.lm$residuals, main = "Residual Histogram of Baccalaureate Attainment Rate Model")
# The histogram above shows there is a right skewness. Therefore, we conclude that residuals 
# of our regression model does not follow a normal distribution.

# Also, using hypothesis testing to evalaute normality assumptions:
# H0 -> The residuals follow a normal distribution
# H1 -> The residual does not follow a normal distribution
ols_test_normality(census_wide_data_2015_2019.lm)
# As the data has more than 50 observation, We should use the Kolmogorov-Smirnov test to examine the normality of the residuals. 
# Because the p-value is below 0.05, we reject the null hypothesis and conclude that the residuals do not follow a normal distribution.


# b) Serial correlation

# Using hypothesis testing to evalaute serial correlation assumptions:
# H0 -> There is no first order serial correlation among the residuals
# H1 -> There is first order serial correlation in residuals
dwtest(formula = census_wide_data_2015_2019.lm,  alternative = "two.sided")
# Since p-value = 0.00559, which means p-value < 0.05, thus we reject the NULL hypothesis 
# We conclude that there is serial correlation present.


# c) Heteroskedasticity

# Heteroscedasticity is the situation in which the variance of the residuals of a regression model 
# is not the same across all values of the predicted variable.
# Check the heteroskedasticity assumption by creating a plot:
plot(census_wide_data_2015_2019.lm,which = 1)
# The plot shows a clear deviation from horizontal line meaning there is heteroscedasticity. 

# Also, using hypothesis testing to evalaute heteroskedasticity assumptions:
# H0 -> Residuals are distributed with equal variance (i.e homoscedasticity)
# H1 -> Residuals are distributed with unequal variance (i.e heteroskedasticity)
lmtest::bptest(census_wide_data_2015_2019.lm)
# Since p-value < 2.2e-16 is less the significance threshold (alpha) of 0.05, 
# we reject NULL hypothesis and  residuals are distributed with unequal variance
# thus the residual distribution is heteroskedasticity.

##########################################STEP 7#####################################################################
#Generate 10,000 samples of simulated tract-level household incomes, each the same size as your Cook County dataset. 
#Perform a ‘non-parametric bootstrap’ in which the values are sampled randomly with replacement from your data

#create a empty vector
npbs_sample_correlations <- c()

#set seed to get consistent results
set.seed(20220110)
sample_size <- 10000
batch_size <- nrow(census_wide_final_2015_2019)
for(j in 1:sample_size){
  census_wide_final_2015_2019["medhhinc_simulated"] <- sample(x=census_wide_final_2015_2019$medhhinc, size=batch_size, replace=TRUE)
  
  #append correlation of each sample into vector
  npbs_sample_correlations[j] = cor(x=census_wide_final_2015_2019$propbac, y=census_wide_final_2015_2019$medhhinc_simulated)
}
drop_columns <- c("medhhinc_simulated")
census_wide_final_2015_2019 <- census_wide_final_2015_2019[,!(names(census_wide_final_2015_2019) %in% drop_columns)]

#a) Determine what proportion of the 10,000 samples show a stronger link between the (simulated) tract-level incomes and 
#the (actual) tract-level baccalaureate attainment rates.

#get the actual correlation from original data frame
true_correlation_from_census_wide_final_2015_2019 <- cor(x=census_wide_final_2015_2019$propbac, y=census_wide_final_2015_2019$medhhinc)


proportion <- sum(true_correlation_from_census_wide_final_2015_2019 < npbs_sample_correlations)/sample_size

#Value of 0 means there is no stronger link between the (simulated) tract-level incomes and the (actual) tract-level baccalaureate attainment rates
#What we are thinking is since true correlation is coming from true data source, generating sample correlation from sample data is not having 
#stronger link between the (simulated) tract-level incomes and the (actual) tract-level baccalaureate attainment rates
#Tract-level baccalaureate attainment rates is strongly linked to actual income and not simulated income and it's not by chance


##########################################STEP 8#####################################################################
#Plot the distribution of correlations between your samples and tract-level baccalaureate attainment. 
#Make the graph as close to publication-ready as you can.

hist(npbs_sample_correlations, main = "Simulated Histogram of Median Household income")


#a) Identify the distribution of these correlations to the best of your ability. Be prepared to discuss the implications of 
#these simulated correlations with respect to the possible presence of a link between income and educational achievement.

#The simulated data point is NORAMLLY DISTRIBUTED
fitdistr(npbs_sample_correlations, "normal")

##########################################STEP 10#####################################################################
#Plot how the SSE (sum of squared errors) of the linear model would change for different values of the slope on median household income, 
#keeping the same intercept as in the original model. In other words, the numerical value of the slope will be plotted on the x-axis. Make
#the graph as close to publication-ready as you can

sample_size <- nrow(census_wide_final_2015_2019)

true_predictor <- census_wide_final_2015_2019$medhhinc
true_response <- census_wide_final_2015_2019$propbac

#------------------------------------BEGIN Mathematically generating the regression line for actual data---------------------------#
sum_x <- sum(true_predictor)
sum_y <- sum(true_response)
sum_x_sqr <- sum(true_predictor^2)
sum_y_sqr <- sum(true_response^2)
sum_xy <- sum(true_predictor*true_response)

SS_xy <- sum_xy - (sum_x*sum_y/sample_size)
SS_xx <- sum_x_sqr - (sum_x^2/sample_size)

#slope of actual data (medhhinc versus propbac)
slope_b_actual_data <- SS_xy/SS_xx
#intercept of actual data (medhhinc versus propbac)
intercept_a_actual_data <- (sum_y - slope_b_actual_data*sum_x)/sample_size

#regression line of actual data (medhhinc versus propbac)
yhat_actual_regression_line <-  intercept_a_actual_data + slope_b_actual_data*true_predictor

SSE_actual_data <- sum((true_response-yhat_actual_regression_line)^2)
SSE_actual_data

SSR_actual_data <- sum((yhat_actual_regression_line - mean(true_response))^2)
SSR_actual_data

SST_actual_data <- SSR_actual_data + SSE_actual_data
SST_actual_data

r_squared_actual_data <- SSR_actual_data/SST_actual_data
r_squared_actual_data

#Plotted regression line from actual median household income versus baccalaureate attainment rate
plot(x=census_wide_final_2015_2019$medhhinc, y=census_wide_final_2015_2019$propbac, 
     pch = 16, cex = 0.8, col='steelblue',
     main = "Actual regression line", 
     xlab = "Actual median household income", 
     ylab = "Baccalaureate attainment rate")

abline(lm(propbac ~ medhhinc, data = census_wide_final_2015_2019))

#------------------------------------END Mathematically generating the regression line for actual data---------------------------#

#------------------------------------BEGIN Mathematically generating the regression line for simulated data---------------------------#
#set seed to get consistent results
SSE_simulated_list <- c()
slope_b_simulated_list <- c()
set.seed(20220110)
sample_size <- 10000
batch_size <- nrow(census_wide_final_2015_2019)
for(j in 1:sample_size){
  simulated_predictor <- sample(x=census_wide_final_2015_2019$medhhinc, size=batch_size, replace=TRUE)
  true_response <- census_wide_final_2015_2019$propbac
  
  sum_x <- sum(simulated_predictor)
  sum_y <- sum(true_response)
  sum_x_sqr <- sum(simulated_predictor^2)
  sum_y_sqr <- sum(true_response^2)
  sum_xy <- sum(simulated_predictor*true_response)
  
  SS_xy <- sum_xy - (sum_x*sum_y/sample_size)
  SS_xx <- sum_x_sqr - (sum_x^2/sample_size)
  
  #slope of simulated data (simulated medhhinc versus propbac)
  slope_b_simulated_data <- SS_xy/SS_xx
  slope_b_simulated_list[j] <- slope_b_simulated_data
  
  #regression line of actual data (simulated medhhinc versus propbac)
  yhat_simulated_regression_line <-  intercept_a_actual_data + slope_b_simulated_data*simulated_predictor
  
  #add the simulated sum of squared errors of the linear model from simulated data
  SSE_simulated_list[j] <- sum((true_response-yhat_simulated_regression_line)^2)
}
#------------------------------------END Mathematically generating the regression line for simulated data---------------------------#

#Plotted regression line from simulated median household income versus baccalaureate attainment rate
plot(x=slope_b_simulated_list, y=SSE_simulated_list, 
     pch = 16, cex = 0.8, col='steelblue',
     main = "SSE versus Slope on simulated median household income", 
     xlab = "Slope on simulated median household income", 
     ylab = "SSE")

abline(lm(SSE_simulated_list ~ slope_b_simulated_list))
