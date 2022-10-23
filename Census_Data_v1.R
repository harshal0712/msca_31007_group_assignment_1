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

install.packages(c("tidycensus", "tidyverse", "gridExtra"))
install.packages("lmtest")
install.packages("nptest")
install.packages("MASS")
install.packages("olsrr")
install.packages("fitdistrplus")
library(olsrr)
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
library(fitdistrplus)

# Enter Census API Key
census_api_key("0c4a2a2815a8d526966f2490024ef157e19478db", overwrite = TRUE, install = TRUE)


# ---
# Perform Step 3
# Bring in tract-level data from the 2015-2019 American Community Survey 5-year estimates for Cook County, IL. 
# Include the shapefile geometries, and format your output as a ‘wide’ table, not a ‘tidy’ table.
# ---

# a) Define specific ACS variables we need to pull
# And, bring in tract-level data from the 2015-2019 American Community Survey (ACS) 5-year estimates for Cook County, IL
# 5-year ACS with the argument survey = "acs5", starting from 2015 till 2019
acs_var <- c('DP05_0001E','DP05_0018E','DP03_0062E','DP02_0065PE','DP03_0096PE','DP03_0128PE','DP04_0047PE')
census_tidy_2015_2019 <- get_acs(
  geography = "tract", variables = acs_var, county = "Cook", state = "IL", year = 2019, geometry = TRUE, survey = "acs5"
)

# c) Drop the columns which report margin of error
census_tidy_dropcols_2015_2019 <- census_tidy_2015_2019[,!(names(census_tidy_2015_2019) %in% "moe")]
# Remove the rows from datafranme that contains at least one NA
census_tidy_final_2015_2019 <- na.omit(census_tidy_dropcols_2015_2019)
# Format your output as a ‘wide’ table, not a ‘tidy’ table
census_wide_2015_2019 <- census_tidy_final_2015_2019 %>% 
  pivot_wider(names_from = 'variable', values_from = c('estimate'))


# d) Rename the remaining columns
# DP02_0065P -> propbac  (Bachelor's degree)
# DP03_0062  -> medhhinc (Median household income)
# DP03_0096P -> propcov  (Health insurance coverage)
# DP03_0128P -> proppov  (PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL)
# DP04_0047P -> proprent (Renter-occupied)
# DP05_0001  -> totpop   (Total population)
# DP05_0018  -> medage   (Median age)
census_wide_final_2015_2019 <- census_wide_2015_2019 %>%  
  rename('geoid' = 'GEOID', 'name' = 'NAME', 'propbac' = 'DP02_0065P', 
         'medhhinc' = 'DP03_0062', 'propcov' = 'DP03_0096P', 'proppov' = 'DP03_0128P', 
         'proprent' = 'DP04_0047P', 'totpop' = 'DP05_0001', 'medage' = 'DP05_0018')
# Remove the rows from datafrane that contains at least one NA
census_wide_final_2015_2019 <- na.omit(census_wide_final_2015_2019)


# ---
# Perform Step 4
# Create a map of tract-level baccalaureate attainment rates within Cook County. Feel free to
# use either base:: R commands such as plot() or tidyverse:: commands such as
# ggplot(). Make the graph as close to publication-ready as you can.
# ---

ggplot(data = census_wide_final_2015_2019, aes(fill = propbac)) +
  geom_sf() + 
  scale_fill_distiller(palette = "YlGnBu", 
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
        panel.background = element_rect(fill = "grey", color = NA))


# ---
# Perform Step 5
# Create a linear model object in which median household income explains baccalaureate
# attainment rates at the tract level, using the lm() command. Summarize the model object
# using the summary() command
# ---

census_wide_data_2015_2019.lm <- lm(propbac ~ medhhinc, data = census_wide_final_2015_2019)
summary(census_wide_data_2015_2019.lm)

# a) Create an x-y plot showing how median household income can help to explain baccalaureate attainment at the tract-level. 
# Add the trend line suggested by your model. Feel free to use either base:: R commands such as plot() or tidyverse::
# commands such as ggplot(). Make the graph as close to publication-ready as you can.
ggplot(census_wide_final_2015_2019, aes(x=medhhinc, y=propbac)) +
  geom_point(color='steelblue',) +
  geom_smooth(method='lm', formula= y~x, se=FALSE, color='turquoise4')  +
  theme_minimal() +
  labs(x='Median Household Income ($)', 
       y='Baccalaureate Attainment Rate (%)', 
       title='Baccalaureate Attainment Rate vs Median Household Income') +
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
# Let's check for serial correlation by plotting acf:
acf(census_wide_data_2015_2019.lm$residuals, type = "correlation")
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


# ---
# Perform Step 7
# Generate 10,000 samples of simulated tract-level household incomes, each the same size as your Cook County dataset. 
# Perform a ‘non-parametric bootstrap’ in which the values are sampled randomly with replacement from your data
# ---

# First let's create a empty vector
npbs_sample_correlations <- c()
# Set seed to get consistent results
set.seed(20220110)
sample_size <- 10000
batch_size <- nrow(census_wide_final_2015_2019)
for(j in 1:sample_size){
  census_wide_final_2015_2019["medhhinc_simulated"] <- sample(x=census_wide_final_2015_2019$medhhinc, size=batch_size, replace=TRUE)
  # Append correlation of each sample into vector
  npbs_sample_correlations[j] = cor(x=census_wide_final_2015_2019$propbac, y=census_wide_final_2015_2019$medhhinc_simulated)
}
census_wide_final_2015_2019 <- census_wide_final_2015_2019[,!(names(census_wide_final_2015_2019) %in% "medhhinc_simulated")]

# a) Determine what proportion of the 10,000 samples show a stronger link between the (simulated) tract-level incomes and 
# the (actual) tract-level baccalaureate attainment rates.
# Let's get the actual correlation from original data frame
actual_correlation_from_data <- cor(x=census_wide_final_2015_2019$propbac, y=census_wide_final_2015_2019$medhhinc)
# Let's see what proportion of the 10,000 samples show a stronger correlation than actual correlation
sum(actual_correlation_from_data < npbs_sample_correlations)/sample_size
# Value of 0 means there is no stronger link between the (simulated) tract-level incomes and the (actual) tract-level baccalaureate attainment rates
# What we are thinking is since true correlation is coming from true data source, generating sample correlation from sample data is not having 
# stronger link between the (simulated) tract-level incomes and the (actual) tract-level baccalaureate attainment rates
# Tract-level baccalaureate attainment rates is strongly linked to actual income and not simulated income and it's not by chance.


# ---
# Perform Step 8
# Plot the distribution of correlations between your samples and tract-level baccalaureate attainment. 
# Make the graph as close to publication-ready as you can.
# ---

hist(npbs_sample_correlations, main = "Distribution of correlation between samples")
npbs_sample_correlations.vector <- as.vector(npbs_sample_correlations)
plot(ecdf(npbs_sample_correlations.vector))

# a) Identify the distribution of these correlations to the best of your ability. Be prepared to discuss the implications of 
# these simulated correlations with respect to the possible presence of a link between income and educational achievement.
fitdist(npbs_sample_correlations, "norm")
ks.test(npbs_sample_correlations,pnorm,mean=-8.219608e-05,sd=2.736541e-02)
# The simulated data point is normally distributed.


# ---
# Perform Step 9
# Restore the dataset, code, and linear model that you made last week.
# ---
str(census_wide_final_2015_2019)
summary(census_wide_data_2015_2019.lm)

#------------------------------------BEGIN Mathematically generating the regression line from actual data---------------------------#
y_propbac <- census_wide_final_2015_2019$propbac
x_medhhinc <- census_wide_final_2015_2019$medhhinc
census_wide_data_2015_2019.lm.summary <- summary(census_wide_data_2015_2019.lm)
actual_intercept <- census_wide_data_2015_2019.lm.summary$coefficients[1,1]
actual_slope <- census_wide_data_2015_2019.lm.summary$coefficient[2,1]
yhat_actual_regression_line <- actual_intercept + actual_slope * x_medhhinc
actual_sse <- sum((y_propbac-yhat_actual_regression_line)^2)
#------------------------------------END Mathematically generating the regression line from actual data---------------------------#

# ---
# Perform Step 10
# Plot how the SSE (sum of squared errors) of the linear model would change for different values of the slope on median household income, 
# keeping the same intercept as in the original model. In other words, the numerical value of the slope will be plotted on the x-axis. Make
# the graph as close to publication-ready as you can
# ---

# Now we want to keep the same intercept and change value of slope to see how SSE changes.
slope_iterations <- seq(from=-0.001, to=0.001, by=0.00005)
sse_iterations <- c()
y_pred_iterations <- c()
for (i in 1:length(slope_iterations)) {
  for(j in 1:nrow(census_wide_final_2015_2019)) {
    y_pred_iterations[j] <- actual_intercept + slope_iterations[i] * x_medhhinc[j]
  }
  sse_iterations[i] <- sum((y_propbac-y_pred_iterations)^2)
}
sse_dataframe <- data.frame("Slope" = c(slope_iterations),
                            "SSE" = c(sse_iterations))

#Plotted from distribution of SSE for different values of the slope on MedHHIncome
options(scipen = 999)
plot(x=sse_dataframe$Slope, y=sse_dataframe$SSE, 
     pch = 16, cex = 0.8, col='steelblue',
     main = "Distribution of SSE for different values of the slope on MedHHIncome", 
     xlab = "Slope of MedHHIncome", 
     ylab = "SSE",
     type="o")
abline(lm(propbac ~ medhhinc, data = census_wide_final_2015_2019), col="red", lty=2)


# ---
# Perform Step 11
#Plot how the log-likelihood of the model would change for different values of the intercept, keeping the same slope as 
#in the original model. In other words, the numerical value of the intercept will be plotted on the x-axis. 
#Make the graph as close to publication-ready as you can.
# ---

intercept_iterations <- seq(from=actual_intercept-1.5, to=actual_intercept+1.5, by=0.1)
loglik_simulated_list <- c()
slope_iterations <- c()
for (i in 1:length(intercept_iterations)) {
  for(j in 1:nrow(census_wide_final_2015_2019)) {
    y_pred_iterations[j] <- intercept_iterations[i] + actual_slope * x_medhhinc[j]
  }
  
  model_dataframe <- data.frame("Y" = c(y_pred_iterations),
                              "X" = c(x_medhhinc))
  
  simulated.model.lm <- lm(model_dataframe$Y ~ model_dataframe$X)
  
  #calculate the Log-likelihood of the model
  loglik_simulated_list[i] <- logLik(simulated.model.lm)
}

loglik_dataframe <- data.frame("Log_likelihood" = c(loglik_simulated_list),
                            "Intercept" = c(intercept_iterations))


#Plotted from distribution of SSE for different values of the intercept on MedHHIncome
options(scipen = 999)
plot(x=loglik_dataframe$Intercept, y=loglik_dataframe$Log_likelihood, 
     pch = 16, cex = 0.8, col='steelblue',
     main = "Distribution of Log-likelihood for different values of the intercept on MedHHIncome", 
     xlab = "Intercept of MedHHIncome", 
     ylab = "Log-likelihood",
     type="o")
abline(lm(propbac ~ medhhinc, data = census_wide_final_2015_2019), col="red", lty=2)



# ---
# Perform Step 12
#Consider a radical tax policy (call it the ‘Robin Hood’ tax) which would reduce the median household income in 
#the 50 highest-earning tracts by $10,000, and increase the median household income in the 50 lowest-earning tracts 
#by $10,000. What would the net effect on baccalaureate attainment be, according to the linear model? 
#Do you consider this realistic? Does the data suggest any evidence for or against this theory?
# ---

#sort the dataframe by column 'medhhinc' in descending order
census_wide_final_2015_2019_sorted <- census_wide_final_2015_2019 %>% arrange(desc(medhhinc))

#Generate Row number or Row index to the dataframe using seq.int()
census_wide_final_2015_2019_sorted$row_num <- seq.int(nrow(census_wide_final_2015_2019_sorted))

total_sample_size <- nrow(census_wide_final_2015_2019_sorted)

#add column simulated_medhhinc based on 50 highest-earning tracts and 50 lowest-earning tracts
robin_hood_threadhold_income <- 10000
robin_hood_threadhold_size <- 50
census_wide_final_2015_2019_sorted <- census_wide_final_2015_2019_sorted %>%
  mutate(simulated_medhhinc = case_when(
    row_num >= 1 & row_num <= robin_hood_threadhold_size  ~ medhhinc-robin_hood_threadhold_income,
    row_num > robin_hood_threadhold_size & row_num < (total_sample_size - robin_hood_threadhold_size)  ~ medhhinc,
    row_num >= (total_sample_size - robin_hood_threadhold_size) ~ medhhinc+robin_hood_threadhold_income
  ))

actual.lm.model <- lm(propbac ~ medhhinc, data = census_wide_final_2015_2019)
actual.summary.lm.model <- summary(actual.lm.model)

simulated.lm.model <- lm(propbac ~ simulated_medhhinc, data = census_wide_final_2015_2019_sorted)
simulated.summary.lm.model <- summary(simulated.lm.model)

#Plotted regression line from actual versus simulated median household income versus baccalaureate attainment rate
ggplot() +
  geom_point(data = census_wide_final_2015_2019, aes(x = medhhinc, y = propbac),
             fill = "#3399ff", color = "black",
             size= 5, shape = 21) +
  geom_smooth(data = census_wide_final_2015_2019, aes(x = medhhinc, y = propbac, col="#3399ff"),
              method='lm', formula= y~x, se=FALSE, color='#3399ff')  +
  geom_point(data = census_wide_final_2015_2019_sorted, aes(x = simulated_medhhinc, y = propbac),
             fill = "#ff6600", color = "black",
             size= 5, shape = 21) +
  geom_smooth(data = census_wide_final_2015_2019_sorted, aes(x = simulated_medhhinc, y = propbac, col="#ff6600"),
              method='lm', formula= y~x, se=FALSE, color='#ff6600')  +
  labs(x='Median Household Income ($)', 
       y='Baccalaureate Attainment Rate (%)', 
       title=sprintf("Tract-level Baccalaureate Attainment Rate vs Median Income\n
       Simulated Adjusted R-squared: %s, Actual Adjusted R-squared: %s", 
                     round(simulated.summary.lm.model$adj.r.squared, 4), 
                     round(actual.summary.lm.model$adj.r.squared, 4)),
       caption = "Data: 2015-2019 5-year ACS, US Census Bureau, Cook County, IL") +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'), 
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white", color = NA))
  theme_minimal()
  

#After applying ‘Robin Hood’ tax policy, even though the sum of squares (SSE_simulated_data = 99642.83) of the regression 
#model from simulated median household income versus baccalaureate attainment rate is lesser than the sum of squares 
#(SSE_actual_data = 101495.9) of the regression model from actual median household income versus baccalaureate attainment 
#rate, overall lesser percentage of baccalaureate attainment rate are still crowded at lower median household income 
#irrespective of simulated versus actual data which tells clearly ‘Robin Hood’ tax policy has no impact on increasing 
#the baccalaureate attainment rate especially for smaller portion of lower household income brackets

#To increase the percentage of baccalaureate attainment rate for lower median household income bracket, we need to 
#increase drastically the income of lower income bracket households

#Hence it's not realistic
