library(readxl)
library(readr)

Month_Value_1 <- read_excel("D:/PG DATA SCIENCE/4th Trimester/Time Series Analysis by Amala Maam/Month_Value_1.xlsx")
# Exploratory Month_Value_1 Analysis
#Criterion 1: Dataset Selection and Preparation

# View the first few rows
head(Month_Value_1)

# View the structure of the Month_Value_1
str(Month_Value_1)

# Summary of each column
summary(Month_Value_1)

# View the dimensions of the Month_Value_1 (rows, columns)
dim(Month_Value_1)

# Count total missing values
sum(is.na(Month_Value_1))

# Visualize missing Month_Value_1
library(Amelia)
missmap(Month_Value_1, main = "Missing values map")

#Convert the Period column to a Date format for easier manipulation.
Month_Value_1$Period <- as.Date(Month_Value_1$Period, format="%d.%m.%Y")

# Fill missing values with the mean
Month_Value_1$Sales_quantity[is.na(Month_Value_1$Sales_quantity)] <- mean(Month_Value_1$Sales_quantity, na.rm = TRUE)

# Fill missing values with the median
Month_Value_1$Average_cost[is.na(Month_Value_1$Average_cost)] <- median(Month_Value_1$Average_cost, na.rm = TRUE)

summary(Month_Value_1$Revenue)
mean(Month_Value_1$Sales_quantity)
sd(Month_Value_1$Average_cost)

#a. Time Series Plot for Revenue
  #Plot the revenue over time to observe trends.

library(ggplot2)
ggplot(Month_Value_1, aes(x = Period, y = Revenue)) +
  geom_line(color = "blue") +
  labs(title = "Revenue Over Time", x = "Period", y = "Revenue")

#b. Bar Plot for Sales Quantity
  #Plot sales quantity to see the monthly distribution.

ggplot(Month_Value_1, aes(x = Period, y = Sales_quantity)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Sales Quantity Over Time", x = "Period", y = "Sales Quantity")

#c. Boxplot for Average Cost
  #Create a boxplot to check for any outliers in the Average_cost.

ggplot(Month_Value_1, aes(y = Average_cost)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot of Average Cost", y = "Average Cost")

#d. Scatter Plot: Revenue vs. Sales Quantity
  #Plot the relationship between Revenue and Sales_quantity.

ggplot(Month_Value_1, aes(x = Sales_quantity, y = Revenue)) +
  geom_point(color = "red") +
  labs(title = "Revenue vs. Sales Quantity", x = "Sales Quantity", y = "Revenue")

cor_matrix <- cor(Month_Value_1[, c("Revenue", "Sales_quantity", "Average_cost", "The_average_annual_payroll_of_the_region")])
print(cor_matrix)

# Visualize the correlation matrix
library(corrplot)
corrplot(cor_matrix, method = "circle")

# Criterion 2: Identification of Time Series Components

rm(list=ls(all=TRUE))

library(tseries)
library(forecast)

# Assuming 'Month_Value_1' is your dataset and 'Revenue' is the column of interest
# Create a time series object
ts_Month_Value_1 <- ts(Month_Value_1$Revenue, start = c(2015, 1), frequency = 12)

# Decompose the time series (for analysis purposes)
z <- decompose(ts_Month_Value_1)

# Plot the decomposed components
plot(z)


# - Trend: The long-term movement in the time series.

# - Revenue: Displays an upward trend from January 2015 to January 2017, indicating overall business growth.There are significant increases in December each year.

# - Sales Quantity: Shows a general increasing trend, with the highest values occurring at the end of each year, aligning with Revenue spikes.

# - Average Cost: Fluctuates over time but remains within a certain range, with no clear upward or downward trend.

# -  Annual Payroll: Remains constant for 2015 and 2016 but decreases slightly in 2017, possibly reflecting changes in regional economic conditions.

#Seasonality: The repeating short-term cycle (e.g., monthly, quarterly).

# - Revenue & Sales Quantity: Both exhibit strong seasonal peaks in December, likely due to holiday sales or year-end promotions. A secondary pattern of increased activity is observed around mid-year.
# - Average Cost: Shows periodic increases, particularly in January and April, suggesting seasonal variations in production or supply costs.


#Cyclic: Longer-term cycles that are not of a fixed frequency.

# - Revenue & Sales Quantity: May display cyclic patterns driven by broader economic cycles or market conditions, but further analysis is needed to confirm these patterns.

#Irregular: Random variations or noise in the time series.

# - Revenue & Sales Quantity: Any unexpected fluctuations that do not align with the trend or seasonality could be due to one-time events like promotions or market disruptions.
# - Average Cost: Sudden changes might reflect irregular factors such as supplier issues or unforeseen production challenges.


# Criterion 3: Model Selection and Justification


## When selecting between an additive and a multiplicative model for time series analysis, the choice is based on the nature of the data and the relationship between the components of the time series (trend, seasonality, and residuals).

## 1. Additive Model:
# - In an additive model, the components of the time series (trend, seasonality, and residuals) are added together:

## Use Case: The additive model is appropriate when the variations in the data (seasonal fluctuations and residuals) remain constant over time, regardless of the level of the trend.

##Justification:

# - Constant Seasonality: If the seasonal variations (peaks and troughs) are roughly the same magnitude across the entire data set, an additive model is appropriate.
# - No Proportional Effect: If the increase in the trend does not amplify the seasonal pattern or residual fluctuations, the additive model should be used.
# - Example: If your time series data shows a constant increase in trend (e.g., adding $1000 every month) while seasonal effects (e.g., a rise of $200 in December) remain consistent, an additive model is a good fit.

#2. Multiplicative Model:

## In a multiplicative model, the components of the time series are multiplied together:

## Use Case: The multiplicative model is suitable when the variations in the data are proportional to the level of the trend.

## Justification:

# - Proportional Seasonality: If the seasonal variations increase or decrease in proportion to the trend, a multiplicative model is more appropriate.
# - Variable Amplitude: If the data exhibits varying amplitude with changes in trend, where higher levels of the trend correspond to larger seasonal fluctuations, a multiplicative model should be selected.
# - Example: If your time series data shows that revenue doubles and the seasonal spikes also double when the trend doubles, the multiplicative model fits well. 

# Strong Rationale for Selection:

##Model Fit: After examining the data visually and statistically, if you observe that the seasonal effect grows with the trend, a multiplicative model will provide a better fit. Conversely, if the seasonal effect is consistent regardless of the trend, the additive model is preferable.

##Data Transformation: Sometimes, taking the logarithm of the data can convert a multiplicative relationship into an additive one, which can simplify the modeling process. For instance, using log-transformed data in an additive model can effectively model a multiplicative relationship.

##Domain Knowledge: If you have knowledge or historical context indicating that growth patterns or fluctuations scale with the level of the trend (e.g., market expansions, inflation), this would justify the use of a multiplicative model.


##The choice between an additive and multiplicative model should be driven by how the trend, seasonal effects, and residuals interact in your data. A clear understanding of these relationships, supported by visual inspections (e.g., time series plots, decomposition) and statistical tests, will provide a strong rationale for your model selection.

#Conclusion:

#Given these observations, an additive model was selected for this analysis. 
#The seasonal component in the data remains relatively constant, and the trend is linear, making the additive model a better fit.
#The additive model accurately captures the linear trend and the consistent seasonal variations without assuming proportional growth, which aligns well with the nature of the dataset.


# Criterion 4: Component Elimination and Methodology

# - The goal is to decompose the time series data to understand its underlying components—trend, seasonality, and residuals—and to remove these components to analyze and forecast the data more effectively. Specifically, we aim to:

  # - Identify and remove the trend component.
  # - Eliminate the seasonal component.
  # - Apply Holt-Winters exponential smoothing for forecasting.


## 1. Removing the Trend Component

  ##Methodology:

    # - Extract the trend component from the decomposed series.
    # - Subtract the trend component from the original time series to isolate the remaining components (seasonal and residual).

# Extract the trend component
trend <- z$trend

# Remove the trend component from the original time series
detrended <- ts_Month_Value_1 - trend 


##Justification: 
 # - Removing the trend component allows us to analyze the time series without the long-term movement effect, which helps in focusing on short-term patterns and seasonal effects.

# Plot the detrended series
plot(detrended, main = "Detrended Time Series", ylab = "Revenue")

##Conclusion
##This methodology involves:
  
# - Decomposing the time series to extract trend, seasonal, and residual components.
# - Removing trend component to better understand residual patterns.

#Criterion 5:
#Forecasting using Exponential Smoothing is accurate with clear interpretation and insights.


# Apply Holt-Winters exponential smoothing to the original time series
zz <- HoltWinters(ts_Month_Value_1)

# Display the Holt-Winters model
print(zz)

# Plot the fitted model
plot(zz)



plot(zz,main="Holt-Winters Smooth")
zz.pred<-predict(zz,30,prediction.interval = TRUE)
zz.pred

plot(zz,zz.pred, main = "Holt-Winter Forecast")

#Conclusion:

## The Holt-Winters model indicates that the revenue for the business is expected to increase over time, with significant seasonal variations.
## The months of January tend to have lower revenues, while December typically sees a peak, likely due to seasonal factors like holidays. 
## The predictions can help in planning and resource allocation, especially around periods of expected higher or lower revenue.
