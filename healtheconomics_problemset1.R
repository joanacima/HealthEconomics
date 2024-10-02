# HEALTH ECONOMICS
# PROBLEM SET 1 - OCTOBER, 3, 2024

# --- Section 1: Loading Libraries ---
# Load necessary R libraries required for the analysis

library(here)   # Simplifies the way to specify file paths that work well across different operating systems.
library(readxl) # Used for reading Excel files.
library(dplyr)  # A powerful data manipulation library in R, part of the 'tidyverse'.
                # Provides functions to efficiently perform common data operations such as:
                   # - filter(): for subsetting rows based on logical conditions.
                   # - select(): for selecting specific columns.
                   # - arrange(): for sorting data.
                   # - mutate(): for creating new columns from existing ones.
                   # - summarise(): for generating statistical summaries.
                   # - group_by(): for grouping data to perform group-wise analyses.
library(car)    # Includes tools for regression diagnostics, hypothesis testing of regression coefficients, and other advanced analyses.
library(stargazer) # # The 'stargazer' package is used for creating well-formatted tables of statistical model outputs.
library(ggplot2)  # for creating plots

# --- Section 2: Setting and Displaying Current Working Directory ---
# Set and print the current working directory to ensure file paths are correctly set.

current_directory <- getwd()
print(current_directory)

# --- Section 3: Data Loading ---
# Load the data from the specified Excel file located within a 'data' subfolder.

data_final <- as.data.frame(read_excel("data/data_final.xlsx"))

# View the data frame in an interactive window (useful in RStudio).

View(data_final)

# --- Section 4: Descriptive statistics ---
# Generate basic descriptive statistics for key variables in the dataset.
data_final %>%
  dplyr::select(gdp_pc, health_expenditure_pc) %>% 
  stargazer(title="",
          type= "text", digits = 2)


# --- Section 5: Scatter Plot of GDP versus Health Expenditure ------

# Plotting the data
ggplot(data_final, aes(x = gdp_pc, y = health_expenditure_pc)) +
  geom_point(aes(color = gdp_pc), alpha = 0.6) +  # Add points with transparency
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Add a linear regression line with standard error
  labs(x = "GDP per Capita ", 
       y = "Health Care Expenditure per Capita",
       title = "Relationship between GDP and Health Expenditure") +
  theme_minimal() 


# --- Section 6: Data Transformation ---
# Calculate the natural logarithm of GDP per capita and Health Expenditure per capita to use in regression models.

data_final <- data_final %>%
  mutate(
    log_gdp_pc = log(gdp_pc),               # Calculate log of GDP per capita.
    log_health_expenditure_pc = log(health_expenditure_pc)  # Calculate log of health expenditure per capita.
  )


# --- Section 7: Regression Analysis ---
# Perform a linear regression where the log of health expenditure per capita is the dependent variable
# and the log of GDP per capita is the independent variable.

model <- lm(log_health_expenditure_pc ~ log_gdp_pc, data = data_final)

# Display the summary of the regression model to see coefficients, residuals, and other important statistics.
summary(model)

# --- Section 8: Hypothesis Testing ---
# Test if the coefficient of log_gdp_pc is exactly equal to 1, testing the null hypothesis against the alternative.

test_result <- linearHypothesis(model, "log_gdp_pc = 1")
print(test_result)  # The output includes the test statistic, degrees of freedom, and the p-value.

# --- Section 9: Advanced Regression Analysis ---
# Calculate the median of GDP per capita and create a binary variable to distinguish between countries above and below the median.

data_final <- data_final %>%
  mutate(above_median_gdp = if_else(gdp_pc > median(gdp_pc, na.rm = TRUE), 1, 0))

# Model for countries with GDP per capita above the median.
model_above_median <- lm(log_health_expenditure_pc ~ log_gdp_pc, data = filter(data_final, above_median_gdp == 1))
summary(model_above_median)

# Model for countries with GDP per capita below the median.
model_below_median <- lm(log_health_expenditure_pc ~ log_gdp_pc, data = filter(data_final, above_median_gdp == 0))
summary(model_below_median)

