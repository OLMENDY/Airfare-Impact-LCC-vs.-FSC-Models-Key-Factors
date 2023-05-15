# AIMS
# Compare the effects of passenger counts (lpassen) and flight distance (ldist) on airfare (lfare) to determine whether the hypothetical airline should follow either the Low-cost carrier (LCC) or full-service carrier (FSC) business model.

# VARIABLES
VARIABLES
1. year: Year of observation
2. id: Unique identifier for each entity (airline)
3. lfare: Log-transformed airfare
4. lpassen: Log-transformed average number of passengers per day
5. y98, y99, y00: Dummy variables for years 1998, 1999, and 2000, respectively
6. ldist: Log-transformed flight distance


# Preparation
# 1. Import dataset
install.packages("wooldridge")
library(wooldridge)
data('airfare')

# 2. Import packages
install.packages(c("tidyverse", "gplots", "lmtest", "stargazer", "plm", "knitr", "broom", "kableExtra"))
library(tidyverse)
library(gplots)
library(lmtest)
library(stargazer)
library(plm)
library(knitr)
library(broom)
library(kableExtra)
library(ggplot2)


# OUTLIER ANALYSIS

# Create a dataframe
airfare_df <- pdata.frame(airfare, index = c("year", "id"))

# Convert pdata.frame to data.frame
airfare_df_regular <- as.data.frame(airfare_df)

# Boxplots for lfare, lpassen, and ldist
ggplot(airfare_df_regular, aes(x = factor(year), y = lfare)) + 
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Year", y = "Log-transformed Airfare", title = "Boxplot of Log-transformed Airfare by Year")

ggplot(airfare_df_regular, aes(x = factor(year), y = lpassen)) + 
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Year", y = "Log-transformed Passengers", title = "Boxplot of Log-transformed Passengers by Year")

ggplot(airfare_df_regular, aes(x = factor(year), y = ldist)) + 
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Year", y = "Log-transformed Distance", title = "Boxplot of Log-transformed Distance by Year")

## Calculate Cook's distance and identify influential observations
# Run the fixed effects model
library(plm)
model <- plm(lfare ~ lpassen + ldist, data = airfare_df, model = "within")

# Calculate Cook's distance
cooksd <- cooks.distance(model)

# Identify influential observations based on a threshold (e.g., 4 times the mean)
influential <- as.numeric(names(cooksd)[cooksd > (4 * mean(cooksd))])

# Create a subset of the data excluding the influential observations
airfare_df_no_outliers <- airfare_df[-influential, ]


##Run the fixed effects model with and without outliers and compare the results:
# Fixed effects model without outliers
model_no_outliers <- plm(lfare ~ lpassen + ldist, data = airfare_df_no_outliers, model = "within")

# Compare the coefficients of the two models
summary(model)$coefficients
summary(model_no_outliers)$coefficients




# PANEL REGRESSION ANALYSIS

# Pooled OLS Model

# 1. Generating data frame with indexed variables
airfare_df <- pdata.frame(airfare, index = c("year", "id"))
airfare_df <- pdata.frame(individual, year, lfare, lpassen, y98, y99, y00, ldist, index = c("year", "id"))

# 2. Display the indexed and unindexed datasets
str(airfare)
str(airfare_df)
head(airfare)
head(airfare_df)
tail(airfare_df)

# 3. OLS using lm
OLS <- lm(lfare ~ y98 + y99 + y00 + ldist + lpassen, data = airfare)
summary(OLS)

# 4. OLS using plm
pooled2 <- plm(lfare ~ y98 + y99 + y00 + ldist + lpassen, data = airfare_df, model = "pooling")
summary(pooled2)

# 5. Create a table
stargazer(pooled2, type = 'text')

# 6. Test for heteroscedasticity
res <- residuals(OLS)
yhat <- fitted(OLS)
plot(airfare$passen, res, xlab = "avg. passengers per day", ylab = "Residuals")
plot(yhat, res, xlab = "Fitted values", ylab = "Residuals")

# Hausman test
#  1. Estimate FE model
## The model assumes changes within a cross-section that might be caused by the inherent properties of that entity
## Model changed to "within" for FE model
fe <- plm(lfare~lpassen+y98+y99+y00+ldist+lpassen, data = airfare, model = "within", index = 'id')
summary(fe)

#  2. Estimate RE model
re <- pggls(lfare~lpassen+y98+y99+y00+ldist+lpassen, data = airfare, model = "random")
summary(re)

# 3. Run Hausman test
phtest(fe, re)

# Fixed effects model
# 1. Summarising within entity effects
fe <- plm(lfare ~ y98 + ldist + lpassen, data = airfare_df, model = "within")
summary(fe)
fixef(fe)

# 2. Test for FE vs OLS
pFtest(fe, OLS)

# 3. Presenting fe model in tabular form
stargazer(fe, type = 'text')

# Random effects model
# 1. Include both between-entity and within-entity effects
re <- plm(lfare ~ y98 + ldist + lpassen, data = airfare_df, model = "random")
summary(re)

# 2. Hausman test (repeated)
phtest(re, fe)

# 3. Beautify/tabulate data
kable(tidy(phtest(fe, re)), caption = "Hausman endogeneity test for the random effects fare model")
summary(re)

# Further diagnostic test
# FE is a better model than RE
# 1. Lagrange Multiplier Test
plmtest(fe, type = c("bp"))

# 2. Cross-sectional dependence
pcdtest(fe, test = c("lm"))



## SENSITIVITY ANALYSIS
# Change id variable to airline_id
airfare_df$airline_id <- airfare_df$id
airfare_df$id <- NULL

# Add an interaction term between distance and passenger count
fe_interaction <- plm(lfare ~ y98 + ldist + lpassen + ldist:lpassen, data = airfare_df, model = "within", index = c("airline_id", "year"))

# Using a quadratic term for distance
airfare_df$ldist_sq <- airfare_df$ldist^2

# Model 1: Fixed effects model without the interaction term
fe_model1 <- plm(lfare ~ y98 + ldist + lpassen, data = airfare_df, model = "within", index = c("airline_id", "year"))

# Update model with a quadratic term for distance
fe_quad <- update(fe_model1, . ~ . + ldist_sq)

# Calculate AIC values for the fixed effects models using a custom function
calc_AIC <- function(model) {
  k <- length(coef(model))
  n <- nobs(model)
  loglik <- -0.5 * n * (log(2 * pi) + 1 + log(deviance(model) / n))
  return(2 * k - 2 * loglik)
}

AIC_fe_model1 <- calc_AIC(fe_model1)
AIC_fe_interaction <- calc_AIC(fe_interaction)
AIC_fe_quad <- calc_AIC(fe_quad)

# Print AIC values
cat("AIC values:\n")
cat("Model 1 (no interaction):", AIC_fe_model1, "\n")
cat("Model 2 (interaction):", AIC_fe_interaction, "\n")
cat("Model 3 (quadratic):", AIC_fe_quad, "\n")

#YEAR-WISE ANALYSIS (USING FIXED EFFECT MODEL)
# Install and load the required packages
install.packages("plm")
library(plm)

# Filter the data by year
airfare_1998 <- airfare_df[airfare_df$y98 == 1,]
airfare_1999 <- airfare_df[airfare_df$y99 == 1,]
airfare_2000 <- airfare_df[airfare_df$y00 == 1,]

# Run fixed effects models for each year
model_1998 <- plm(lfare ~ lpassen + ldist, data = airfare_1998, model = "within")
model_1999 <- plm(lfare ~ lpassen + ldist, data = airfare_1999, model = "within")
model_2000 <- plm(lfare ~ lpassen + ldist, data = airfare_2000, model = "within")

# Print the model summaries
summary(model_1998)
summary(model_1999)
summary(model_2000)



## SUBGROUP ANALYSIS
# Create a threshold for short and long-haul flights
threshold <- 1500

# Divide the dataset into two groups based on the threshold
short_haul <- airfare_df[airfare_df$ldist <= log(threshold),]
long_haul <- airfare_df[airfare_df$ldist > log(threshold),]

# Fixed effects model for short-haul flights
short_haul_fe <- plm(lfare ~ lpassen * ldist + factor(year), data = short_haul, model = "within")

# Fixed effects model for long-haul flights
long_haul_fe <- plm(lfare ~ lpassen * ldist + factor(year), data = long_haul, model = "within")

# Summarize the results
summary(short_haul_fe)
summary(long_haul_fe)
