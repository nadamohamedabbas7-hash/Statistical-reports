# Load necessary packages
library(car)
library(GGally)
# Import data
data <- read.csv("D:/Regression/regression project/Real estate.csv")
# Initial data exploration: Correlation matrix
ggpairs(data)
# Step 1: Fit the model without log transformation
model <- lm(house_price_of_unit_area ~ house_age + distance_to_the_nearest_MRT_station +
              
              number_of_convenience_stores + latitude + longitude, data = data)

# Regression summary and diagnostics for the initial model
summary(model)
anova(model)
vif(model)
# Residuals vs Fitted
plot(rstandard(model) ~ fitted(model), main = "Residuals vs Fitted (Initial Model)",
     xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(h = 0, lty = 2, col = "red")
# Q-Q plot
qqnorm(rstandard(model), main = "Normal Q-Q Plot (Initial Model)", ylim = c(-4, 10))
qqline(rstandard(model), col = "red")
# Cook's distance
plot(model, which = 5, main = "Cook's Distance (Initial Model)")
# Step 2: Remove 'longitude' as it is insignificant
model_updated <- lm(house_price_of_unit_area ~ house_age +
                      distance_to_the_nearest_MRT_station +
                      
                      number_of_convenience_stores + latitude, data = data)

# Regression summary after removing 'longitude'
summary(model_updated)
anova(model_updated)
vif(model_updated)
# Residuals vs Fitted
plot(rstandard(model_updated) ~ fitted(model_updated), main = "Residuals vs Fitted (Without
Longitude)",
     xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(h = 0, lty = 2, col = "red")
# Q-Q plot
qqnorm(rstandard(model_updated), main = "Normal Q-Q Plot (Without Longitude)", ylim = c(-4,
                                                                                        10))
qqline(rstandard(model_updated), col = "red")
# Cook's distance
plot(model_updated, which = 5, main = "Cook's Distance (Without Longitude)")
# Step 3: Log-transform the dependent variable and fit the model
  data$log_house_price <- log(data$house_price_of_unit_area)
log_model <- lm(log_house_price ~ house_age + distance_to_the_nearest_MRT_station +
                  
                  number_of_convenience_stores + latitude, data = data)

# Summary and diagnostics for the log-transformed model
summary(log_model)
anova(log_model)
vif(log_model)
# Residuals vs Fitted
plot(rstandard(log_model) ~ fitted(log_model), main = "Residuals vs Fitted (Log Model)",
     xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(h = 0, lty = 2, col = "red")
# Q-Q plot
qqnorm(rstandard(log_model), main = "Normal Q-Q Plot (Log Model)", ylim = c(-4, 10))
qqline(rstandard(log_model), col = "red")
# Cook's distance
plot(log_model, which = 5, main = "Cook's Distance (Log Model)")
# Step 4: Identify and remove outliers based on Cook's Distance
cooks_d <- cooks.distance(log_model)
potential_outliers <- which(cooks_d > (4 / nrow(data))) # Rule of thumb threshold
# Removing outliers
clean_data <- data[-potential_outliers, ]
# Step 5: Refit the log-transformed model with cleaned data
clean_log_model <- lm(log_house_price ~ house_age + distance_to_the_nearest_MRT_station +
                        number_of_convenience_stores + latitude, data = clean_data)

# Summary of the cleaned log-transformed model
summary(clean_log_model)
anova(clean_log_model)
vif(clean_log_model)
# Diagnostics for the cleaned log-transformed model
# Residuals vs Fitted
plot(rstandard(clean_log_model) ~ fitted(clean_log_model),
     main = "Residuals vs Fitted (Cleaned Log Model)",
     xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(h = 0, lty = 2, col = "red")
# Q-Q plot for cleaned log-transformed model
qqnorm(rstandard(clean_log_model), main = "Normal Q-Q Plot (Cleaned Log Model)", ylim = c(-4,
                                                                                          10))
qqline(rstandard(clean_log_model), col = "red")
# Cook's distance plot for cleaned model
plot(clean_log_model, which = 5, main = "Cook's Distance (Cleaned Log Model)")
# Correlation matrix for cleaned data
ggpairs(clean_data)