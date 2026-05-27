# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(car)
library(lmtest)
library(interactions)
library(nortest)
# Read the data
data <- read.csv("D:/ECOMETRICS PROJECT/House_Rent_Dataset.csv")
head(data)
str(data)
# Convert specified columns to factors
factor_cols <- c("Area.Type", "City", "Furnishing.Status",
                 "Tenant.Preferred", "Point.of.Contact")
data[factor_cols] <- lapply(data[factor_cols], as.factor)
df <- data.frame(data)
# Split Floor column and select - FIXED VERSION
df <- df %>%
  mutate(
    no_of_floors = as.numeric(str_extract(Floor, "(?<=out of )\\d+")),
    floor = case_when(
      grepl("Ground", Floor) ~ 0,
      grepl("Upper Basement", Floor, ignore.case = TRUE) ~ -1,
      grepl("Lower Basement", Floor, ignore.case = TRUE) ~ -2,
      TRUE ~ as.numeric(str_extract(Floor, "^\\d+"))
    )
  ) %>%
  dplyr::select(BHK, Rent, Size, no_of_floors, floor, everything(), -Floor)
#remove sparse categories
head(df)
summary(df)
df <- df %>%
  filter(
    Area.Type != "Built Area",
    Point.of.Contact != "Contact Builder",
    !is.na(no_of_floors), # Remove NA in floor counts
    Bathroom < 6 # Remove rows with 6 or more bathrooms
  ) %>%
  droplevels()
#remove outliers:
model <- lm(log(Rent) ~ Size + BHK + no_of_floors + floor + Area.Type + City + Furnishing.Status
              + Tenant.Preferred + Bathroom + Point.of.Contact,data = df)
n <- nrow(df)
df_clean <- df %>%
  mutate(cooks_d = cooks.distance(model)) %>%
  filter(cooks_d <= 4/n) %>%
  dplyr::select(-cooks_d)
summary(df_clean)
#------------------------------------------------------------------------------------
# Scatterplots for quantitative variables
plot(df_clean$Size, df_clean$Rent,
     main="Rent vs Size (Cleaned Data)",
     xlab="Size", ylab="Rent",
     pch=20, col="gray50")
plot(df_clean$BHK, df_clean$Rent,
     main="Rent vs BHK (Cleaned Data)",
     xlab="BHK", ylab="Rent)",
     pch=20, col="gray50")
plot(df_clean$Bathroom, df_clean$Rent,
     main="Rent vs Bathroom (Cleaned Data)",
     xlab="Bathroom", ylab="Rent",
     pch=20, col="gray50")
plot(df_clean$no_of_floors, df_clean$Rent,
     main="Rent vs no_of_floors (Cleaned Data)",
     xlab="no_of_floors", ylab="Rent",
     pch=20, col="gray50")
plot(df_clean$floor, df_clean$Rent,
     main="Rent vs floor (Cleaned Data)",
     xlab="floor", ylab="Rent",
     pch=20, col="gray50")
# Boxplots for categorical variables
boxplot(log(Rent) ~ City, data=df_clean,
        main="Rent by City (Cleaned Data)",
        xlab="City", ylab="log(Rent)",
        col="lightblue")
boxplot(log(Rent) ~ Area.Type, data=df_clean,
        main="Rent by Area Type (Cleaned Data)",
        xlab="Area Type", ylab="log(Rent)",
        col="lightblue")
boxplot(log(Rent) ~ Furnishing.Status, data=df_clean,
        main="Rent by Furnishing (Cleaned Data)",
        xlab="Furnishing Status", ylab="log(Rent)",
          col="lightblue")
boxplot(log(Rent) ~ Tenant.Preferred, data=df_clean,
        main="Rent by Tenant Preference (Cleaned Data)",
        xlab="Tenant Preferred", ylab="log(Rent)",
        col="lightblue")
boxplot(log(Rent) ~ Point.of.Contact, data=df_clean,
        main="Rent by Contact Type (Cleaned Data)",
        xlab="Point of Contact", ylab="Rent",
        col="lightblue")
#------------------------------------------------------------------------------------
# Initial regression model
model <- lm(Rent ~ Size + BHK + Bathroom + City + Area.Type +
              
              Furnishing.Status + floor+ no_of_floors + Tenant.Preferred +
              Point.of.Contact , data = df_clean)

# Display model summary
summary(model)
#test mullticollinerity
vif(model)
# Residuals vs Fitted
plot(rstandard(model) ~ fitted(model), main = "Residuals vs Fitted (Initial Model)",
     xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(h = 0, lty = 2, col = "red")
# Q-Q plot
qqnorm(rstandard(model), main = "Normal Q-Q Plot (Initial Model)")
qqline(rstandard(model), col = "red")
#auto colleration of residuals test
durbinWatsonTest(model)
#Model misspecification test
resettest(model, power = 2:3, type = "fitted")
#normality tests
shapiro.test(residuals(model))
ad.test(residuals(model))
#histogram residuals
hist(rstandard(model))
#-------------------------------------------------------------------------------------
# Log regression model
log_model <- lm(log(Rent) ~ Size + BHK + Bathroom + City + Area.Type +
                  Furnishing.Status + floor+ no_of_floors + Tenant.Preferred +
                  Point.of.Contact , data = df_clean)

# Display model summary
summary(log_model)
#test mullticollinerity
vif(log_model)
# Residuals vs Fitted
plot(rstandard(log_model) ~ fitted(log_model), main = "Residuals vs Fitted (log Model)",
     xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(h = 0, lty = 2, col = "red")
# Q-Q plot

-46-
  
  qqnorm(rstandard(log_model), main = "Normal Q-Q Plot (log Model)")
qqline(rstandard(log_model), col = "red")
#auto colleration of residuals test
durbinWatsonTest(log_model)
#Model misspecification test
resettest(log_model, power = 2:3, type = "fitted")
#normality tests
shapiro.test(residuals(log_model))
ad.test(residuals(log_model))
#histogram residuals
hist(rstandard(log_model))
#--------------------------------------------------------------------------------------------
#log_log_model
log_log_model <- lm(log(Rent) ~ log(Size) + BHK + Bathroom + City + Area.Type +
                      Furnishing.Status +floor + no_of_floors + Tenant.Preferred +
                      Point.of.Contact , data = df_clean)

vif(log_log_model)
summary(log_log_model)
# Residuals vs Fitted
plot(rstandard(log_log_model) ~ fitted(log_log_model), main = "Residuals vs Fitted (log-log
Model)",
     xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(h = 0, lty = 2, col = "red")
# Q-Q plot
qqnorm(rstandard(log_log_model), main = "Normal Q-Q Plot (log-log Model)")
qqline(rstandard(log_log_model), col = "red")
#auto colleration of residuals test
durbinWatsonTest(log_log_model)
#Model misspecification test
resettest(log_log_model, power = 2:3, type = "fitted")
#normality tests
shapiro.test(residuals(log_log_model))
ad.test(residuals(log_log_model))
#histogram residuals
hist(rstandard(log_log_model))
#compare between two models
AIC(log_log_model, log_model)
BIC(log_log_model, log_model)
#--------------------------------------------------------------------------------------
reduced_log_model <- lm(log(Rent) ~ Size + BHK + Bathroom + City + Area.Type +
                          Furnishing.Status + no_of_floors + Tenant.Preferred +
                          Point.of.Contact , data = df_clean)

vif(reduced_log_model)
summary(reduced_log_model)
# Residuals vs Fitted
plot(rstandard(reduced_log_model) ~ fitted(reduced_log_model), main = "Residuals vs Fitted
(reduced-log Model)",
     xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(h = 0, lty = 2, col = "red")
# Q-Q plot
qqnorm(rstandard(reduced_log_model), main = "Normal Q-Q Plot (reduced-log Model)")
qqline(rstandard(reduced_log_model), col = "red")
#auto colleration of residuals test
durbinWatsonTest(reduced_log_model)
#Model misspecification test
resettest(reduced_log_model, power = 2:3, type = "fitted")
#normality tests
shapiro.test(residuals(reduced_log_model))
ad.test(residuals(reduced_log_model))
#histogram residuals
hist(rstandard(reduced_log_model))
#compare between two models
anova(reduced_log_model,log_model)
#compare between two models
AIC(reduced_log_model, log_model)
BIC(reduced_log_model, log_model)
#--------------------------------------------------------------------------------------
#interaction_model
model_interaction <- lm(
  log(Rent) ~ Size + BHK + Bathroom + City + Area.Type +
    Furnishing.Status + no_of_floors + Tenant.Preferred + Point.of.Contact +
    Area.Type:City + Size:City,
  data = df_clean
)
summary(model_interaction)
# Residuals vs Fitted
plot(rstandard(model_interaction) ~ fitted(model_interaction), main = "Residuals vs Fitted
(interaction Model)",
     xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(h = 0, lty = 2, col = "red")
# Q-Q plot
qqnorm(rstandard(model_interaction), main = "Normal Q-Q Plot (interaction Model)")
qqline(rstandard(model_interaction), col = "red")
#auto colleration of residuals test
durbinWatsonTest(model_interaction)
#Model misspecification test
resettest(model_interaction, power = 2:3, type = "fitted")
#normality tests
shapiro.test(residuals(model_interaction))
ad.test(residuals(model_interaction))
#histogram residuals
hist(rstandard(model_interaction))
#check if the interaction model better than the main effect model
anova(reduced_log_model,model_interaction)
interact_plot(model_interaction, pred= Size, modx= City)