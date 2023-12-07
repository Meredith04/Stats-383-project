## Cameron Watson ## STAT 383 ## Project ## December 8, 2023 ##
# R maintenance, clean consol, environment, set directory, and import packages
cat("\f")
rm(list = ls())
location <- getwd()
setwd(location)
library(readxl)

# The data file is read into R, and then converted into manipulable vectors for each variable
data = read_excel('converted_Data.xlsx')

# The sample outcome is changed to one's and zero's
normality <- vector(length = 100)
for (i in 1:100) {
  if (data[i, 10] == 'N') normality[i] <- 1
  else normality[i] <- 0 }

# The other nine variables
season <- age <- disease <- trauma <- surgery <- fevers <- alcohol <- smoking <- sitting <- vector(length = 100)
for (i in 1:100) {
  season[i] <- as.numeric(data[i,1])
  age[i] <- as.numeric(data[i,2])
  disease[i] <- as.numeric(data[i,3])
  trauma[i] <- as.numeric(data[i,4])
  surgery[i] <- as.numeric(data[i,5])
  fevers[i] <- as.numeric(data[i,6])
  alcohol[i] <- as.numeric(data[i,7])
  smoking[i] <- as.numeric(data[i,8])
  sitting[i] <- as.numeric(data[i,9])
}

# The correlation coefficients are calculated using "cor()"
# The coeefficients calculated are for each of the 9 variables with respect to "normality"
cor_season = cor(season, normality) # -0.1924
cor_age = cor(age, normality) # -0.1152
cor_disease = cor(disease, normality) # 0.0403
cor_trauma = cor(trauma, normality) # 0.1413
cor_surgery = cor(surgery, normality) # 0.0542
cor_fevers = cor(fevers, normality) # 0.1214
cor_alcohol = cor(alcohol, normality) # 0.1448
cor_smoking = cor(smoking, normality) # -0.0489
cor_sitting = cor(sitting, normality) # -0.0230

# These are then printed to the consol for user viewing
cat("// Correlation Values //", "\nSeason: ", cor_season, "\nAge: ", cor_age,
    "\nDisease: ", cor_disease, "\nTrauma: ", cor_trauma, "\nSurgery: ",
    cor_surgery, "\nFevers: ", cor_fevers, "\nAlcohol: ", cor_alcohol,
    "\nSmoking: ", cor_smoking, "\nSitting: ", cor_sitting, "\n")

# All hypothesis testing is done with "t.test()"
# Hypothesis testing is done to evaluate if the average of testing results within
# a season are the same out of that season

# A set of vectors for each season are made which are then filled with the 
# correlating normality results
winter <- not_winter <- vector()
spring <- not_spring <- vector()
summer <- not_summer <- vector()
fall <- not_fall <- vector()
for (i in 1:100) {
  if (season[i] == -1) winter <- c(winter, normality[i])
  else not_winter <- c(not_winter, normality[i])
  if (season[i] == -0.33) spring <- c(spring, normality[i])
  else not_spring <- c(not_spring, normality[i])
  if (season[i] == 0.33) summer <- c(summer, normality[i])
  else not_summer <- c(not_summer, normality[i])
  if (season[i] == 1) fall <- c(fall, normality[i])
  else not_fall <- c(not_fall, normality[i])
}
test_winter <- t.test(winter, not_winter, conf.level = 0.95)
test_spring <- t.test(spring, not_spring, conf.level = 0.95)
test_summer <- t.test(summer, not_summer, conf.level = 0.95)
test_fall <- t.test(fall, not_fall, conf.level = 0.95)

# The p-values are shown to the user
cat("// Hypothesis Testing for Seasons - p-values //\nConfidence Level is 0.95
    \nWinter: ", test_winter$p.value, "\nSpring: ", test_spring$p.value, 
    "\nSummer: ", test_summer$p.value, "\nFall : ", test_fall$p.value, "\n")

# The next hypothesis testing is to see if the average normality result is different
# for light drinkers (up to one drink a week) and consistent drinkers (anything over)

# Two vectors are made to store the relevant normality results
consistent_drinkers <- light_drinkers <- vector()
for (i in 1:100) {
  temp = alcohol[i] #temporary value
  if (temp == 0.8 | temp == 1) {
    light_drinkers <- c(light_drinkers, normality[i])
  }
  if (temp == 0.2 | temp == 0.4 | temp == 0.6) {
    consistent_drinkers <- c(consistent_drinkers, normality[i])
  }
}
test_drinkers <- t.test(consistent_drinkers, light_drinkers)

# The results are shown to the user
cat("// Hypothesis Testing for Alcohol Consumption //\nConfidence Level is 0.95
p-value: ", test_drinkers$p.value, "\n")

# The final hypothesis test is done for trauma following similar procedures 
trauma_y <- trauma_n <- vector()
for (i in 1:100) {
  if (trauma[i] == 1) trauma_y <- c(trauma_y, normality[i])
  if (trauma[i] == 0) trauma_n <- c(trauma_n, normality[i])
}
test_trauma <- t.test(trauma_y, trauma_n)
cat("// Hypothesis Testing for Trauma //\nConfidence Level is 0.95
p-value: ", test_trauma$p.value, "\n")

# Linear regression is conducted on data relating to the hypothesis testing
# Average hypothesis testing for each season is plotted along the value that month holds
mu_winter <- sum(winter == 1) / length(winter)
mu_spring <- sum(spring == 1) / length(spring)
mu_summer <- sum(summer == 1) / length(summer)
mu_fall <- sum(fall == 1) / length(fall)

# put into vectors that can be used with "plot()"
season_mu <- c(mu_winter, mu_spring, mu_summer, mu_fall)
season_value <- c(-1, -0.33, 0.33, 1)
plot(season_value, season_mu,
     main = "Normality Percentage vs. Sesaon", 
     xlab = "Season Values (-1, -0.33, 033, 1)", 
     ylab = "Normal Samples (%)")

# Linear regression for this data is found, added to the plot, and then the 
# results are show to the user
season_lm <- lm(season_mu ~ season_value)
abline(season_lm, col = "blue")
cat("// Regression for Seasons //")
print(summary(season_lm))

# The average result for different categories for drinkers undergoes a similar process
alcohol_0.2 <- alcohol_0.4 <- alcohol_0.6 <- alcohol_0.8 <- alcohol_1.0 <- vector()
for (i in 1:100) {
  temp <- alcohol[i]
  norm <- normality[i]
  if (temp == 0.2) alcohol_0.2 <- c(alcohol_0.2, norm)
  if (temp == 0.4) alcohol_0.4 <- c(alcohol_0.4, norm)
  if (temp == 0.6) alcohol_0.6 <- c(alcohol_0.6, norm)
  if (temp == 0.8) alcohol_0.8 <- c(alcohol_0.8, norm)
  if (temp == 1.0) alcohol_1.0 <- c(alcohol_1.0, norm)
}
alcohol_0.2_mu <- sum(alcohol_0.2 == 1) / length(alcohol_0.2)
alcohol_0.4_mu <- sum(alcohol_0.4 == 1) / length(alcohol_0.4)
alcohol_0.6_mu <- sum(alcohol_0.6 == 1) / length(alcohol_0.6)
alcohol_0.8_mu <- sum(alcohol_0.8 == 1) / length(alcohol_0.8)
alcohol_1.0_mu <- sum(alcohol_1.0 == 1) / length(alcohol_1.0)
drinkers_mu <- c(alcohol_0.2_mu, alcohol_0.4_mu, alcohol_0.6_mu, alcohol_0.8_mu, alcohol_1.0_mu)
drinkers_value <- c(0.2, 0.4, 0.6, 0.8, 1.0)
plot(drinkers_value, drinkers_mu, 
     main = "Normality Percentage vs. Alcohol Consupmtion", 
     xlab = "Alcohol Cunsumption Values (0.2, 0.4, 0.6, 0.8, 1.0)", 
     ylab = "Normal Samples (%)")
drinkers_lm <- lm(drinkers_mu ~ drinkers_value)
abline(drinkers_lm, col = "blue")
cat("// Regression for Alcohol Concumation //")
print(summary(drinkers_lm))

# The average normality result for having trauma undergoes a similr proces
trauma_y_mu <- sum(trauma == 1) / length(trauma)
trauma_n_mu <- sum(trauma == 0) / length(trauma)
trauma_mu <- c(trauma_y_mu, trauma_n_mu)
trauma_values <- c(1, 0)
plot(trauma_values, trauma_mu, 
     main = "Normality Percentage vs. Trauma Result", 
     xlab = "Trauma Values (0, 1)", 
     ylab = "Normal Samples (%)")
trauma_lm <- lm(trauma_mu ~ trauma_values)
abline(trauma_lm, col = "blue")
cat("// Regression for Trauma //")
print(summary(trauma_lm))

## END ##

