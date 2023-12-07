# The excel file with all information is stored into the variable "data"
library(readxl)
data = read_excel('converted_Data.xlsx')

# Convert all data for the normality of the samples into 1's for normal and
# 0's for abnormal
# This is done by indexing into data at the tenth column and comparing the 
# element at that location in an if-else statement
normality <- vector(length = 100)
for (i in 1:100) {
  if (data[i, 10] == 'N') normality[i] <- 1
  else normality[i] <- 0 }

# Initializing all variables as empty vectors of 100 elements
season <- age <- disease <- trauma <- surgery <- fevers <- alcohol <- smoking <- sitting <- vector(length = 100)

# A for loop to retrieve elements from data and convert them to numeric values
# This is done with respect to the variable type each column is classified as
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

# Check correlation between all variables and normality using "cor()"
cor_season = cor(season, normality) # -0.1924
cor_age = cor(age, normality) # -0.1152
cor_disease = cor(disease, normality) # 0.0403
cor_trauma = cor(trauma, normality) # 0.1413
cor_surgery = cor(surgery, normality) # 0.0542
cor_fevers = cor(fevers, normality) # 0.1214
cor_alcohol = cor(alcohol, normality) # 0.1448
cor_smoking = cor(smoking, normality) # -0.0489
cor_sitting = cor(sitting, normality) # -0.0230

# Printing the calculated correslation values to the consol
print("// Correlation Values //")
print(paste("season: ", cor_season))
print(paste("Age: ", cor_age))
print(paste("Disease: ", cor_disease))
print(paste("Trauma: ", cor_trauma))
print(paste("Surgery: ", cor_surgery))
print(paste("Fevers: ", cor_fevers))
print(paste("Alcohol: ", cor_alcohol))
print(paste("Smoking: ", cor_smoking))
print(paste("Sitting: ", cor_sitting))
print("")

# Hypothesis testing was then conducted on the values with a correlation test
# with the three highest values
# These variables are season, trauma, alcohol

# hypothesis testing in each season
# the average testing in a certain season vs not in that season
# Initializing the four hypotheses related to sesason
winter <- not_winter <- vector()
spring <- not_spring <- vector()
summer <- not_summer <- vector()
fall <- not_fall <- vector()

# Filling the vectors with results from the corresponding normality
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

# Run hypothesis testing for each season using "t.test()"
test_winter <- t.test(winter, not_winter, conf.level = 0.95)
test_spring <- t.test(spring, not_spring, conf.level = 0.95)
test_summer <- t.test(summer, not_summer, conf.level = 0.95)
test_fall <- t.test(fall, not_fall, conf.level = 0.95)

# Displaying information to the user
print("// Hypothesis Testing for Seasons - p-values //")
print("Confidence Level is 0.95")
print(paste("Winter: ", test_winter$p.value))
print(paste("Spring: ", test_spring$p.value))
print(paste("Summer: ", test_summer$p.value))
print(paste("Fall : ", test_fall$p.value))
print("")

# Is there a difference between light-drinkers and consistent-drinkers having normal counts
# hardly ever & once a week vs several times a week, every day and several times a day
# initializing empt vectors and filling them with needed information
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

# Displaying information to the user
print("// Hypothesis Testing for Alcohol Consumption //")
print("Confidence Level is 0.95")
print(paste("p-value: ", test_drinkers$p.value))
print("")

# Is there a difference between trauma with normal counts
# initializing vectors and filling them
trauma_y <- trauma_n <- vector()
for (i in 1:100) {
  if (trauma[i] == 1) trauma_y <- c(trauma_y, normality[i])
  if (trauma[i] == 0) trauma_n <- c(trauma_n, normality[i])
}
test_trauma <- t.test(trauma_y, trauma_n)

# Displaying information to the user
print("// Hypothesis Testing for Trauma //")
print("Confidence Level is 0.95")
print(paste("p-value: ", test_trauma$p.value))
print("")

# Linear Regression Testing on the three data sets that went through hypothesis testing
# looking at the trend of average normal results within each season
mu_winter <- sum(winter == 1) / length(winter)
mu_spring <- sum(spring == 1) / length(spring)
mu_summer <- sum(summer == 1) / length(summer)
mu_fall <- sum(fall == 1) / length(fall)

# putting the values into two vectors that can be plotted as x vs. y
season_mu <- c(mu_winter, mu_spring, mu_summer, mu_fall)
season_value <- c(-1, -0.33, 0.33, 1)

# plotting the average vs. season value, running regression, adding regression line, and printing summary
plot(season_value, season_mu,
     main = "Normality Percentage vs. Sesaon", 
     xlab = "Season Values (-1, -0.33, 033, 1)", 
     ylab = "Normal Samples (%)")
season_lm <- lm(season_mu ~ season_value)
abline(season_lm, col = "blue")
print("// Regression for Seasons //")
print(summary(season_lm))

# Running linear regression for the five categories of drinkers
# initializing vectors then filling in normality results for the different types of drinkers
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
# Calculating the average number of normal results in each category
alcohol_0.2_mu <- sum(alcohol_0.2 == 1) / length(alcohol_0.2)
alcohol_0.4_mu <- sum(alcohol_0.4 == 1) / length(alcohol_0.4)
alcohol_0.6_mu <- sum(alcohol_0.6 == 1) / length(alcohol_0.6)
alcohol_0.8_mu <- sum(alcohol_0.8 == 1) / length(alcohol_0.8)
alcohol_1.0_mu <- sum(alcohol_1.0 == 1) / length(alcohol_1.0)

# putting the values into two vectors that can be plotted as x vs. y
drinkers_mu <- c(alcohol_0.2_mu, alcohol_0.4_mu, alcohol_0.6_mu, alcohol_0.8_mu, alcohol_1.0_mu)
drinkers_value <- c(0.2, 0.4, 0.6, 0.8, 1.0)

# plotting the average vs. season value, running regression, adding regression line, and printing summary
plot(drinkers_value, drinkers_mu, 
     main = "Normality Percentage vs. Alcohol Consupmtion", 
     xlab = "Alcohol Cunsumption Values (0.2, 0.4, 0.6, 0.8, 1.0)", 
     ylab = "Normal Samples (%)")
drinkers_lm <- lm(drinkers_mu ~ drinkers_value)
abline(drinkers_lm, col = "blue")
print("// Regression for Alcohol Concumation //")
print(summary(drinkers_lm))

# Linear regression for trauma
# Calculating the average number of normal results in each category
trauma_y_mu <- sum(trauma == 1) / length(trauma)
trauma_n_mu <- sum(trauma == 0) / length(trauma)

# putting the values into two vectors that can be plotted as x vs. y
trauma_mu <- c(trauma_y_mu, trauma_n_mu)
trauma_values <- c(1, 0)

# plotting the average vs. season value, running regression, adding regression line, and printing summary
plot(trauma_values, trauma_mu, 
     main = "Normality Percentage vs. Trauma Result", 
     xlab = "Trauma Values (0, 1)", 
     ylab = "Normal Samples (%)")
trauma_lm <- lm(trauma_mu ~ trauma_values)
abline(trauma_lm, col = "blue")
print("// Regression for Trauma //")
print(summary(trauma_lm))