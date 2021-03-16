# Darwin CLark 2021.03.08
# ~/Programming/AppliedStats/8FirstGenCollege.r

library(ggplot2)
library(readxl)  # Excel reading lib

rawData = read_excel("Programming/AppliedStats/FirstYearGPA.xls")

orderedData = rawData[order(rawData$FirstGen), ] # What this line does is reorderes the rows based off of firstGen or not
                                                 # The first 195 lines are NOT first gen
                                                 # The remaining 24 lines ARE first gen

# Creating the two different generation data sets
notFirstGenRows = head(orderedData, 194)
firstGenRows = tail(orderedData, 25)

# Data collection
firstGenGPAMean = mean(firstGenRows$GPA)
firstGenGPASD = sd(firstGenRows$GPA)

notFirstGenGPAMean = mean(notFirstGenRows$GPA)
notFirstGenGPASD = sd(notFirstGenRows$GPA)

# === Work for part (b) === 
partBRegression = lm(rawData$GPA~rawData$FirstGen)
# For fun lines
summary(partBRegression)
plot(partBRegression)

plot(residuals(partBRegression), main="Residual Plot")
qqnorm(rawData$GPA)

# --- Calculating Sb ---
# Finding Sxx
avgFirstGen = mean(rawData$FirstGen)
Sxx = sum((rawData$FirstGen-avgFirstGen)^2)

# Finding Se
sumresids = sum(residuals(partBRegression)^2)
Se = sqrt(sumresids / (219-2))
Sb = Se / Sxx

# === Work for part (c) ===
# Logistic regression
partCRegression = glm(rawData$FirstGen~rawData$GPA, family=binomial("logit"))
# Predicting first generational status from GPA
summary(partCRegression)

# --- Calculating Sb ---
# Finding Sxx
avgGPA = mean(rawData$GPA)
Sxx = sum((rawData$GPA-avgGPA)^2)

# Finding Se
sumresids = sum(residuals(partCRegression)^2)
Se = sqrt(sumresids / (219-2))

Sb = Se / Sxx

plot(residuals(partCRegression), main="Residual Plot")

qqnorm(rawData$FirstGen)
