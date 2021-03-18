# Darwin Clark
# 021.03.16
# ~/Programming/AppliedStats/09RedVsBlueStates.r

library(ggplot2)
library(readxl)  # Excel reading lib

rawData = read_excel("Programming/AppliedStats/Election08.xls")

# orderedData = rawData[order(rawData$FirstGen), ] # What this line does is reorderes the rows based off of firstGen or not
# The first 195 lines are NOT first gen
# The remaining 24 lines ARE first gen

# === Work for part 1 ===
# Fitting a logistic regression by income per capita
OneARegression = glm(rawData$ObamaWin~rawData$Income, family=binomial("logit"))
summary(OneARegression)

# Fitting a logistic regression by percent HS
OneBRegression = glm(rawData$ObamaWin~rawData$HS, family=binomial("logit"))
summary(OneBRegression)

# Fitting a logistic regression by percent BS
OneCRegression = glm(rawData$ObamaWin~rawData$BA, family=binomial("logit"))
summary(OneCRegression)

# Fitting a logistic regression by percent party delta
OneDRegression = glm(rawData$ObamaWin~rawData$Dem.Rep, family=binomial("logit"))
summary(OneDRegression)




# === Work for part 2 ===

# === Work for part 5 ===
# Creating a col for income/1000
rawData$ScaledIncome = rawData$Income/1000

