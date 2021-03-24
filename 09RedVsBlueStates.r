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
OneARegression = glm(ObamaWin~Income, family=binomial("logit"), data=rawData)
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
# === Work for part 4 ===
# Getting our input list
inputIncomes = data.frame(Income=seq(28845, 61052)) # Creates every income within the data set 28845, 61052

# data.frame(Income=inputIncomes)
outPutProbability = predict(OneARegression, newdata=inputIncomes, type="response")

outPutOdds = outPutProbability / (1-outPutProbability)
allOddsRatio = c()

for (index in seq(1, 32207)) {
  print(outPutOdds[as.integer(index)] / outPutOdds[as.integer(index)+1])
  allOddsRatio = c(allOddsRatio, (outPutOdds[as.integer(index+1)] / outPutOdds[as.integer(index)]))
  #append(allOddsRatio, )
}


# === Work for part 5 ===
# Creating a col for income/1000
rawData$ScaledIncome = rawData$Income/1000

Part5Regression = glm(ObamaWin~ScaledIncome, family=binomial("logit"), data=rawData)
summary(Part5Regression)

# === Work for part 6 ===
# Getting our input list
inputIncomes = data.frame(ScaledIncome=seq(28.845, 61.052, by=0.001)) # Creates every income within the data set 28845, 61052

# data.frame(Income=inputIncomes)
outPutProbability = predict(Part5Regression, newdata=inputIncomes, type="response")

outPutOdds = outPutProbability / (1-outPutProbability)
allOddsRatio = c()

for (index in seq(1, 32207)) {
  print(outPutOdds[as.integer(index)] / outPutOdds[as.integer(index)+1])
  allOddsRatio = c(allOddsRatio, (outPutOdds[as.integer(index+1)] / outPutOdds[as.integer(index)]))
  #append(allOddsRatio, )
}

sd(allOddsRatio)
