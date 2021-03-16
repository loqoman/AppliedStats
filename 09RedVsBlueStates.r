# Darwin Clark
# 021.03.16
# ~/Programming/AppliedStats/09RedVsBlueStates.r

library(ggplot2)
library(readxl)  # Excel reading lib

rawData = read_excel("Programming/AppliedStats/Election08.xls")

# orderedData = rawData[order(rawData$FirstGen), ] # What this line does is reorderes the rows based off of firstGen or not
# The first 195 lines are NOT first gen
# The remaining 24 lines ARE first gen

# Work for part A
# Fitting a logistic regression by income per capita
OneARegression = 
summary(OneARegression)

# Fitting a logistic regression by percent HS
OneBRegression = 
summary(OneBRegression)

# Fitting a logistic regression by percent BS
OneCRegression = 
summary(OneCRegression)

# Fitting a logistic regression by percent party delta
OneDRegression = 
summary(OneDRegression)