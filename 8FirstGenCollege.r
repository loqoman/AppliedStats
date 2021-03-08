# Darwin CLark 2021.03.08
# ~/Programming/AppliedStats/8FirstGenCollege.r

library(ggplot2)
library(readxl)  # Excel reading lib

rawData = read_excel("Programming/HAppliedStats/FirstYearGPA.xls")

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
