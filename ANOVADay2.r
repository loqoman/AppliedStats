# Darwin Clark
# #3 ANOVA Day 2
# ~/Programming/HAppliedStats/ANOVADay2.r

library(readxl)  # Excel reading lib
library(ggplot2) # ggplot for box plots

rawData = read_excel("Darwin Clark - #3 ANOVA Lesson Day II.xlsx")

refinedData = data.frame(imp = c(14.1, 13.6, 14.4, 14.3, 14.1),
                         parkay = c(12.8, 12.5, 13.4, 13, 12.3),
                         blueBonnet = c(13.5, 13.4, 14.1, 14.2, 13.825),
                         chiffon = c(13.2, 12.7, 12.6,13.9, 13.1),
                         mazola = c(16.8, 17.2, 16.4, 17.3, 18),
                         fleischman = c(18.1, 17.2, 18.7, 18.4, 18.1))

inputObject = (rawData$percent~rawData$treatment)

anovaObj = aov(inputObject)
summary(anovaObj)

# ### Doing box plots
boxplot(refinedData)

