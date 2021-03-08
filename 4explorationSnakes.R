# Darwin CLark, 2021.02.12
# ~/Programming/HAppliedStats/4ExplorationSnakes.r

library(ggplot2)
library(readxl)  # Excel reading lib

rawData = read_excel("Darwin Clark - #4 Exploration Snakes.xlsx")

# TODO: Build these for later questions
adultFemaleSnakes =  c(0.582, 0.585, 0.55, 0.554, 0.609, 0.545, 0.544, 0.6, 0.638)
juvenileFemaleSnakes = c(0.512, 0.556, 0.565, 0.417, 0.429, 0.523, 0.524, 0.438, 0.485)
adultMaleSnakes =    c(0.563, 0.556, 0.522, 0.541, 0.395, 0.561, 0.545, 0.512, 0.421)
juvenileMaleSnakes =   c(0.486, 0.492, 0.475, 0.464, 0.493, 0.495, 0.498, 0.51, 0.476)

maleSnakeIndex = c(.486, .492, .475, .563, .556, .522, .464, .493, .495, .541, .395, .561, .498, .51, .476, .545, .512, .421)
femaleSnakeIndex = c(c(0.582, 0.585, 0.550, 0.512, 0.556, 0.565, 0.554, 0.609, 0.545, 0.417, 0.429, 0.523, 0.544, 0.6, 0.638, 0.524, 0.438, 0.485))

juvenileSnakeIndex = c(0.486, 0.492, 0.475, 0.512, 0.556, 0.565, 0.464, 0.493, 0.495, 0.417, 0.429, 0.523, 0.498, 0.51, 0.476, 0.524, 0.438, 0.485)
adultSnakeIndex = c(0.582, 0.585, 0.55, 0.563, 0.556, 0.522, 0.554, 0.609, 0.545, 0.541, 0.395, 0.561, 0.544, 0.6, 0.638, 0.545, 0.512, 0.421)

boxPlotData = data.frame( adultFemale = adultFemaleSnakes,
                          juvenileFemale = juvenileFemaleSnakes,
                          adultMale = adultFemaleSnakes,
                          juvenileMale = juvenileMaleSnakes)


boxplot(boxPlotData)

lienarModelDataGood = data.frame(treatment = c(replicate(9, "adultFemale"), replicate(9, "juvenileFemale"), replicate(9,"adultMale"), replicate(9, "juvenileMale")),
                                 index = c(0.582, 0.585, 0.55, 0.554, 0.609, 0.545, 0.544, 0.6, 0.638, 0.512, 0.556, 0.565, 0.417, 0.429, 0.523, 0.524, 0.438, 0.485,0.563, 0.556, 0.522, 0.541, 0.395, 0.561, 0.545, 0.512, 0.421, 0.486, 0.492, 0.475, 0.464, 0.493, 0.495, 0.498, 0.51, 0.476))


inputANOVAmodel = lm(lienarModelDataGood$index~lienarModelDataGood$treatment)
thingy = aov(inputANOVAmodel)
summary(thingy)
# ### Question 2, building linear models ### 
# Single Variable
indexFromLength = summary(lm(rawData$latIndex~rawData$Length))
indexFromGender = summary(lm(rawData$latIndex~rawData$Gender))1
indexFromAge = summary(lm(rawData$latIndex~rawData$Age))

# BiVariate
indexFromGenderAndAge =    summary(lm(rawData$latIndex~(rawData$Gender + rawData$Age)))
indexFromGenderAndLength = summary(lm(rawData$latIndex~(rawData$Gender + rawData$Length)))
indexFromLengthAndAge =    summary(lm(rawData$latIndex~(rawData$Length + rawData$Age)))

# Trivariate
indexFromLengthAndAgeAndGender = summary(lm(rawData$latIndex~(rawData$Length + rawData$Age + rawData$Gender)))

# ## Graphing them ##

r2list = c(indexFromLength[["r.squared"]], indexFromGender[["r.squared"]], indexFromAge[["r.squared"]], indexFromGenderAndAge[["r.squared"]], indexFromGenderAndLength[["r.squared"]],indexFromLengthAndAge[["r.squared"]],
           indexFromLengthAndAgeAndGender[["r.squared"]])
r2list

# adj.r.squared
r2adjlist = c(indexFromLength[["adj.r.squared"]], indexFromGender[["adj.r.squared"]], indexFromAge[["adj.r.squared"]], indexFromGenderAndAge[["adj.r.squared"]], indexFromGenderAndLength[["adj.r.squared"]],indexFromLengthAndAge[["adj.r.squared"]],
              indexFromLengthAndAgeAndGender[["adj.r.squared"]])


r2adjlist

# S_e 
selist = c(indexFromLength[["sigma"]], indexFromGender[["sigma"]], indexFromAge[["sigma"]], indexFromGenderAndAge[["sigma"]], indexFromGenderAndLength[["sigma"]],indexFromLengthAndAge[["sigma"]],
                    indexFromLengthAndAgeAndGender[["sigma"]])


graphingData = data.frame(lm=c(1,2,3,4,5,6,7),
                          se=selist, r2=r2list, r2adj=r2adjlist)

ggplot(graphingData, aes(x=graphingData$lm)) + 
  geom_line(aes(y=graphingData$r2adj), color="deepskyblue4", size=2) +
  #geom_line(aes(y=graphingData$r2), color="slateblue2") +
  labs(title = "Line Plot Comparing R-Squared-Adjusted to Linear Models with Increasing Determinants", 
       x = "Numerical Value of Linear Model (In general, larger x-value means more determinants) (See table on writeup)",
       y = "R-Squared-Adjusted Value for the Corresponding Linear Model") 
  #scale_x_discrete()

ggplot(graphingData, aes(x=graphingData$lm)) + 
  geom_line(aes(y=graphingData$r2), color="deepskyblue4", size=2) +
  #geom_line(aes(y=graphingData$r2), color="slateblue2") +
  labs(title = "Line Plot Comparing R-Squared Value to Linear Models with Increasing Determinants", 
       x = "Numerical Value of Linear Model (In general, larger x-value means more determinants) (See table on writeup)",
       y = "R-Squared Value for the Corresponding Linear Model") +
  scale_x_discrete()

ggplot(graphingData, aes(x=graphingData$lm)) + 
  geom_line(aes(y=graphingData$se), color="deepskyblue4", size=2) +
  #geom_line(aes(y=graphingData$r2), color="slateblue2") +
  labs(title = "Line Plot Comparing Se Value to Linear Models with Increasing Determinants", 
       x = "Numerical Value of Linear Model (In general, larger x-value means more determinants) (See table on writeup)",
       y = "Se Value for the Corresponding Linear Model") +
  scale_x_discrete()


# ### ??????? ###
anovaObj = aov(inputObject)
summary(anovaObj)

# ### Doing box plots
boxplot(refinedData)

