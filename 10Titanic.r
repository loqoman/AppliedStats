# Darwin Clark 2021.03.25
# ~/Programming/AppliedStats

library(ggplot2) # Used to make graphics
library(readxl)  # Excel reading lib
library(plyr)    # Used to count occurancs of a categorical data

library(extrafont)
loadfonts(device = "win")

rawData = read_excel("Programming/AppliedStats/titanic.xls")

# === part 1 === 

count(rawData, c("Sex", "Survived"))  # This actually makes everything for you lmao
 # female 314
 # male 573

count(rawData, c("Pclass", "Survived", "Sex"))  # This acutally makes everything for you lmao

# === part 2 == 
# Creating an ordered data set based off of Class

orderedData = rawData[order(rawData$Pclass), ] # What this line does is reorderes the rows based off of firstGen or not

# The first 216 lines are are all class 1
# Lines 401 to 887 are 3rd class
# This gives you data.frame() objects that are all 3 classes
firstClass  = head(orderedData, 216)
mean(firstClass$Fare) # 84.15469

# Gotta do some clever head() magic B) 
secondClass = head(tail(orderedData, 671), 184)
mean(secondClass$Fare)

thirdClass  = tail(orderedData, 486)
mean(thirdClass$Fare)


# === Part 3 ===
ggplot(rawData, aes(rawData$Survived, fill = rawData$Sex)) +
  geom_bar(position="fill") +
  theme_minimal() + xlab() + ylab()
