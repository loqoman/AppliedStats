# Darwin Clark 2021.03.25
# ~/Programming/AppliedStats

library(ggplot2) # Used to make graphics
library(readxl)  # Excel reading lib
library(plyr)    # Used to count occurancs of a categorical data

# library(extrafont)
# loadfonts(device = "win")

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
  geom_bar(position="fill", width = 0.3) + # Changing to "fill" will cause normalize all bars to be the same height
                                            # Changing to "stack" will cause the height of the bars to be proportional to count
  theme_minimal() + 
  xlab(label="Groupings of Passengers (left = died, right = lived)") + 
  ylab(label="Percentage of the Respective Passenger Population") +
  ggtitle("Stacked bar Chart Comparing Proportion of Passenger Survival by Gender") +
  labs(fill="Gender")

ggplot(rawData, aes(rawData$Survived, fill = rawData$Sex)) +
  geom_bar(position="stack", width = 0.3) + # Changing to "fill" will cause normalize all bars to be the same height
  # Changing to "stack" will cause the height of the bars to be proportional to count
  theme_minimal() + 
  xlab(label="Groupings of Passengers (left = died, right = lived)") + 
  ylab(label="Number of Respective Passenger ") +
  ggtitle("Stacked bar Chart Comparing Count of Passenger Survival by Gender") +
  labs(fill="Gender")

# === Part 5 ===

ggplot(rawData, aes(factor(rawData$Survived), rawData$Age)) + 
  geom_boxplot() +
  coord_flip() + 
  xlab(label="Grouping of Passenger (top = survived, bottom = died)") + 
  ylab(label="Age of Passenger") +
  ggtitle("Horizontal Box Plots Comparing age Distribution of Living and Dead Passengers") 

# === Part 5 ===
class(rawData$Survived)
class(rawData$Sex)
class(rawData$Pclass)

ggplot(rawData, aes(rawData$Survived, fill = as.character(rawData$Pclass))) +
  geom_bar(position="stack", width = 0.3) + # Changing to "fill" will cause normalize all bars to be the same height
  # Changing to "stack" will cause the height of the bars to be proportional to count
  theme_minimal() + 
  xlab(label="Groupings of Passengers (left = died, right = lived)") + 
  ylab(label="Number of respective Passenger ") +
  ggtitle("Stacked bar Chart Comparing Count of Passenger Survivial by Ticket Class") +
  labs(fill="Ticket Class")


# === Part 7 ===
survivalByAge = glm(Survived~Age, family=binomial("logit"), data=rawData)
summary(survivalByAge)

# === Part 9 ===
survivalBySex = glm(Survived~Sex, family=binomial("logit"), data=rawData)
summary(survivalBySex)

# === Part 9 ===
survivalByClass = glm(Survived~Pclass, family=binomial("logit"), data=rawData)
summary(survivalByClass)

# === Part 13 ===
# lm16 = summary(lm(mydata$volume ~ (mydata$minwidth + mydata$maxwidth + mydata$material)))

# With no Interactive Terms
all3Variables1 = glm(Survived~(Pclass+Sex+Age), family=binomial("logit"), data=rawData)

# With 2 interacting terms
all3Variables2 = glm(Survived~(Pclass*Sex+Age), family=binomial("logit"), data=rawData)
all3Variables3 = glm(Survived~(Pclass+Sex*Age), family=binomial("logit"), data=rawData)
all3Variables4 = glm(Survived~(Pclass*Age+Sex), family=binomial("logit"), data=rawData)

# With all interacting terms
all3Variables4 = glm(Survived~(Pclass*Age*Sex), family=binomial("logit"), data=rawData)


summary(all3Variables4)
 

















