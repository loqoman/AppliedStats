#This is a demo to just show you some useful tool.

#You must ALWAYS set your working directory FIRST!!!!
#Look under the 'Session' tab.

setwd("H:/Applied Statistics/2020/Unit 2/R Introduction/Assignment #13")
data("mtcars")
head(mtcars)

#If you are managing multiple data sets:
plot(mtcars$wt,mtcars$mpg)


#A short cut, if you are dealing entirely with one data set:
attach(mtcars)

#Basic Scatter plot:
plot(wt,mpg)

#Add a title:
plot(wt,mpg,main="MPG as a Function of Weight")

#Add Labels for your x-and y-axis:
plot(wt,mpg,main="MGP as a Function of Weight",xlab="Weight in lbs",ylab="miles per gallon")

#Add Symbols:
plot(wt,mpg,main="MGP as a Function of Weight",xlab="Weight in lbs",ylab="miles per gallon",pch=22)

#Add colors to your symbols:
plot(wt,mpg,main="MGP as a Function of Weight",xlab="Weight in lbs",ylab="miles per gallon",
     pch=22,col="red",bg="grey")

#Add colors to your labels:
plot(wt,mpg,main="MGP as a Function of Weight",xlab="Weight in lbs",ylab="miles per gallon",
     pch=22,col="red",bg="grey",col.main="blue",col.lab="chocolate1")

#Calculate a linear model:
lm(mpg~wt)


#You need to store your linear model as a variable/object:
regres1<-lm(mpg~wt)

#To look at a summary of your model:
summary(regres1)
anova(regres1)

#Plot your line:
abline(regres1)

#Or, look at or plot your fitted values for your given data:
fitted(regres1)
head(mtcars)

predict(regres1)

lines(wt,fitted(regres1))
segments(wt,fitted(regres1),wt,mpg)


#If you want the residuals:
resid(regres1)
plot(wt,resid(regres1))
abline(0,0)
plot(wt,resid(regres1), main="Residual Plot: Weight versus Residuals")
abline(0,0)


#Short cut:
plot(regres1)


qqnorm(resid(regres1))
qqline(resid(regres1))
shapiro.test(resid(regres1))


#Make sure to detach data
detach(mtcars)





#Applied Stats

#import excel sheets filled with data (.csv)
# summarizing single and bi-variate date
#t-test and 2 sample t difference of means, or 2 sample z difference of proportions
#boxplots, histograms, probability plots


#Lets start. Always start by setting your working directory (WD)

setwd("H:/Applied Statistics/2020/Unit 2/R Introduction/Assignment #13")


#Go to Google Classroom and download the excel (csv) file and save it to your working directory.

#Import your data called 'range'
range <- read.csv("H:/Applied Statistics/2020/Unit 2/R Introduction/Assignment #13/range.csv")
View(range)

#The command head(range) shows the first six lines of data and titles to the columns

head(range)

range

#Find the summary statistics on the Pre column

summary(range$Pre)

#You can also use the attach command

attach(range)

summary(Pre)

#Lets compile

#mean, sd, var, min, max, median, range

mean(Pre)

sd(Pre)

min(Pre)

#Plot bi-variate data

plot(Pre,Post)

plot(Pre,Post,main="Range of Motion (Pre and Post Treatment",sub="Year 2015",xlab="Range of Motion (Pre)",ylab="Range of Motion (Post)")

#Pick ponits pch=, then add color..For symbols 21 through 25, specify border color (col=) and fill color (bg=)

plot(Pre,Post,main="Range of Motion (Pre and Post Treatment",
     sub="Year 2015",xlab="Range of Motion (Pre)",ylab="Range of Motion (Post)", pch=22, col="red",bg="grey")

#Correlation?

cor(Pre,Post)

#Regression model

lm(Pre~Post)

regression<-lm(Pre~Post)

plot(regression)

summary(regression)


#Plots

boxplot(Pre)
boxplot(Pre, horizontal = TRUE)
boxplot(Pre,Post)
boxplot(Pre,Post, horizontal = TRUE)
boxplot(Pre,Post,xlab="Pre                     Range of Motion                      Post")



#Histograms

hist(Pre)
hist(Pre, breaks=10)


#t.tests and 2 sample t.tests

t.test(Pre,mu=43)
t.test(Pre,mu=43,alternative = c("greater"))
t.test(Pre,Post)

