library(ggplot2)

# Importing data from table
myData<-data.frame(y=c(20.5,20.1,20.0,21.7,20.7),
                   x1=c(18.6,23.9,20.9,18.7,21.1),
                   x2=c(22,19.1,20.7,18.1,21.7),
                   x3=c(17.1,21.1,19.4,20.9,23.7),
                   x4=c(18.5,21.3,20.6,18.1,17))

# Creating 4 differnt plots
ggplot(myData, aes(x = myData$x1, y=myData$y)) +
  geom_point(size = 2, shape = 15) +
  labs(title = "Scatter Plot comparing x1 and y data sets", 
       x = "x1",
       y = "y")

ggplot(myData, aes(x = myData$x2, y=myData$y)) +
  geom_point(size = 2, shape = 14) +
  labs(title = "Scatter Plot comparing x2 and y data sets", 
       x = "x2",
       y = "y")

ggplot(myData, aes(x = myData$x3, y=myData$y)) +
  geom_point(size = 2, shape = 13) +
  labs(title = "Scatter Plot comparing x3 and y data sets", 
       x = "x3",
       y = "y")

ggplot(myData, aes(x = myData$x4, y=myData$y)) +
  geom_point(size = 2, shape = 12) +
  labs(title = "Scatter Plot comparing x4 and y data sets", 
       x = "x4",
       y = "y")

# Fitting and summarizing each linear model
yvsx1 = lm(myData$y~myData$x1)
summary(yvsx1)

 yvsx1x2 = lm(myData$y~(myData$x1 + myData$x2))
summary(yvsx1x2)

yvsx1x2x3 = lm(myData$y~(myData$x1 + myData$x2 + myData$x3))
summary(yvsx1x2x3)

yvsx1x2x3x4 = lm(myData$y~(myData$x1 + myData$x2 + myData$x3 + myData$x4))
summary(yvsx1x2x3x4)
