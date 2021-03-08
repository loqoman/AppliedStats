library(ggplot2)

# Getting MTCars
data("mtcars")

# Create a box plot using ggplot2 showing the range of values of 1/4 mile time (qsec) for each
# transmission type (am, 0 = automatic, 1 = manual) from the mtcars data set.
mtcars$am = as.factor(mtcars$am)
ggplot(mtcars, aes(y=mtcars$qsec, x=mtcars$am, fill=mtcars$am)) + 
  geom_boxplot() + 
  labs(title= "Box Plot Comparing Quarter-Mile Speed to Transmission Type",
  y = "Quarter-Mile Speed (S)", x = "Transmission Type") + 
  labs(fill = "Transmission Type") + 
  scale_fill_discrete(labels = c("Automatic", "Manuel")) 

# Create a bar graph using ggplot2 that shows the number of each carb type in mtcars.
ggplot(mtcars, aes(x=mtcars$carb)) + 
  geom_bar() +
  labs(title = "Bar Chart Illistrating Number of Carburetors", 
      x = "Number of Carburetors",
      y = "Models of Cars With That Number of Carburetors")

# Next show one stacked bar graph using ggplot2 of the number of each gear type and how they
# are further divided out by cyl. (Each cylinder type should have its own stacked bar graph.)
ggplot(mtcars) +
  geom_bar(aes(x = mtcars$gear, fill=factor(cyl)), width=0.4) +
  labs(title = "Bar Chart Illistrating Count of Cars by Gear Type, Colored By Cylinder Count", 
       x = "Gear Count",
       y = "Models of Cars With That Number of Gears") +
  scale_fill_discrete(labels = c("4-Cylinder", "6-Cylinder", "8-Cylinder")) +
  labs(fill = "Cylinder Type")  
  
# Create a scatter plot using ggplot2 showing the relationship between wt and mpg.
ggplot(mtcars, aes(x = mtcars$wt, y=mtcars$mpg)) +
  geom_point() +
  labs(title = "Scatter Plot Comparing Weight of Cars vs. MPG ", 
       x = "Car Weight (Tons)",
       y = "Miles-Per-Gallon") 

# Design a visualization of your choice that I have not included using ggplot2 using the data in
# mtcars, and then write a brief summary about why you chose that visualization and what it
# shows the reader.

# X    = HP
# Y    = MPG
# Size = Gear
ggplot(mtcars, aes(x = mtcars$hp, y=mtcars$mpg)) +
  geom_point(aes(color=gear), size = 3) +
  labs(title = "Scatter Plot Illistrating Weight of Cars vs. MPG vs. Number of Forward Gears", 
       x = "Gross Horsepower",
       y = "Miles-Per-Gallon")


  
  
