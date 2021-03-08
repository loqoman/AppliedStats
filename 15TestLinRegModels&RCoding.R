# Create Data Sets

girth = c(96,105.1,108.1,109.1,110.2,114.2,121.3,124.1,131.1,135.3,137.2,138.2,140.3,142.4,
          155.3,157.4,157.4,159.3,162.3)

weight = c(98.6, 195.8, 162.8, 195.8, 182.2, 170.6, 229.9, 225, 211.4, 230.8, 224, 264.9, 
            239.6, 263.9, 335.8, 283.3, 292.1, 298.9, 337.8)

# A plot to Visualize
plot(girth,weight,main="Weight vs. Girth for Elk in Custer State Park ", 
     xlab="Girth (cm)", ylab="Weight (kg)", pch=10, 
     col="brown",bg="darkblue", col.main="bisque4")

# Creating the linear model
regression = lm(weight~girth)
summary(regression)

# Finding the sum of the squared residuals
sumresids = sum(residuals(regression)^2)

# Calculating Se
Se = sqrt(sumresids / (19-2))
print(Se)

# Anova table
anova(regression)

# Calculating Sb
# Finding Sxx
avgGirth = mean(girth)

Sxx = sum((girth-avgGirth)^2)

Sb = Se / Sxx

# Conditions for Model Utility Test (#5)

# Residual plot
plot(girth,residuals(regression),main="(Residuals) Weight vs. Girth for Elk in Custer State Park ", 
     xlab="Girth (cm)", ylab="Residual Weight (kg)", pch=10, 
     col="brown",bg="darkblue", col.main="bisque4")

# Normal Probability plot
qqnorm(weight)

# Standarised residuals plot
plot(residuals(regression)/Se,weight,main="(Std. Residuals) Weight vs. Girth for Elk in Custer State Park ", 
     xlab="Girth (cm)", ylab="Residual Weight (kg)", pch=10, 
     col="brown",bg="darkblue", col.main="bisque4")
