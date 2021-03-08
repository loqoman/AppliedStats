library(ggplot2)
# --- Question 1 ---
# Setting up Data (Question 1)
mydata = data.frame(
  y=c(14.7,48,25.6,10,16,16.8,20.7,38.8,16.9, 27, 16, 24.9, 7.3, 12.8),
  x1=c(8.9,36.6,36.8, 6.1, 6.9, 6.9, 7.3, 8.4, 6.5, 8, 4.5, 9.9, 2.9, 2),
  x2=c(31.5,27,25.9,39.1, 39.2, 38.3, 33.9, 33.8, 27.9, 33.1, 26.3, 27.8, 34.6, 36.4),
  haty=c(23.35,46.38,27.13,10.99,14.1,16.54,23.34,25.43,15.63,24.29,15.36,29.61,15.38,7.96),
  resids=c(-8.65,1.62,-1.53,-.99, 1.9, .26, -2.64,13.37, 1.27, 2.71, 0.64, -4.71, -8.08, 4.84))
  
# Calculating SSResid
SSResid = sum((mydata$y - mydata$haty)^2)
print(SSResid)

# Calculating SSTot 
ybar = mean(mydata$y)
SSTot = sum((mydata$y - ybar)^2)
print(SSTot)

# Checks for F-Test
# Normal Probability Plot
qqnorm(mydata$resids, main= "Normal Probability Plot of Std. Residuals for Thawed Permafrost Soils", 
       xlab = "Predicted Z-Scores", ylab = "Standardised Residuals")


# Residual plot, 'E' Check
ggplot(mydata, aes(y=mydata$resids, x=mydata$haty)) +
  geom_point(size = 2, color = "cornflowerblue", ) +
  labs(title = "Scatter Plot Illistrating the Precited Y-Values vs. Calculated Residuals", 
       x = "Predicted Y-Values",
       y = "Calculated Residuals")

# --- Question 2 --- 
# Setting Up Data (Question 2)
mydata = data.frame(
  volume=c(125,135,175,285,330,90,120,520,330,570,340,175,240,240,360,310,635,1250,650,303,315,305,245,200,1205,2330,730),
  height=c(7.7,6.2,8.5,10.4,8,8.7,10.2,10.5,3.4,6.9,10.9,9.7,10.1,13,13,11,8.7,17.1,16.5,16.5,9.7,17.8,14,13.6,27.9,19.5,13.8),
  minwidth=c(1.8,2.7,2,2.6,3.15,1.8,1.5,3.8,5,4.75,2.8,2.1,2.2,2.6,2.6,2.9,5.1,10.2,3.5,1.2,1.7,1.75,1.7,1.2,1.2,7.5,4.25),
  maxwidth=c(2.5,2.9,2.15,2.9,3.2,2,1.6,4.8,5.8,5.9,2.9,2.45,2.6,2.6,2.7,3.1,5.1,10.2,3.5,2.7,3,2.7,2.5,2.4,4.4,7.5,4.25),
  elongation=c(1.5,1.07,1.98,1.79,1.25,2.17,3.19,1.09,0.29,0.59,1.88,1.98,1.94,2.5,2.41,1.77,.85,.84,2.36,3.06,1.62,3.3,2.8,2.83,3.17,1.3,1.62),
  material=c(0,0,0,0,1,0,0,1,1,1,2,1,0,0,0,0,3,3,0,0,0,0,0,0,1,2,2))

# Predicting volume from minimum width, maxiumum width, material, height, and elongation score
# This is gonna be a bunch of code, buckle in
# 1-Variable Base Models (5C1 = 5)
lm1 = summary(lm(mydata$volume ~ mydata$minwidth))
lm2 = summary(lm(mydata$volume ~ mydata$maxwidth))
lm3 = summary(lm(mydata$volume ~ mydata$elongation))
lm4 = summary(lm(mydata$volume ~ mydata$material))
lm5 = summary(lm(mydata$volume ~ mydata$height))
# 2-Varaiable Base Models (5C2 = 10)
lm6 = summary(lm(mydata$volume ~ (mydata$minwidth + mydata$maxwidth)))
lm7 = summary(lm(mydata$volume ~ (mydata$minwidth + mydata$material)))
lm8 = summary(lm(mydata$volume ~ (mydata$maxwidth + mydata$material)))
lm9 = summary(lm(mydata$volume ~ (mydata$elongation + mydata$material)))
lm10 = summary(lm(mydata$volume ~ (mydata$elongation + mydata$minwidth)))
lm11 = summary(lm(mydata$volume ~ (mydata$elongation + mydata$maxwidth)))
lm12 = summary(lm(mydata$volume ~ (mydata$height + mydata$maxwidth)))
lm13 = summary(lm(mydata$volume ~ (mydata$height + mydata$minwidth)))
lm14 = summary(lm(mydata$volume ~ (mydata$height + mydata$elongation)))
lm15 = summary(lm(mydata$volume ~ (mydata$height + mydata$material)))
# 2-Variable Interaction Models (2C2 = 1)
lm6i  = summary(lm(mydata$volume ~ (mydata$minwidth * mydata$maxwidth)))
lm7i  = summary(lm(mydata$volume ~ (mydata$minwidth * mydata$material)))
lm8i  = summary(lm(mydata$volume ~ (mydata$maxwidth * mydata$material)))
lm9i  = summary(lm(mydata$volume ~ (mydata$elongation * mydata$material)))
lm10i = summary(lm(mydata$volume ~ (mydata$elongation * mydata$minwidth)))
lm11i = summary(lm(mydata$volume ~ (mydata$elongation * mydata$maxwidth)))
lm12i = summary(lm(mydata$volume ~ (mydata$height * mydata$maxwidth)))
lm13i = summary(lm(mydata$volume ~ (mydata$height * mydata$minwidth)))
lm14i = summary(lm(mydata$volume ~ (mydata$height * mydata$elongation)))
lm15i = summary(lm(mydata$volume ~ (mydata$height * mydata$material))) #24
# 3-Variable Base Models (5C3= 10)
lm16 = summary(lm(mydata$volume ~ (mydata$minwidth + mydata$maxwidth + mydata$material)))
lm17 = summary(lm(mydata$volume ~ (mydata$minwidth + mydata$maxwidth + mydata$height)))
lm18 = summary(lm(mydata$volume ~ (mydata$minwidth + mydata$maxwidth + mydata$elongation)))
lm19 = summary(lm(mydata$volume ~ (mydata$elongation + mydata$height + mydata$material)))
lm20 = summary(lm(mydata$volume ~ (mydata$elongation + mydata$height + mydata$minwidth)))
lm21 = summary(lm(mydata$volume ~ (mydata$elongation + mydata$height + mydata$maxwidth)))
lm22 = summary(lm(mydata$volume ~ (mydata$elongation + mydata$material + mydata$maxwidth)))
lm23 = summary(lm(mydata$volume ~ (mydata$elongation + mydata$material + mydata$minwidth)))
lm24 = summary(lm(mydata$volume ~ (mydata$material + mydata$height + mydata$minwidth)))
lm25 = summary(lm(mydata$volume ~ (mydata$material + mydata$height + mydata$maxwidth)))
# 3-Variable Interaction Models (3C2 = 3)
lm16i   = summary(lm(mydata$volume ~ (mydata$minwidth * mydata$maxwidth + mydata$material)))
lm16ii  = summary(lm(mydata$volume ~ (mydata$minwidth + mydata$maxwidth * mydata$material)))
lm16iii = summary(lm(mydata$volume ~ (mydata$minwidth * mydata$material + mydata$maxwidth)))
lm16iiii= summary(lm(mydata$volume ~ (mydata$minwidth * mydata$material * mydata$maxwidth)))

lm17i   = summary(lm(mydata$volume ~ (mydata$minwidth * mydata$maxwidth + mydata$height)))
lm17ii  = summary(lm(mydata$volume ~ (mydata$minwidth + mydata$maxwidth * mydata$height)))
lm17iii = summary(lm(mydata$volume ~ (mydata$minwidth * mydata$height + mydata$maxwidth)))
lm17iiii= summary(lm(mydata$volume ~ (mydata$minwidth * mydata$height * mydata$maxwidth)))

lm18i   = summary(lm(mydata$volume ~ (mydata$minwidth * mydata$maxwidth + mydata$elongation)))
lm18ii  = summary(lm(mydata$volume ~ (mydata$minwidth + mydata$maxwidth * mydata$elongation)))
lm18iii = summary(lm(mydata$volume ~ (mydata$minwidth * mydata$elongation + mydata$maxwidth )))
lm18iiii= summary(lm(mydata$volume ~ (mydata$minwidth * mydata$elongation * mydata$maxwidth )))

lm19i   = summary(lm(mydata$volume ~ (mydata$elongation * mydata$height + mydata$material)))
lm19ii  = summary(lm(mydata$volume ~ (mydata$elongation + mydata$height * mydata$material)))
lm19iii = summary(lm(mydata$volume ~ (mydata$elongation * mydata$material + mydata$height)))
lm19iiii= summary(lm(mydata$volume ~ (mydata$elongation * mydata$material * mydata$height)))

lm20i   = summary(lm(mydata$volume ~ (mydata$elongation * mydata$height + mydata$minwidth)))
lm20ii  = summary(lm(mydata$volume ~ (mydata$elongation + mydata$height * mydata$minwidth)))
lm20iii = summary(lm(mydata$volume ~ (mydata$elongation * mydata$minwidth + mydata$height)))
lm20iiii= summary(lm(mydata$volume ~ (mydata$elongation * mydata$material * mydata$height)))

lm21i   = summary(lm(mydata$volume ~ (mydata$elongation * mydata$height + mydata$maxwidth)))
lm21ii  = summary(lm(mydata$volume ~ (mydata$elongation + mydata$height * mydata$maxwidth)))
lm21iii = summary(lm(mydata$volume ~ (mydata$elongation * mydata$maxwidth + mydata$height)))
lm21iiii= summary(lm(mydata$volume ~ (mydata$elongation * mydata$maxwidth * mydata$height)))

lm22i   = summary(lm(mydata$volume ~ (mydata$elongation * mydata$material + mydata$maxwidth)))
lm22ii  = summary(lm(mydata$volume ~ (mydata$elongation + mydata$material * mydata$maxwidth)))
lm22iii = summary(lm(mydata$volume ~ (mydata$elongation * mydata$maxwidth + mydata$material)))
lm22iiii = summary(lm(mydata$volume ~ (mydata$elongation * mydata$maxwidth * mydata$material)))

lm23i   = summary(lm(mydata$volume ~ (mydata$elongation * mydata$material + mydata$minwidth)))
lm23ii  = summary(lm(mydata$volume ~ (mydata$elongation + mydata$material * mydata$minwidth)))
lm23iii = summary(lm(mydata$volume ~ (mydata$elongation * mydata$minwidth + mydata$material )))
lm23iiii = summary(lm(mydata$volume ~ (mydata$elongation * mydata$minwidth * mydata$material )))

lm24i   = summary(lm(mydata$volume ~ (mydata$material * mydata$height + mydata$minwidth)))
lm24ii  = summary(lm(mydata$volume ~ (mydata$material + mydata$height * mydata$minwidth)))
lm24iii = summary(lm(mydata$volume ~ (mydata$material * mydata$minwidth + mydata$height)))
lm24iiii = summary(lm(mydata$volume ~ (mydata$material * mydata$minwidth * mydata$height)))

lm25i   = summary(lm(mydata$volume ~ (mydata$material * mydata$height + mydata$maxwidth)))
lm25ii  = summary(lm(mydata$volume ~ (mydata$material + mydata$height * mydata$maxwidth)))
lm25iii = summary(lm(mydata$volume ~ (mydata$material * mydata$maxwidth+ mydata$height)))
lm25iiii = summary(lm(mydata$volume ~ (mydata$material * mydata$maxwidth * mydata$height)))

# 4-Variable Base Models (5C4= 5)
lm26 = summary(lm(mydata$volume ~ (mydata$material + mydata$height + mydata$minwidth + mydata$maxwidth)))
lm27 = summary(lm(mydata$volume ~ (mydata$elongation + mydata$height + mydata$minwidth + mydata$maxwidth)))
lm28 = summary(lm(mydata$volume ~ (mydata$material + mydata$elongation + mydata$minwidth + mydata$maxwidth)))
lm29 = summary(lm(mydata$volume ~ (mydata$material + mydata$height + mydata$elongation + mydata$maxwidth)))
lm30 = summary(lm(mydata$volume ~ (mydata$material + mydata$height + mydata$elongation + mydata$minwidth)))
# 4-Variable Interaction Models (4C2 = 6) (Probabbly the largest set of models)
# lm26
lm26i    = summary(lm(mydata$volume ~ (mydata$material * mydata$height + mydata$minwidth + mydata$maxwidth))) # 1
lm26ii   = summary(lm(mydata$volume ~ (mydata$material + mydata$height * mydata$minwidth + mydata$maxwidth))) # 1
lm26iii  = summary(lm(mydata$volume ~ (mydata$material + mydata$height + mydata$minwidth * mydata$maxwidth))) # 1 
lm26iiii = summary(lm(mydata$volume ~ (mydata$material + mydata$height * mydata$maxwidth + mydata$minwidth))) # 1
lm26v    = summary(lm(mydata$volume ~ (mydata$material * mydata$maxwidth + mydata$height + mydata$minwidth))) # 1
lm26vi   = summary(lm(mydata$volume ~ (mydata$material * mydata$minwidth + mydata$height + mydata$maxwidth))) # 1
lm26vii  = summary(lm(mydata$volume ~ (mydata$material * mydata$height + mydata$minwidth * mydata$maxwidth))) # 2
lm26viii = summary(lm(mydata$volume ~ (mydata$material * mydata$maxwidth + mydata$height * mydata$minwidth))) # 2
lm26ix   = summary(lm(mydata$volume ~ (mydata$material * mydata$minwidth + mydata$height * mydata$maxwidth))) # 2
lm26x    = summary(lm(mydata$volume ~ (mydata$material * mydata$height * mydata$minwidth + mydata$maxwidth))) # 3
lm26xi   = summary(lm(mydata$volume ~ (mydata$material + mydata$height * mydata$minwidth * mydata$maxwidth))) # 3
lm26xii  = summary(lm(mydata$volume ~ (mydata$material * mydata$height * mydata$maxwidth + mydata$minwidth))) # 3
lm26xiii = summary(lm(mydata$volume ~ (mydata$material * mydata$minwidth * mydata$maxwidth + mydata$height))) # 3
lm26xiiii = summary(lm(mydata$volume ~ (mydata$material * mydata$minwidth * mydata$maxwidth * mydata$height)))# 4


# lm27
lm27i    = summary(lm(mydata$volume ~ (mydata$elongation * mydata$height + mydata$minwidth + mydata$maxwidth))) # 1
lm27ii   = summary(lm(mydata$volume ~ (mydata$elongation + mydata$height * mydata$minwidth + mydata$maxwidth))) # 1
lm27iii  = summary(lm(mydata$volume ~ (mydata$elongation + mydata$height + mydata$minwidth * mydata$maxwidth))) # 1
lm27iiii = summary(lm(mydata$volume ~ (mydata$elongation * mydata$minwidth + mydata$maxwidth + mydata$height))) # 1
lm27v    = summary(lm(mydata$volume ~ (mydata$elongation + mydata$minwidth + mydata$maxwidth * mydata$height))) # 1
lm27vi   = summary(lm(mydata$volume ~ (mydata$elongation * mydata$maxwidth + mydata$height + mydata$minwidth))) # 1
lm27vii  = summary(lm(mydata$volume ~ (mydata$elongation * mydata$height + mydata$minwidth * mydata$maxwidth))) # 2
lm27viii = summary(lm(mydata$volume ~ (mydata$elongation * mydata$minwidth + mydata$maxwidth * mydata$height))) # 2
lm27ix   = summary(lm(mydata$volume ~ (mydata$elongation * mydata$maxwidth + mydata$height * mydata$minwidth))) # 2
lm27x    = summary(lm(mydata$volume ~ (mydata$elongation * mydata$height * mydata$minwidth + mydata$maxwidth))) # 3
lm27xi   = summary(lm(mydata$volume ~ (mydata$elongation + mydata$height * mydata$minwidth * mydata$maxwidth))) # 3
lm27xii  = summary(lm(mydata$volume ~ (mydata$elongation * mydata$minwidth * mydata$maxwidth + mydata$height))) # 3
lm27xiii = summary(lm(mydata$volume ~ (mydata$elongation * mydata$height * mydata$maxwidth + mydata$minwidth))) # 3
lm27xiiii= summary(lm(mydata$volume ~ (mydata$elongation * mydata$height * mydata$minwidth * mydata$maxwidth))) # 4
# lm28
lm28i    = summary(lm(mydata$volume ~ (mydata$material * mydata$elongation + mydata$minwidth + mydata$maxwidth))) # 1
lm28ii   = summary(lm(mydata$volume ~ (mydata$material + mydata$elongation * mydata$minwidth + mydata$maxwidth))) # 1
lm28iii  = summary(lm(mydata$volume ~ (mydata$material + mydata$elongation + mydata$minwidth * mydata$maxwidth))) # 1
lm28iiii = summary(lm(mydata$volume ~ (mydata$material * mydata$minwidth + mydata$maxwidth + mydata$elongation))) # 1
lm28v    = summary(lm(mydata$volume ~ (mydata$material * mydata$maxwidth + mydata$elongation + mydata$minwidth))) # 1
lm28vi   = summary(lm(mydata$volume ~ (mydata$material + mydata$minwidth + mydata$maxwidth * mydata$elongation))) # 1
lm28vii  = summary(lm(mydata$volume ~ (mydata$material * mydata$elongation + mydata$minwidth * mydata$maxwidth))) # 2
lm28viii = summary(lm(mydata$volume ~ (mydata$material * mydata$minwidth + mydata$maxwidth * mydata$elongation))) # 2
lm28ix   = summary(lm(mydata$volume ~ (mydata$material * mydata$maxwidth + mydata$elongation * mydata$minwidth))) # 2
lm28x    = summary(lm(mydata$volume ~ (mydata$material * mydata$elongation * mydata$minwidth + mydata$maxwidth))) # 3
lm28xi   = summary(lm(mydata$volume ~ (mydata$material + mydata$elongation * mydata$minwidth * mydata$maxwidth))) # 3
lm28xii  = summary(lm(mydata$volume ~ (mydata$material * mydata$elongation * mydata$maxwidth + mydata$minwidth))) # 3
lm28xiii = summary(lm(mydata$volume ~ (mydata$material * mydata$elongation * mydata$minwidth + mydata$maxwidth))) # 3
lm28xiiii= summary(lm(mydata$volume ~ (mydata$material * mydata$elongation * mydata$minwidth * mydata$maxwidth))) # 4
# lm29
lm29i    = summary(lm(mydata$volume ~ (mydata$material * mydata$height + mydata$elongation + mydata$maxwidth))) # 1
lm29ii   = summary(lm(mydata$volume ~ (mydata$material + mydata$height * mydata$elongation + mydata$maxwidth))) # 1
lm29iii  = summary(lm(mydata$volume ~ (mydata$material + mydata$height + mydata$elongation * mydata$maxwidth))) # 1
lm29iiii = summary(lm(mydata$volume ~ (mydata$material * mydata$elongation + mydata$maxwidth + mydata$height))) # 1
lm29v    = summary(lm(mydata$volume ~ (mydata$material * mydata$maxwidth + mydata$height + mydata$elongation))) # 1
lm29vi   = summary(lm(mydata$volume ~ (mydata$material + mydata$height * mydata$maxwidth + mydata$elongation))) # 1
lm29vii  = summary(lm(mydata$volume ~ (mydata$material * mydata$height + mydata$elongation * mydata$maxwidth))) # 2
lm29viii = summary(lm(mydata$volume ~ (mydata$material * mydata$elongation + mydata$maxwidth * mydata$height))) # 2
lm29ix   = summary(lm(mydata$volume ~ (mydata$height * mydata$elongation + mydata$maxwidth * mydata$material))) # 2
lm29x    = summary(lm(mydata$volume ~ (mydata$material * mydata$height * mydata$elongation + mydata$maxwidth))) # 3
lm29xi   = summary(lm(mydata$volume ~ (mydata$material + mydata$height * mydata$elongation * mydata$maxwidth))) # 3
lm29xii  = summary(lm(mydata$volume ~ (mydata$material * mydata$height * mydata$maxwidth + mydata$elongation))) # 3
lm29xiii = summary(lm(mydata$volume ~ (mydata$material * mydata$elongation * mydata$maxwidth + mydata$height))) # 3
lm29xiiii= summary(lm(mydata$volume ~ (mydata$material * mydata$height * mydata$elongation * mydata$maxwidth))) # 4
# lm30
lm30i    = summary(lm(mydata$volume ~ (mydata$material * mydata$height + mydata$elongation + mydata$minwidth))) # 1 
lm30ii   = summary(lm(mydata$volume ~ (mydata$material + mydata$height * mydata$elongation + mydata$minwidth))) # 1 
lm30iii  = summary(lm(mydata$volume ~ (mydata$material + mydata$height + mydata$elongation * mydata$minwidth))) # 1 
lm30iiii = summary(lm(mydata$volume ~ (mydata$material * mydata$elongation + mydata$minwidth + mydata$height))) # 1 
lm30v    = summary(lm(mydata$volume ~ (mydata$material * mydata$minwidth + mydata$elongation + mydata$height))) # 1 
lm30vi   = summary(lm(mydata$volume ~ (mydata$minwidth * mydata$height + mydata$elongation + mydata$material))) # 1 
lm30vii  = summary(lm(mydata$volume ~ (mydata$material * mydata$height + mydata$elongation * mydata$minwidth))) # 2 
lm30viii = summary(lm(mydata$volume ~ (mydata$material * mydata$minwidth + mydata$height * mydata$elongation))) # 2 
lm30ix   = summary(lm(mydata$volume ~ (mydata$material * mydata$elongation + mydata$minwidth * mydata$height))) # 2 
lm30x    = summary(lm(mydata$volume ~ (mydata$material * mydata$height * mydata$elongation + mydata$minwidth))) # 3 
lm30xi   = summary(lm(mydata$volume ~ (mydata$material + mydata$height * mydata$elongation * mydata$minwidth))) # 3 
lm30xii  = summary(lm(mydata$volume ~ (mydata$material * mydata$height + mydata$elongation + mydata$minwidth))) # 3 
lm30xiii = summary(lm(mydata$volume ~ (mydata$material * mydata$height + mydata$elongation + mydata$minwidth))) # 4 
# 5-Variable Base Models (5C5= 1)
lm31    = summary(lm(mydata$volume ~ (mydata$material + mydata$height + mydata$elongation + mydata$minwidth + mydata$maxwidth)))

# 5-Variable Interaction Models
lm31i     = summary(lm(mydata$volume ~ (mydata$material * mydata$height + mydata$elongation + mydata$minwidth + mydata$maxwidth))) # 1 
lm31ii    = summary(lm(mydata$volume ~ (mydata$material + mydata$height * mydata$elongation + mydata$minwidth + mydata$maxwidth))) # 1
lm31iii   = summary(lm(mydata$volume ~ (mydata$material + mydata$height + mydata$elongation * mydata$minwidth + mydata$maxwidth))) # 1 
lm31iv    = summary(lm(mydata$volume ~ (mydata$material + mydata$height + mydata$elongation + mydata$minwidth * mydata$maxwidth))) # 1 
lm31v     = summary(lm(mydata$volume ~ (mydata$material * mydata$elongation + mydata$minwidth + mydata$maxwidth + mydata$height))) # 1 
lm31vi    = summary(lm(mydata$volume ~ (mydata$material * mydata$minwidth + mydata$height + mydata$elongation + mydata$maxwidth))) # 1 
lm31vii   = summary(lm(mydata$volume ~ (mydata$material * mydata$maxwidth + mydata$height + mydata$elongation + mydata$minwidth))) # 1 
lm31viii  = summary(lm(mydata$volume ~ (mydata$height * mydata$minwidth + mydata$elongation + mydata$maxwidth + mydata$material))) # 1 
lm31ix    = summary(lm(mydata$volume ~ (mydata$height * mydata$maxwidth + mydata$minwidth + mydata$elongation + mydata$material))) # 1 
lm31x     = summary(lm(mydata$volume ~ (mydata$elongation * mydata$maxwidth + mydata$height + mydata$minwidth + mydata$material))) # 1 
lm31xi    = summary(lm(mydata$volume ~ (mydata$material * mydata$height + mydata$elongation * mydata$minwidth + mydata$maxwidth))) # 2
lm31xii   = summary(lm(mydata$volume ~ (mydata$material * mydata$height + mydata$elongation + mydata$minwidth * mydata$maxwidth))) # 2
lm31xiii  = summary(lm(mydata$volume ~ (mydata$material * mydata$height + mydata$elongation * mydata$maxwidth + mydata$minwidth))) # 2
lm31xiv   = summary(lm(mydata$volume ~ (mydata$material * mydata$elongation + mydata$height * mydata$minwidth + mydata$maxwidth))) # 2
lm31xv    = summary(lm(mydata$volume ~ (mydata$material * mydata$elongation + mydata$height * mydata$maxwidth + mydata$minwidth))) # 2
lm31xvi   = summary(lm(mydata$volume ~ (mydata$material * mydata$elongation + mydata$maxwidth * mydata$minwidth + mydata$height))) # 2
lm31xvii  = summary(lm(mydata$volume ~ (mydata$material * mydata$minwidth + mydata$elongation * mydata$height + mydata$maxwidth))) # 2
lm31xviii = summary(lm(mydata$volume ~ (mydata$material * mydata$minwidth + mydata$elongation * mydata$maxwidth + mydata$height))) # 2
lm31xix   = summary(lm(mydata$volume ~ (mydata$material * mydata$minwidth + mydata$height * mydata$maxwidth + mydata$elongation))) # 2
lm31xx    = summary(lm(mydata$volume ~ (mydata$material * mydata$maxwidth + mydata$elongation * mydata$height + mydata$minwidth))) # 2
lm31xxi   = summary(lm(mydata$volume ~ (mydata$material * mydata$maxwidth + mydata$elongation * mydata$minwidth + mydata$height))) # 2
lm31xxii  = summary(lm(mydata$volume ~ (mydata$material * mydata$maxwidth + mydata$minwidth * mydata$height + mydata$elongation))) # 2
lm31xxiii = summary(lm(mydata$volume ~ (mydata$height * mydata$elongation + mydata$maxwidth * mydata$minwidth + mydata$material))) # 2
lm31xxiv  = summary(lm(mydata$volume ~ (mydata$height * mydata$maxwidth + mydata$elongation + mydata$minwidth + mydata$material))) # 2
lm31xxv   = summary(lm(mydata$volume ~ (mydata$height * mydata$minwidth + mydata$maxwidth * mydata$elongation + mydata$material))) # 2

# ================================================ MODELS AND GRAPHS FOR ALL NON-INTERACTIVE TERM MODELS ======================================#
# Regurgitating Values
# r.squared

r2list = c(lm1[["r.squared"]],  lm2[["r.squared"]],  lm3[["r.squared"]],  lm4[["r.squared"]],  lm5[["r.squared"]],  lm6[["r.squared"]],  lm7[["r.squared"]],  lm8[["r.squared"]],  lm9[["r.squared"]],  lm10[["r.squared"]], 
              lm11[["r.squared"]], lm12[["r.squared"]], lm13[["r.squared"]], lm14[["r.squared"]], lm15[["r.squared"]], lm16[["r.squared"]], lm17[["r.squared"]], lm18[["r.squared"]], lm19[["r.squared"]], lm20[["r.squared"]],
              lm21[["r.squared"]], lm22[["r.squared"]], lm23[["r.squared"]], lm24[["r.squared"]], lm25[["r.squared"]], lm26[["r.squared"]], lm27[["r.squared"]], lm28[["r.squared"]], lm29[["r.squared"]], lm30[["r.squared"]], lm31[["r.squared"]])
r2list

# adj.r.squared
r2adjlist = c(lm1[["adj.r.squared"]],  lm2[["adj.r.squared"]],  lm3[["adj.r.squared"]],  lm4[["adj.r.squared"]],  lm5[["adj.r.squared"]],  lm6[["adj.r.squared"]],  lm7[["adj.r.squared"]],  lm8[["adj.r.squared"]],  lm9[["adj.r.squared"]],  lm10[["adj.r.squared"]], 
              lm11[["adj.r.squared"]], lm12[["adj.r.squared"]], lm13[["adj.r.squared"]], lm14[["adj.r.squared"]], lm15[["adj.r.squared"]], lm16[["adj.r.squared"]], lm17[["adj.r.squared"]], lm18[["adj.r.squared"]], lm19[["adj.r.squared"]], lm20[["adj.r.squared"]],
              lm21[["adj.r.squared"]], lm22[["adj.r.squared"]], lm23[["adj.r.squared"]], lm24[["adj.r.squared"]], lm25[["adj.r.squared"]], lm26[["adj.r.squared"]], lm27[["adj.r.squared"]], lm28[["adj.r.squared"]], lm29[["adj.r.squared"]], lm30[["adj.r.squared"]], lm31[["adj.r.squared"]])


r2adjlist

# S_e 
selist = c(lm1[["sigma"]],  lm2[["sigma"]],  lm3[["sigma"]],  lm4[["sigma"]],  lm5[["sigma"]],  lm6[["sigma"]],  lm7[["sigma"]],  lm8[["sigma"]],  lm9[["sigma"]],  lm10[["sigma"]], 
              lm11[["sigma"]], lm12[["sigma"]], lm13[["sigma"]], lm14[["sigma"]], lm15[["sigma"]], lm16[["sigma"]], lm17[["sigma"]], lm18[["sigma"]], lm19[["sigma"]], lm20[["sigma"]],
              lm21[["sigma"]], lm22[["sigma"]], lm23[["sigma"]], lm24[["sigma"]], lm25[["sigma"]], lm26[["sigma"]], lm27[["sigma"]], lm28[["sigma"]], lm29[["sigma"]], lm30[["sigma"]], lm31[["sigma"]])

selist

# Maybe some graphing?
graphingData = data.frame(lm=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                          se=selist, r2=r2list, r2adj=r2adjlist)

ggplot(graphingData, aes(x=graphingData$lm)) + 
  geom_line(aes(y=graphingData$r2adj), color="deepskyblue4", size=2) +
  #geom_line(aes(y=graphingData$r2), color="slateblue2") +
  labs(title = "Line Plot Comparing R-Squared-Adjusted to Linear Models with Increasing Determinants", 
       x = "Numerical Value of Linear Model (In general, larger x-value means more determinants) (See table on writeup)",
       y = "R-Squared-Adjusted Value for the Corresponding Linear Model") +
  geom_label(aes(label = "1 Determinant",x = 3.5, y = 0.92), fontface = "bold", fill="deepskyblue1") +
  geom_label(aes(label = "2 Determinants",x = 11, y = 0.92), fontface = "bold", fill="deepskyblue1") +
  geom_label(aes(label = "3 Determinants",x = 21, y = 0.92), fontface = "bold", fill="deepskyblue1") +
  geom_label(aes(label = "4 Determinants",x = 28.5, y = 0.92), fontface = "bold", fill="deepskyblue1") +
  geom_rect(aes(xmin=0.9, xmax=5.9, ymax=0.9, ymin=-0.01), alpha = 0.005, size = 0.5, color = "white") +
  geom_rect(aes(xmin=5.9, xmax=15.9, ymax=0.9, ymin=-0.01), alpha = 0.006, size = 0.5, color = "white") +
  geom_rect(aes(xmin=15.9, xmax=25.9, ymax=0.9, ymin=-0.01), alpha = 0.005, size = 0.5, color = "white") +
  geom_rect(aes(xmin=25.9, xmax=31.1, ymax=0.9, ymin=-0.01), alpha = 0.006, size = 0.5, color = "white") 
  
ggplot(graphingData, aes(x=graphingData$lm)) + 
  geom_line(aes(y=graphingData$se), color="deepskyblue4", size=2) +
  #geom_line(aes(y=graphingData$r2), color="slateblue2") +
  labs(title = "Line Plot Comparing Se to Linear Models with Increasing Determinants", 
       x = "Numerical Value of Linear Model (In general, larger x-value means more determinants) (See table on writeup)",
       y = "R-Squared-Adjusted Value for the Corresponding Linear Model") +
  geom_label(aes(label = "1 Determinant",x = 3.5, y = 496), fontface = "bold", fill="deepskyblue1") +
  geom_label(aes(label = "2 Determinants",x = 11, y = 496), fontface = "bold", fill="deepskyblue1") +
  geom_label(aes(label = "3 Determinants",x = 21, y = 496), fontface = "bold", fill="deepskyblue1") +
  geom_label(aes(label = "4 Determinants",x = 28.5, y = 496), fontface = "bold", fill="deepskyblue1") +
  geom_rect(aes(xmin=0.9, xmax=5.9, ymax=490, ymin=190), alpha = 0.005, size = 0.5, color = "white") +
  geom_rect(aes(xmin=5.9, xmax=15.9, ymax=490, ymin=190), alpha = 0.006, size = 0.5, color = "white") +
  geom_rect(aes(xmin=15.9, xmax=25.9, ymax=490, ymin=190), alpha = 0.005, size = 0.5, color = "white") +
  geom_rect(aes(xmin=25.9, xmax=31.1, ymax=490, ymin=190), alpha = 0.006, size = 0.5, color = "white") 

  
# ================================== MODELS AND GRAPHS FOR ALL INTERACTIVE AND NON-INTERACTIVE TERM MODELS ======================================#
# First, finding values
r2list = c(lm1[["r.squared"]],  lm2[["r.squared"]],  lm3[["r.squared"]],  lm4[["r.squared"]],  lm5[["r.squared"]],  lm6[["r.squared"]],  
           lm6i[["r.squared"]], lm7[["r.squared"]],  lm7i[["r.squared"]],  lm8[["r.squared"]],  lm8i[["r.squared"]], lm9i[["r.squared"]],  
           lm10[["r.squared"]], lm10i[["r.squared"]], lm11[["r.squared"]], lm11i[["r.squared"]], lm12[["r.squared"]], lm12i[["r.squared"]], 
           lm13[["r.squared"]], lm13i[["r.squared"]], lm14[["r.squared"]], lm14i[["r.squared"]], lm15[["r.squared"]], lm15i[["r.squared"]], 
           lm16[["r.squared"]], lm16i[["r.squared"]], lm16ii[["r.squared"]], lm16iii[["r.squared"]], lm16iiii[["r.squared"]], 
           lm17[["r.squared"]], lm17i[["r.squared"]], lm17ii[["r.squared"]], lm17iii[["r.squared"]], lm17iiii[["r.squared"]],
           lm18[["r.squared"]], lm18i[["r.squared"]], lm18ii[["r.squared"]], lm18iii[["r.squared"]], lm18iiii[["r.squared"]],
           lm19[["r.squared"]], lm19i[["r.squared"]], lm19ii[["r.squared"]], lm19iii[["r.squared"]], lm19iiii[["r.squared"]],
           lm20[["r.squared"]], lm20i[["r.squared"]], lm20ii[["r.squared"]], lm20iii[["r.squared"]], lm20iiii[["r.squared"]],
           lm21[["r.squared"]], lm21i[["r.squared"]], lm21ii[["r.squared"]], lm21iii[["r.squared"]], lm21iiii[["r.squared"]],
           lm22[["r.squared"]], lm22i[["r.squared"]], lm22ii[["r.squared"]], lm22iii[["r.squared"]], lm22iiii[["r.squared"]],
           lm23[["r.squared"]], lm23i[["r.squared"]], lm23ii[["r.squared"]], lm23iii[["r.squared"]], lm23iiii[["r.squared"]],
           lm24[["r.squared"]], lm24i[["r.squared"]], lm24ii[["r.squared"]], lm24iii[["r.squared"]], lm24iiii[["r.squared"]],  
           lm25[["r.squared"]], lm25i[["r.squared"]], lm25ii[["r.squared"]], lm25iii[["r.squared"]], lm25iiii[["r.squared"]], 
           lm26[["r.squared"]], lm26i[["r.squared"]], lm26ii[["r.squared"]], lm26iii[["r.squared"]], lm26iiii[["r.squared"]], 
           lm26v[["r.squared"]],lm26vi[["r.squared"]],lm26vii[["r.squared"]],lm26viii[["r.squared"]],lm26ix[["r.squared"]],   
           lm26x[["r.squared"]],lm26xi[["r.squared"]],lm26xii[["r.squared"]],lm26xiii[["r.squared"]],lm26xiiii[["r.squared"]],     
           lm27[["r.squared"]], lm27i[["r.squared"]], lm27ii[["r.squared"]], lm27iii[["r.squared"]], lm27iiii[["r.squared"]], 
           lm27v[["r.squared"]],lm27vi[["r.squared"]],lm27vii[["r.squared"]],lm27viii[["r.squared"]],lm27ix[["r.squared"]],   
           lm27x[["r.squared"]],lm27xi[["r.squared"]],lm27xii[["r.squared"]],lm27xiii[["r.squared"]],lm27xiiii[["r.squared"]],  
           lm28[["r.squared"]], lm28i[["r.squared"]], lm28ii[["r.squared"]], lm28iii[["r.squared"]], lm28iiii[["r.squared"]], 
           lm28v[["r.squared"]],lm28vi[["r.squared"]],lm28vii[["r.squared"]],lm28viii[["r.squared"]],lm28ix[["r.squared"]],   
           lm28x[["r.squared"]],lm28xi[["r.squared"]],lm28xii[["r.squared"]],lm28xiii[["r.squared"]],lm28xiiii[["r.squared"]],
           lm29[["r.squared"]], lm29i[["r.squared"]], lm29ii[["r.squared"]], lm29iii[["r.squared"]], lm29iiii[["r.squared"]], 
           lm29v[["r.squared"]],lm29vi[["r.squared"]],lm29vii[["r.squared"]],lm29viii[["r.squared"]],lm29ix[["r.squared"]],   
           lm29x[["r.squared"]],lm29xi[["r.squared"]],lm29xii[["r.squared"]],lm29xiii[["r.squared"]],lm29xiiii[["r.squared"]],  
           lm30[["r.squared"]], lm30i[["r.squared"]], lm30ii[["r.squared"]], lm30iii[["r.squared"]], lm30iiii[["r.squared"]], 
           lm30v[["r.squared"]],lm30vi[["r.squared"]],lm30vii[["r.squared"]],lm30viii[["r.squared"]],lm30ix[["r.squared"]],   
           lm30x[["r.squared"]],lm30xi[["r.squared"]],lm30xii[["r.squared"]],lm30xiii[["r.squared"]],
           lm31[["r.squared"]], lm31i[["r.squared"]], lm31ii[["r.squared"]], lm31iii[["r.squared"]], lm31iv[["r.squared"]], 
           lm31v[["r.squared"]],lm31vi[["r.squared"]],lm31vii[["r.squared"]],lm31viii[["r.squared"]],lm31ix[["r.squared"]],   
           lm31x[["r.squared"]],lm31xi[["r.squared"]],lm31xii[["r.squared"]],lm31xiii[["r.squared"]],lm31xiv[["r.squared"]], 
           lm31xv[["r.squared"]],lm31xvi[["r.squared"]],lm31xvii[["r.squared"]],lm31xviii[["r.squared"]],lm31xix[["r.squared"]],
           lm31xx[["r.squared"]],lm31xxi[["r.squared"]],lm31xxii[["r.squared"]],lm31xxiii[["r.squared"]], lm31xxiv[["r.squared"]],
           lm31xxv[["r.squared"]])
r2list

# adj.r.squared
r2adjlist = c(lm1[["adj.r.squared"]],  lm2[["adj.r.squared"]],  lm3[["adj.r.squared"]],  lm4[["adj.r.squared"]],  lm5[["adj.r.squared"]],  lm6[["adj.r.squared"]],  
              lm6i[["adj.r.squared"]], lm7[["adj.r.squared"]],  lm7i[["adj.r.squared"]],  lm8[["adj.r.squared"]],  lm8i[["adj.r.squared"]], lm9i[["adj.r.squared"]],  
              lm10[["adj.r.squared"]], lm10i[["adj.r.squared"]], lm11[["adj.r.squared"]], lm11i[["adj.r.squared"]], lm12[["adj.r.squared"]], lm12i[["adj.r.squared"]], 
              lm13[["adj.r.squared"]], lm13i[["adj.r.squared"]], lm14[["adj.r.squared"]], lm14i[["adj.r.squared"]], lm15[["adj.r.squared"]], lm15i[["adj.r.squared"]], 
              lm16[["adj.r.squared"]], lm16i[["adj.r.squared"]], lm16ii[["adj.r.squared"]], lm16iii[["adj.r.squared"]], lm16iiii[["adj.r.squared"]], 
              lm17[["adj.r.squared"]], lm17i[["adj.r.squared"]], lm17ii[["adj.r.squared"]], lm17iii[["adj.r.squared"]], lm17iiii[["adj.r.squared"]],
              lm18[["adj.r.squared"]], lm18i[["adj.r.squared"]], lm18ii[["adj.r.squared"]], lm18iii[["adj.r.squared"]], lm18iiii[["adj.r.squared"]],
              lm19[["adj.r.squared"]], lm19i[["adj.r.squared"]], lm19ii[["adj.r.squared"]], lm19iii[["adj.r.squared"]], lm19iiii[["adj.r.squared"]],
              lm20[["adj.r.squared"]], lm20i[["adj.r.squared"]], lm20ii[["adj.r.squared"]], lm20iii[["adj.r.squared"]], lm20iiii[["adj.r.squared"]],
              lm21[["adj.r.squared"]], lm21i[["adj.r.squared"]], lm21ii[["adj.r.squared"]], lm21iii[["adj.r.squared"]], lm21iiii[["adj.r.squared"]],
              lm22[["adj.r.squared"]], lm22i[["adj.r.squared"]], lm22ii[["adj.r.squared"]], lm22iii[["adj.r.squared"]], lm22iiii[["adj.r.squared"]],
              lm23[["adj.r.squared"]], lm23i[["adj.r.squared"]], lm23ii[["adj.r.squared"]], lm23iii[["adj.r.squared"]], lm23iiii[["adj.r.squared"]],
              lm24[["adj.r.squared"]], lm24i[["adj.r.squared"]], lm24ii[["adj.r.squared"]], lm24iii[["adj.r.squared"]], lm24iiii[["adj.r.squared"]],  
              lm25[["adj.r.squared"]], lm25i[["adj.r.squared"]], lm25ii[["adj.r.squared"]], lm25iii[["adj.r.squared"]], lm25iiii[["adj.r.squared"]], 
              lm26[["adj.r.squared"]], lm26i[["adj.r.squared"]], lm26ii[["adj.r.squared"]], lm26iii[["adj.r.squared"]], lm26iiii[["adj.r.squared"]], 
              lm26v[["adj.r.squared"]],lm26vi[["adj.r.squared"]],lm26vii[["adj.r.squared"]],lm26viii[["adj.r.squared"]],lm26ix[["adj.r.squared"]],   
              lm26x[["adj.r.squared"]],lm26xi[["adj.r.squared"]],lm26xii[["adj.r.squared"]],lm26xiii[["adj.r.squared"]],lm26xiiii[["adj.r.squared"]],     
              lm27[["adj.r.squared"]], lm27i[["adj.r.squared"]], lm27ii[["adj.r.squared"]], lm27iii[["adj.r.squared"]], lm27iiii[["adj.r.squared"]], 
              lm27v[["adj.r.squared"]],lm27vi[["adj.r.squared"]],lm27vii[["adj.r.squared"]],lm27viii[["adj.r.squared"]],lm27ix[["adj.r.squared"]],   
              lm27x[["adj.r.squared"]],lm27xi[["adj.r.squared"]],lm27xii[["adj.r.squared"]],lm27xiii[["adj.r.squared"]],lm27xiiii[["adj.r.squared"]],  
              lm28[["adj.r.squared"]], lm28i[["adj.r.squared"]], lm28ii[["adj.r.squared"]], lm28iii[["adj.r.squared"]], lm28iiii[["adj.r.squared"]], 
              lm28v[["adj.r.squared"]],lm28vi[["adj.r.squared"]],lm28vii[["adj.r.squared"]],lm28viii[["adj.r.squared"]],lm28ix[["adj.r.squared"]],   
              lm28x[["adj.r.squared"]],lm28xi[["adj.r.squared"]],lm28xii[["adj.r.squared"]],lm28xiii[["adj.r.squared"]],lm28xiiii[["adj.r.squared"]],
              lm29[["adj.r.squared"]], lm29i[["adj.r.squared"]], lm29ii[["adj.r.squared"]], lm29iii[["adj.r.squared"]], lm29iiii[["adj.r.squared"]], 
              lm29v[["adj.r.squared"]],lm29vi[["adj.r.squared"]],lm29vii[["adj.r.squared"]],lm29viii[["adj.r.squared"]],lm29ix[["adj.r.squared"]],   
              lm29x[["adj.r.squared"]],lm29xi[["adj.r.squared"]],lm29xii[["adj.r.squared"]],lm29xiii[["adj.r.squared"]],lm29xiiii[["adj.r.squared"]],  
              lm30[["adj.r.squared"]], lm30i[["adj.r.squared"]], lm30ii[["adj.r.squared"]], lm30iii[["adj.r.squared"]], lm30iiii[["adj.r.squared"]], 
              lm30v[["adj.r.squared"]],lm30vi[["adj.r.squared"]],lm30vii[["adj.r.squared"]],lm30viii[["adj.r.squared"]],lm30ix[["adj.r.squared"]],   
              lm30x[["adj.r.squared"]],lm30xi[["adj.r.squared"]],lm30xii[["adj.r.squared"]],lm30xiii[["adj.r.squared"]],
              lm31[["adj.r.squared"]], lm31i[["adj.r.squared"]], lm31ii[["adj.r.squared"]], lm31iii[["adj.r.squared"]], lm31iv[["adj.r.squared"]], 
              lm31v[["adj.r.squared"]],lm31vi[["adj.r.squared"]],lm31vii[["adj.r.squared"]],lm31viii[["adj.r.squared"]],lm31ix[["adj.r.squared"]],   
              lm31x[["adj.r.squared"]],lm31xi[["adj.r.squared"]],lm31xii[["adj.r.squared"]],lm31xiii[["adj.r.squared"]],lm31xiv[["adj.r.squared"]], 
              lm31xv[["adj.r.squared"]],lm31xvi[["adj.r.squared"]],lm31xvii[["adj.r.squared"]],lm31xviii[["adj.r.squared"]],lm31xix[["adj.r.squared"]],
              lm31xx[["adj.r.squared"]],lm31xxi[["adj.r.squared"]],lm31xxii[["adj.r.squared"]],lm31xxiii[["adj.r.squared"]], lm31xxiv[["adj.r.squared"]],
              lm31xxv[["adj.r.squared"]])
r2adjlist

# S_e 
selist = c(lm1[["sigma"]],  lm2[["sigma"]],  lm3[["sigma"]],  lm4[["sigma"]],  lm5[["sigma"]],  lm6[["sigma"]],  
           lm6i[["sigma"]], lm7[["sigma"]],  lm7i[["sigma"]],  lm8[["sigma"]],  lm8i[["sigma"]], lm9i[["sigma"]],  
           lm10[["sigma"]], lm10i[["sigma"]], lm11[["sigma"]], lm11i[["sigma"]], lm12[["sigma"]], lm12i[["sigma"]], 
           lm13[["sigma"]], lm13i[["sigma"]], lm14[["sigma"]], lm14i[["sigma"]], lm15[["sigma"]], lm15i[["sigma"]], 
           lm16[["sigma"]], lm16i[["sigma"]], lm16ii[["sigma"]], lm16iii[["sigma"]], lm16iiii[["sigma"]], 
           lm17[["sigma"]], lm17i[["sigma"]], lm17ii[["sigma"]], lm17iii[["sigma"]], lm17iiii[["sigma"]],
           lm18[["sigma"]], lm18i[["sigma"]], lm18ii[["sigma"]], lm18iii[["sigma"]], lm18iiii[["sigma"]],
           lm19[["sigma"]], lm19i[["sigma"]], lm19ii[["sigma"]], lm19iii[["sigma"]], lm19iiii[["sigma"]],
           lm20[["sigma"]], lm20i[["sigma"]], lm20ii[["sigma"]], lm20iii[["sigma"]], lm20iiii[["sigma"]],
           lm21[["sigma"]], lm21i[["sigma"]], lm21ii[["sigma"]], lm21iii[["sigma"]], lm21iiii[["sigma"]],
           lm22[["sigma"]], lm22i[["sigma"]], lm22ii[["sigma"]], lm22iii[["sigma"]], lm22iiii[["sigma"]],
           lm23[["sigma"]], lm23i[["sigma"]], lm23ii[["sigma"]], lm23iii[["sigma"]], lm23iiii[["sigma"]],
           lm24[["sigma"]], lm24i[["sigma"]], lm24ii[["sigma"]], lm24iii[["sigma"]], lm24iiii[["sigma"]],  
           lm25[["sigma"]], lm25i[["sigma"]], lm25ii[["sigma"]], lm25iii[["sigma"]], lm25iiii[["sigma"]], 
           lm26[["sigma"]], lm26i[["sigma"]], lm26ii[["sigma"]], lm26iii[["sigma"]], lm26iiii[["sigma"]], 
           lm26v[["sigma"]],lm26vi[["sigma"]],lm26vii[["sigma"]],lm26viii[["sigma"]],lm26ix[["sigma"]],   
           lm26x[["sigma"]],lm26xi[["sigma"]],lm26xii[["sigma"]],lm26xiii[["sigma"]],lm26xiiii[["sigma"]],     
           lm27[["sigma"]], lm27i[["sigma"]], lm27ii[["sigma"]], lm27iii[["sigma"]], lm27iiii[["sigma"]], 
           lm27v[["sigma"]],lm27vi[["sigma"]],lm27vii[["sigma"]],lm27viii[["sigma"]],lm27ix[["sigma"]],   
           lm27x[["sigma"]],lm27xi[["sigma"]],lm27xii[["sigma"]],lm27xiii[["sigma"]],lm27xiiii[["sigma"]],  
           lm28[["sigma"]], lm28i[["sigma"]], lm28ii[["sigma"]], lm28iii[["sigma"]], lm28iiii[["sigma"]], 
           lm28v[["sigma"]],lm28vi[["sigma"]],lm28vii[["sigma"]],lm28viii[["sigma"]],lm28ix[["sigma"]],   
           lm28x[["sigma"]],lm28xi[["sigma"]],lm28xii[["sigma"]],lm28xiii[["sigma"]],lm28xiiii[["sigma"]],
           lm29[["sigma"]], lm29i[["sigma"]], lm29ii[["sigma"]], lm29iii[["sigma"]], lm29iiii[["sigma"]], 
           lm29v[["sigma"]],lm29vi[["sigma"]],lm29vii[["sigma"]],lm29viii[["sigma"]],lm29ix[["sigma"]],   
           lm29x[["sigma"]],lm29xi[["sigma"]],lm29xii[["sigma"]],lm29xiii[["sigma"]],lm29xiiii[["sigma"]],  
           lm30[["sigma"]], lm30i[["sigma"]], lm30ii[["sigma"]], lm30iii[["sigma"]], lm30iiii[["sigma"]], 
           lm30v[["sigma"]],lm30vi[["sigma"]],lm30vii[["sigma"]],lm30viii[["sigma"]],lm30ix[["sigma"]],   
           lm30x[["sigma"]],lm30xi[["sigma"]],lm30xii[["sigma"]],lm30xiii[["sigma"]],
           lm31[["sigma"]], lm31i[["sigma"]], lm31ii[["sigma"]], lm31iii[["sigma"]], lm31iv[["sigma"]], 
           lm31v[["sigma"]],lm31vi[["sigma"]],lm31vii[["sigma"]],lm31viii[["sigma"]],lm31ix[["sigma"]],   
           lm31x[["sigma"]],lm31xi[["sigma"]],lm31xii[["sigma"]],lm31xiii[["sigma"]],lm31xiv[["sigma"]], 
           lm31xv[["sigma"]],lm31xvi[["sigma"]],lm31xvii[["sigma"]],lm31xviii[["sigma"]],lm31xix[["sigma"]],
           lm31xx[["sigma"]],lm31xxi[["sigma"]],lm31xxii[["sigma"]],lm31xxiii[["sigma"]], lm31xxiv[["sigma"]],
           lm31xxv[["sigma"]]) #174
selist

mean(selist)
mean(r2adjlist)

graphingData = data.frame(lm=seq(1, 174),
                          se=selist, r2=r2list, r2adj=r2adjlist)

ggplot(graphingData, aes(x=graphingData$lm)) + 
  geom_line(aes(y=graphingData$r2adj), color="deepskyblue4", size=1) +
  #geom_line(aes(y=graphingData$r2), color="slateblue2") +
  labs(title = "Line Plot Comparing R-Squared-Adjusted to Linear Models with Increasing Determinants (Interaction and no Interaction Terms)", 
       x = "Numerical Value of Linear Model (In general, larger x-value means more determinants) (See table on writeup)",
       y = "R-Squared-Adjusted Value for the Corresponding Linear Model") +
  #geom_label(aes(label = "1 Determinant",x = 3.5, y = 0.92), fontface = "bold", fill="deepskyblue1") +
  geom_label(aes(label = "2 Determinants",x = 15.1, y = 1.01), fontface = "bold", fill="deepskyblue1") +
  geom_label(aes(label = "3 Determinants",x = 50, y = 1.01), fontface = "bold", fill="deepskyblue1") +
  geom_label(aes(label = "4 Determinants",x = 110, y = 1.01), fontface = "bold", fill="deepskyblue1") +
  geom_label(aes(label = "5 Determinants",x = 161, y = 1.01), fontface = "bold", fill="deepskyblue1") +
  geom_rect(aes(xmin=0.9, xmax=5.9, ymax=0.99, ymin=0.01), alpha = 0.003, fill = "cyan") +
  geom_rect(aes(xmin=6.1, xmax=23.9, ymax=0.99, ymin=0.01), alpha = 0.003,  fill = "darkseagreen1") +
  geom_rect(aes(xmin=24.1, xmax=73.9, ymax=0.99, ymin=0.5), alpha = 0.003,  fill = "cyan") +
  geom_rect(aes(xmin=74.1, xmax=147.9, ymax=0.99, ymin=0.5), alpha = 0.003,  fill = "darkseagreen1") +
  geom_rect(aes(xmin=148.1, xmax=174, ymax=0.99, ymin=0.5), alpha = 0.003,  fill = "cyan") 





ggplot(graphingData, aes(x=graphingData$lm)) + 
  geom_line(aes(y=graphingData$se), color="deepskyblue4", size=1) +
  #geom_line(aes(y=graphingData$r2), color="slateblue2") +
  labs(title = "Line Plot Comparing Se to Linear Models with Increasing Determinants (Interaction and no Interaction Terms)", 
       x = "Numerical Value of Linear Model (In general, larger x-value means more determinants) (See table on writeup)",
       y = "Se for the Corresponding Linear Model") +
  geom_label(aes(label = "2 Determinants",x = 15.1, y = 495), fontface = "bold", fill="deepskyblue1") +
  geom_label(aes(label = "3 Determinants",x = 50, y = 495), fontface = "bold", fill="deepskyblue1") +
  geom_label(aes(label = "4 Determinants",x = 110, y = 495), fontface = "bold", fill="deepskyblue1") +
  geom_label(aes(label = "5 Determinants",x = 161, y = 495), fontface = "bold", fill="deepskyblue1") +
  geom_rect(aes(xmin=0.7, xmax=5.9, ymax=485, ymin=300), alpha = 0.003, fill = "cyan") +
  geom_rect(aes(xmin=6.1, xmax=23.9, ymax=485, ymin=170), alpha = 0.003, fill = "darkseagreen1") +
  geom_rect(aes(xmin=24.1, xmax=73.9, ymax=485, ymin=100), alpha = 0.003, fill = "cyan") +
  geom_rect(aes(xmin=74.1, xmax=147.9, ymax=485, ymin=40), alpha = 0.003, fill = "darkseagreen1") +
  geom_rect(aes(xmin=148.1, xmax=174.1, ymax=485, ymin=40), alpha = 0.003, fill = "cyan") 


# ========================== CREATING GRAPHS FOR F* / MODEL UTILITY TEST ==================================
lm20ii  = lm(mydata$volume ~ (mydata$elongation + mydata$height * mydata$minwidth))
haty = -(lm20ii$residuals - mydata$volume)

# Checks for F-Test
# Normal Probability Plot
qqnorm(lm20ii$residuals, main= "Normal Probability Plot of Std. Residuals for Food Product Volume", 
       xlab = "Predicted Z-Scores", ylab = "Standardised Residuals")



# Residual plot, 'E' Check
ggplot(lm20ii, aes(y=lm20ii$residuals, x=haty)) +
  geom_point(size = 2, color = "cornflowerblue", ) +
  labs(title = "Scatter Plot Illustrating the Predicted Y-Values vs. Calculated Residuals for Food Container Volume", 
       x = "Predicted Y-Values",
       y = "Calculated Residuals")




