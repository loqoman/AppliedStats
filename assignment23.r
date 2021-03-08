# Darwin Clark
# 2020.01.20
# HAppliedStats/Assignment23.r

# Libraries
library(ggplot2)




# Setting up Fish Data

fishData = data.frame(ChosenBodLen=c(24,16,24,23,24,22,19,24,24,21,28,23,18,25,18,17.5,23,16,24,27,23,23,15,27,29,19,25,26), 
                      ChosenDuration=c(1.58,1.12,1.69,1.78,1.46,1.76,1.44,1.36,1.55,1.86,1.99,1.6,1.2,1.74,1.85,1.63,1.49,2.39,1.6,1.61,1.66,1.58,1.46,2.06,2.03,1.28,1.89,1.82), 
                      ChosenAmplitude=c(5.5,2.55,5.02,6.03,4.68,6.9,6.45,6.45,6.44,5.44,5.5,6.87,3.36,7.65,4.31,4.59,7.18,2.96,7.28,7.26,6.9,7.73,1.81,6.22,5.94,3.83,4.86,8.02), 
                      RejectedBodLen=c(20,21,19.5,16,19,17,25,16,20,15,21,17,26,18,25,24.5,18,24,18,19,17,18,26,15,23,26,18,20), 
                      RejectedDuration=c(1.56,1.68,1.68,1.65,1.19,1.19,1.18,1.56,1.38,1.29,1.55,1.55,2.12,1.59,2.01,1.71,1.34,1.7,1.73,1.63,1.46,1.51,1.58,1.09,1.8,1.51,1.37,1.67), 
                      RejectedAmplitude=c(4.79,3.51,8.37,2.96,4.14,3.3,6.07,3.22,4.25,3.34,4.31,3.38,7.01,4.04,9.14,7.42,5.63,5.56,4.01,4.29,3.61,4.82,7.35,2.31,6.76,8.84,4.59,3.64))

# ######
# Construct and describe graphical displays of your choice which easily compares the
# distributions of the differences in body length, waveform duration, and waveform
# amplitude of the chosen vs. rejected males. What differences, if any, do you see in these
# distributions? You should end up with three different comparisons, or six separate
# graphs. Please include titles, labels, and colors. When discussing differences in
# distributions, make sure you state in context differences in center, shape and spread of
# the distributions using comparative language. Make sure you are using calculated
# statistics in your contextual descriptions.
# ######

# ### Numerical Analysis of chosen vs. rejected

# Bod Length
summary(fishData$ChosenBodLen)
summary(fishData$RejectedBodLen)

sd(fishData$ChosenBodLen)
sd(fishData$RejectedBodLen)
# Amp
summary(fishData$ChosenAmplitude)
summary(fishData$RejectedAmplitude)

sd(fishData$ChosenAmplitude)
sd(fishData$RejectedAmplitude)
# Duration
summary(fishData$ChosenDuration)
summary(fishData$RejectedDuration)

sd(fishData$ChosenDuration)
sd(fishData$RejectedDuration)
# ### Body Length ###
# Bar chart of chosen body length
ggplot(fishData, aes(x = fishData$ChosenBodLen)) +
  geom_boxplot(color = "darkcyan") +
  #geom_point() +
  xlim(5, 40) +
  ylim(-.8, .8) + 
  xlab("Chosen Body Length (cm)") + ylab("") + labs(title = "Box Plot Comparing Distribtuion of Chosen Body Length (cm)")
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5))  # Vertical x labels 

# Bar chart of rejected body length
ggplot(fishData, aes(x = fishData$RejectedBodLen)) +
  geom_boxplot(color = "darkcyan") +
  #geom_point() +
  xlim(5, 40) +
  ylim(-.8, .8) + 
  xlab("Rejected Body Length (cm)") + ylab("") + labs(title = "Box Plot Comparing Distribtuion of Rejected Body Length (cm)")
#theme(axis.text.x = element_text(angle = 45, vjust = 0.5))  # Vertical x labels 

# ### Amplitude ###
# Chosen Amp
ggplot(fishData, aes(x = fishData$ChosenAmplitude)) +
  geom_boxplot(color = "darkcyan") +
  #geom_point() +
  xlim(0, 10) +
  ylim(-.5, .5) + 
  xlab("Chosen EOD Amplitude (mV)") + ylab("") + labs(title = "Box Plot Comparing Distribtuion of Chosen EOD Amplitude (mV)")

# Rejected Amp
ggplot(fishData, aes(x = fishData$RejectedAmplitude)) +
  geom_boxplot(color = "darkcyan") +
  #geom_point() +
  xlim(0, 10) +
  ylim(-.5, .5) + 
  xlab("Rejected EOD Amplitude (mV)") + ylab("") + labs(title = "Box Plot Comparing Distribtuion of Rejected EOD Amplitude (mV)")

# ### Duration ###
# Chosen Duration
ggplot(fishData, aes(x = fishData$ChosenDuration)) +
  geom_boxplot(color = "darkcyan") +
  #geom_point() +
  xlim(.6, 2.7) +
  ylim(-.5, .5) + 
  xlab("Chosen EOD Duration (ms)") + ylab("") + labs(title = "Box Plot Comparing Distribtuion of Chosen EOD Duration (ms)")

# Rejected Duration
ggplot(fishData, aes(x = fishData$RejectedDuration)) +
  geom_boxplot(color = "darkcyan") +
  #geom_point() +
  xlim(.6, 2.7) +
  ylim(-.5, .5) + 
  xlab("Rejected EOD Duration (ms)") + ylab("") + labs(title = "Box Plot Comparing Distribtuion of Rejected EOD Duration (ms)")

# ###
# It is also possible that the female may be considering combinations of characteristics.
# For example, some combination of body length and EOD duration may be particularly
# attractive to the female. We are NOT going to run a multiple regression analysis (this
# time), but rather for each pair of characteristics (body length vs EOD duration, body
# length vs EOD amplitude, and EOD duration vs EOD amplitude) compare the chosen and rejected males using 
# scatterplots of these pairs of variables as well as regression
# techniques. Include summary statistics of your linear regression models. (This means you will have three comparisons and six scatterplots/regression models.)
# ###
# 
# Combination (AMP and Duration)
ggplot(fishData) +
  geom_point(color = "darkcyan", aes(x = fishData$ChosenDuration, y = fishData$ChosenAmplitude)) +
  geom_point(color = "red", aes(x = fishData$RejectedDuration, y = fishData$RejectedAmplitude)) +
  
  #geom_point() +
  xlim(1,2.5) +
  ylim(1,10) + 
  xlab("Duration of EOD (ms)") + ylab("Amplitude of EOD (mV)") + labs(title = "Comparing Rejected (red) and Chosen fish (blue) EOD Amplitude and Duration")

a = lm(fishData$ChosenAmplitude~fishData$ChosenDuration)
aa = lm(fishData$ChosenDuration~fishData$ChosenAmplitude)

b = lm(fishData$RejectedAmplitude~fishData$RejectedDuration)
bb = lm(fishData$RejectedDuration~fishData$RejectedAmplitude)


ggplot(fishData) +
  geom_point(color = "darkcyan", aes(x = fishData$ChosenBodLen, y = fishData$ChosenDuration)) +
  geom_point(color = "red", aes(x = fishData$RejectedBodLen, y = fishData$RejectedDuration)) +
  
  #geom_point() +
  xlim(14, 30) +
  ylim(1, 2.5) + 
  xlab("Fish Body Length (cm)") + ylab("Duration of EOD (ms)") + labs(title = "Comparing Rejected (red) and Chosen fish (blue) EOD Duration and Body Length")

a = lm(fishData$ChosenBodLen~fishData$ChosenDuration)
aa = lm(fishData$ChosenDuration~fishData$ChosenBodLen)

b = lm(fishData$RejectedBodLen~fishData$RejectedDuration)
bb = lm(fishData$RejectedDuration~fishData$RejectedBodLen)

ggplot(fishData) +
  geom_point(color = "darkcyan", aes(x = fishData$ChosenBodLen, y = fishData$ChosenAmplitude)) +
  geom_point(color = "red", aes(x = fishData$RejectedBodLen, y = fishData$RejectedAmplitude)) +
  
  #geom_point() +
  xlim(14, 30) +
  ylim(1,10) + 
  xlab("Fish Body Length (cm)") + ylab("Amplitude of EOD (mV)") + labs(title = "Comparing Rejected (red) and Chosen fish (blue) EOD Amplitude and Body Length")

a = lm(fishData$ChosenAmplitude~fishData$ChosenBodLen)
aa = lm(fishData$ChosenBodLen~fishData$ChosenAmplitude)

b = lm(fishData$RejectedAmplitude~fishData$RejectedBodLen)
bb = lm(fishData$RejectedBodLen~fishData$RejectedAmplitude)

