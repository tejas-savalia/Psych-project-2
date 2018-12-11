rm(list = ls())

#Read data into relevant variables
data = read.csv(file = file.choose())
distance = data['x11']
gender = data['x7']
#usage = data['x10']

recommend = data['x8']
#satisfaction = data['x9']

#####################################################
female_low_recommend = recommend[gender[[1]] == 0 & distance[[1]] == 1, 1]
female_mid_recommend = recommend[gender[[1]] == 0 & distance[[1]] == 2, 1]
female_high_recommend = recommend[gender[[1]] == 0 & distance[[1]] == 3, 1]
#female_low_satisfy = satisfaction[gender[[1]] == 0 & distance[[1]] == 1, 1]
#female_mid_satisfy = satisfaction[gender[[1]] == 0 & distance[[1]] == 2, 1]
#female_high_satisfy = satisfaction[gender[[1]] == 0 & distance[[1]] == 3, 1]


male_low_recommend = recommend[gender[[1]] == 1 & distance[[1]] == 1, 1]
male_mid_recommend = recommend[gender[[1]] == 1 & distance[[1]] == 2, 1]
male_high_recommend = recommend[gender[[1]] == 1 & distance[[1]] == 3, 1]
#male_low_satisfy = satisfaction[gender[[1]] == 1 & distance[[1]] == 1, 1]
#male_mid_satisfy = satisfaction[gender[[1]] == 1 & distance[[1]] == 2, 1]
#male_high_satisfy = satisfaction[gender[[1]] == 1 & distance[[1]] == 3, 1]

#Testing for normality
par(mfrow=c(2, 3), row.names(c("males", "females")), colnames(c('low, mid, high')))
qqnorm(male_low_recommend, main = "Recommendations", col = 'blue')
qqnorm(male_mid_recommend, main = "Recommendations", col = 'blue')
qqnorm(male_high_recommend, main = "Recommendations", col = 'blue')
qqnorm(female_low_recommend, main = "Recommendations", col = 'blue')
qqnorm(female_mid_recommend, main = "Recommendations", col = 'blue')
qqnorm(female_high_recommend, main = "Recommendations", col = 'blue')

hist(male_low_recommend, main = "Recommendations", breaks = seq(1, 7, 1), col = 'blue')
hist(male_mid_recommend, main = "Recommendations", breaks = seq(1, 7, 1), col = 'blue')
hist(male_high_recommend, main = "Recommendations", breaks = seq(1, 7, 1), col = 'blue')
hist(female_low_recommend, main = "Recommendations", breaks = seq(1, 7, 1), col = 'blue')
hist(female_mid_recommend, main = "Recommendations", breaks = seq(1, 7, 1), col = 'blue')
hist(female_high_recommend, main = "Recommendations", breaks = seq(1, 7, 1), col = 'blue')

#hist(recommend[[1]], main = "Recommendations", col = "blue", breaks = seq(1, 7, 1))
#barplot(table(recommend[[1]]), ylab = "Frequencies", main = "Recommendations", xlab = "Scores", col = "blue")
library('e1071')

skewness(recommend[[1]])
#0.145



#Shapiro test for male_high_recommend throws up an error because all entries in that
#group are identical. I guess, we can say it's heavily not-normal.

shapiro.test(male_mid_recommend)

#Shapiro-Wilk normality test

#data:  male_mid_recommend
#W = 0.42208, p-value = 4.195e-08
shapiro.test(male_low_recommend)
#Shapiro-Wilk normality test

#data:  male_low_recommend
#W = 0.41679, p-value = 1.109e-12
shapiro.test(female_high_recommend)
#Shapiro-Wilk normality test

#data:  female_high_recommend
#W = 0.63036, p-value = 6.75e-07
shapiro.test(female_mid_recommend)
#Shapiro-Wilk normality test

#data:  female_mid_recommend
#W = 0.86774, p-value = 3.548e-05

shapiro.test(female_low_recommend)
#Shapiro-Wilk normality test

#data:  female_low_recommend
#W = 0.80629, p-value = 3.399e-05

#######################################################
#Test for Homogeniety of varience

library(car)
leveneTest(recommend[[1]], group = as.factor(gender[[1]]))

#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value    Pr(>F)    
#group   1  11.727 0.0007486 ***
#       198                      
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

leveneTest(recommend[[1]], group = as.factor(distance[[1]]))
#Levenes Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
#group   2  0.0946 0.9098
#       183               




######################################################################################

gender_fac <- as.factor(gender[[1]])
dist_fac <- as.factor(distance[[1]])

sub <- as.factor(1:200)
d <- data.frame(recommend = recommend[[1]], gender_fac = gender_fac, sub = sub, dist_fac = dist_fac)
aov.mult <- aov(recommend ~ gender_fac*dist_fac, data = d)
Method <- as.factor(gender_fac*dist_fac)
summary(aov.mult)

#                       Df Sum Sq Mean Sq F value   Pr(>F)    
#  gender_fac            1  21.75  21.747  39.480 2.43e-09 ***
#  dist_fac              2  53.03  26.516  48.138  < 2e-16 ***
#  gender_fac:dist_fac   2   2.72   1.359   2.467   0.0877 .  
#   Residuals           180  99.15   0.551                     
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#  14 observations deleted due to missingness



library(multcomp)

tukey_gender = TukeyHSD(aov.mult, "gender_fac")
#             diff        lwr        upr       p adj
#   1-0   -0.6986004 -0.9179912 -0.4792096 2.42611e-09

tukey_dist = TukeyHSD(aov.mult, "dist_fac")
#           diff         lwr       upr        p adj
#2-1      0.8981295  0.61669134 1.1795677 6.551870e-12
#3-1      1.1807200  0.80707361 1.5543664 1.009537e-11
#3-2      0.2825904 -0.09778965 0.6629705 1.876334e-01

