rm(list = ls())

#Read data into relevant variables
data = read.csv(file = file.choose())
distance = data['x11']
gender = data['x7']
#usage = data['x10']

recommend = data['x8']
satisfaction = data['x9']

#####################################################
female_low_recommend = recommend[gender[[1]] == 0 & distance[[1]] == 1, 1]
female_mid_recommend = recommend[gender[[1]] == 0 & distance[[1]] == 2, 1]
female_high_recommend = recommend[gender[[1]] == 0 & distance[[1]] == 3, 1]
female_low_satisfy = satisfaction[gender[[1]] == 0 & distance[[1]] == 1, 1]
female_mid_satisfy = satisfaction[gender[[1]] == 0 & distance[[1]] == 2, 1]
female_high_satisfy = satisfaction[gender[[1]] == 0 & distance[[1]] == 3, 1]


male_low_recommend = recommend[gender[[1]] == 1 & distance[[1]] == 1, 1]
male_mid_recommend = recommend[gender[[1]] == 1 & distance[[1]] == 2, 1]
male_high_recommend = recommend[gender[[1]] == 1 & distance[[1]] == 3, 1]
male_low_satisfy = satisfaction[gender[[1]] == 1 & distance[[1]] == 1, 1]
male_mid_satisfy = satisfaction[gender[[1]] == 1 & distance[[1]] == 2, 1]
male_high_satisfy = satisfaction[gender[[1]] == 1 & distance[[1]] == 3, 1]

#Testing for normality
par(mfrow=c(2, 3), row.names(c("males", "females")), colnames(c('low, mid, high')))
qqnorm(male_low_recommend, main = "Recommendations", col = 'blue')
qqnorm(male_high_recommend, main = "Recommendations", col = 'blue')
qqnorm(male_high_recommend, main = "Recommendations", col = 'blue')
qqnorm(female_low_recommend, main = "Recommendations", col = 'blue')
qqnorm(female_mid_recommend, main = "Recommendations", col = 'blue')
qqnorm(female_high_recommend, main = "Recommendations", col = 'blue')

hist(male_low_recommend, main = "Recommendations", breaks = seq(1, 7, 1), col = 'blue')
hist(male_high_recommend, main = "Recommendations", breaks = seq(1, 7, 1), col = 'blue')
hist(male_high_recommend, main = "Recommendations", breaks = seq(1, 7, 1), col = 'blue')
hist(female_low_recommend, main = "Recommendations", breaks = seq(1, 7, 1), col = 'blue')
hist(female_mid_recommend, main = "Recommendations", breaks = seq(1, 7, 1), col = 'blue')
hist(female_high_recommend, main = "Recommendations", breaks = seq(1, 7, 1), col = 'blue')

hist(recommend[[1]], main = "Recommendations", col = "blue", breaks = seq(1, 7, 1))
#barplot(table(recommend[[1]]), ylab = "Frequencies", main = "Recommendations", xlab = "Scores", col = "blue")
library('e1071')

skewness(recommend[[1]])
#0.145


par(mfrow=c(1, 2))
qqnorm(satisfaction[[1]], main = "Satisfactions", col = 'red')
hist(satisfaction[[1]], main = "Satisfactions", col = "red", breaks = seq(1, 7, 1))
#barplot(table(satisfaction[[1]]), ylab = "Frequencies", xlab = "Scores", col = "red", main = "Satisfactions")
#barplot(satisfaction[[1]], ylab = "Frequencies", xlab = "Scores", col = "red", main = "Satisfactions")
skewness(satisfaction[[1]])
#0.023


shapiro.test(satisfaction[[1]])

#Shapiro-Wilk normality test

#data:  satisfaction[[1]]
#W = 0.89711, p-value = 1.652e-10

shapiro.test(recommend[[1]])

#Shapiro-Wilk normality test

#data:  recommend[[1]]
#W = 0.89681, p-value = 1.581e-10


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


leveneTest(satisfaction[[1]], group = as.factor(gender[[1]]))
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value  Pr(>F)  
#group   1  5.2572 0.02291 *
#       198                  
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


leveneTest(satisfaction[[1]], group = as.factor(distance[[1]]))
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value  Pr(>F)  
#group   2  2.6119 0.07613 .
#183                  
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1




###########################################################
#Apriori power analysis
#used gpower
#Need 42 samples in each group for an effect size of 0.8 to get a power of 0.95

###########################################################
#Null Hypothesis 1. No difference in recommendation across gender. Alternate would be 
#there *is* a significant difference in recommendation across gender

##############################
#T test for recommendations 
#Across Gender

male_rec = recommend[gender == 1] #Will call this group 1
female_rec = recommend[gender == 0] #Will call this group 2



#Independent groups design. 


#mean1 = mean(male_rec)
#sd1 = sd(male_rec)
#n1 = length(male_rec)
#mean2 = mean(female_rec)
#sd2 = sd(female_rec)
##n2 = length(female_rec)
#s_pooled = sqrt(((n1-1)/(n1+n2-2))*sd1^2 + ((n2-1)/(n1+n2-2))*sd2^2)
#mean_diff = mean1 - mean2
#t_stat = mean_diff/((s_pooled)* sqrt((1/n1) + (1/n2)) )
#(s_pooled)* sqrt((1/n1) + (1/n2))

#pt(t_stat, df = 198)

#effect_size = mean_diff/s_pooled

t.test(male_rec, female_rec)
#t = -5.882
#df = 197.77,
#p = 1.7e-08
#95% CI -0.956, -0.476
#Females are more likely to recommend

#Across Distance

lt1 = recommend[distance == 1]
bt15 = recommend[distance == 2]
gt5 = recommend[distance == 3]

t.test(lt1, bt15)
#t = -8.7343, df = 152.97, p-value = 4.005e-15
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.3605652 -0.8586129
#sample estimates:
#  mean of x mean of y 
#4.000000  5.109589 

t.test(bt15, gt5)
#data:  bt15 and gt5
#t = -3.0098, df = 80.939, p-value = 0.003485
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.6485006 -0.1323213
#sample estimates:
#  mean of x mean of y 
#5.109589  5.500000 


t.test(gt5, lt1)
#data:  gt5 and lt1
#t = 11.658, df = 82.293, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  1.244057 1.755943
#sample estimates:
#  mean of x mean of y 
#5.5       4.0 

#Farther you are, more likely you are to recommend

##############################
#T tests for satisfactions
#Across gender

male_satsifaction = satisfaction[gender == 1]
female_satisfaction = satisfaction[gender == 0]

t.test(male_satsifaction, female_satisfaction)
#data:  male_satsifaction and female_satisfaction
#t = -5.3401, df = 137.94, p-value = 3.731e-07
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.9820319 -0.4513014
#sample estimates:
#  mean of x mean of y 
#4.350000  5.066667 

#Across distance
lt1_satisfaction = satisfaction[distance == 1]
bt15_satisfaction = satisfaction[distance == 2]
gt5_satisfaction = satisfaction[distance == 3]

t.test(lt1_satisfaction, bt15_satisfaction)
#data:  lt1_satisfaction and bt15_satisfaction
#t = -17.267, df = 150.07, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.435577 -1.140756
#sample estimates:
#  mean of x mean of y 
#3.903614  5.191781 

t.test(bt15_satisfaction, gt5_satisfaction)
#data:  bt15_satisfaction and gt5_satisfaction
#t = -12.527, df = 69.682, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.0528273 -0.7636111
#sample estimates:
#  mean of x mean of y 
#5.191781  6.100000 

t.test(gt5_satisfaction, lt1_satisfaction)
#data:  gt5_satisfaction and lt1_satisfaction
#t = 27.208, df = 89.554, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  2.036001 2.356770
#sample estimates:
#  mean of x mean of y 
#6.100000  3.903614 

############################################################




