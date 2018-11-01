rm(list = ls())

#Read data into relevant variables
data = read.csv(file = file.choose())
distance = data['x11']
gender = data['x7']
usage = data['x10']

recommend = data['x8']
satisfaction = data['x9']

#Null Hypothesis 1. No difference in recommendation across gender. Alternate would be 
#there *is* a significant difference in recommendation across gender
male_rec = recommend[gender == 1] #Will call this group 1
female_rec = recommend[gender == 0] #Will call this group 2

#Independent groups design. 
mean1 = mean(male_rec)
sd1 = sd(male_rec)
n1 = length(male_rec)
mean2 = mean(female_rec)
sd2 = sd(female_rec)
n2 = length(female_rec)
s_pooled = sqrt(((n1-1)/(n1+n2-2))*sd1^2 + ((n2-1)/(n1+n2-2))*sd2^2)
mean_diff = mean1 - mean2
t_stat = mean_diff/((s_pooled)* sqrt((1/n1) + (1/n2)) )
(s_pooled)* sqrt((1/n1) + (1/n2))

pt(t_stat, df = 198)

effect_size = mean_diff/s_pooled
t.test(male_rec, female_rec)

#A priori power. To get power 0.8; we need 

#Null Hyp 2. No difference across usage
high_use = recommend[usage == 1]
low_use = recommend[usage == 0]

t.test(high_use, low_use)

# Null 3. No difference across distance
far = recommend[distance == 1]
near = recommend[distance == 2]
close = recommend[distance == 3]

t.test(far, close)
t.test(far, near)
t.test(near, close)
