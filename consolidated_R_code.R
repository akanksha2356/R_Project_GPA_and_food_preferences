data <-  read_csv("~/.Business Analytics and Project management/OPIM-5603_Statistics in Business Analytics/group project/food_coded.csv")

nrow(data)

#data cleaning to delete garbage values in GPA and Weight Column

data$GPA1 <- as.numeric(as.character(data$GPA))
data$weight1 <- as.numeric(as.character(data$weight))

# Discarding the columns with only texts and a few others which are irrelevant to our analyses
data1 <- data[,c(2,3,5,7,10,11,12,13,15,16,18,19,20,21,22,23,24,27,28,30,31,32,33,34,37,38,39,40,41,42,44,46,47,48,49,50,51,52,53,54,58,59,62,63)]

# Method 1- Dropping all rows with missing values
data3 <- na.omit(data1)

#install.packages("DMwR")
sapply(data1, class)
data2 <- data1
cols.num <- c(1:42)
data2[cols.num] <- sapply(data1[cols.num],as.character)
sapply(data2, class)

## Method 2 - imputing missing values using KNN Imputation
library(DMwR)
data2 <- knnImputation(data1, k = 10, scale = T, meth = "weighAvg", distData = NULL)
library(data.table)
data2 <- as.data.table(data2)
cols <- names(data2)[1:42]
data2[,(cols) := round(.SD,0), .SDcols=cols]

Missing = function(x)
{ 
  # d would be used to store column names and e would be used to store corresponding number of missing values in the column
  d <- NA
  e <- NA
  
  # Calculating the number of columns in the data frame
  p <- ncol(x)
  for(i in 1:p)
  {
    d[i] <- colnames(x)[i]
    e[i] <- sum(is.na(x[,i]))
  }
  
  # Combining the two vectors
  r <- cbind(d,e)
  r <- data.frame(r)
  
  # Renaming the vectors appropriately
  setnames(r, old = c('d','e'), new = c('Column Name','No. of Missing Values'))
  return(r)
}
##
Missing(data1)
Missing(data.frame(data2))

library(readr)
GPA_Food <- read.csv("D:/Naresh/MS BAPM/Statistical Analysis using R/Project/GPA Food.csv")
library(corrgram) # Load corrgram

table(GPA_Food$calories_chicken) # answer is 720, highest responses gave answer as 610
table(GPA_Food$calories_day) # 63 respondents feel consuming calories per day is moderately important 
table(GPA_Food$calories_scone) # answer is 420, highest responses gave answer as 420
table(GPA_Food$coffee) # 94 selected coffee correctly
table(GPA_Food$comfort_food_reasons_coded) # 49 said boredom was reason for comfort food,stress & depression these three are 90%
prop.table(table(GPA_Food$cook))*100 #40% - whenever they can, 28-Couple times a week
prop.table(table(GPA_Food$cuisine))*100 # 80% american, 12% mexican,spanish
prop.table(table(GPA_Food$diet_current_coded))*100 # 48% unhealthy, 40% healthy
prop.table(table(GPA_Food$drink))*100 # 56% soda, 44% juice
prop.table(table(GPA_Food$eating_changes_coded1))*100 # 35% worse quality, 26% healthier
prop.table(table(GPA_Food$eating_out))*100 #48% - 1-2 times a week
prop.table(table(GPA_Food$employment))*100 #47% no employment, 52% parttime
prop.table(table(GPA_Food$ethnic_food))*100 # 82% neutral to verylikely, in increasing order
prop.table(table(GPA_Food$exercise))*100 # 51% everyday, 39% twice a week``
prop.table(table(GPA_Food$father_education))*100 # 37% college, 23% graduate, 27 High school
prop.table(table(GPA_Food$fav_cuisine_coded))*100 # 47% mexican
prop.table(table(GPA_Food$fav_food))*100 # 60% home cooked, 31% store baught cooked home
prop.table(table(GPA_Food$fruit_day))*100 # 50% very likely, 26% likely
prop.table(table(GPA_Food$grade_level))*100 # 1 - freshman, 2 -Sophomore, 3 - Junior, 4 - Senior
prop.table(table(GPA_Food$healthy_feeling))*100 #1 to 10 where 1 is strongly feel healthy   
prop.table(table(GPA_Food$sports))*100 # 61% sports yes
prop.table(table(GPA_Food$vitamins))*100 # do you take vitamins



#_____________________________________________________________________________________________________________________________________________________

# data preprocessing : removed null values from gpa, father and mothers education

#H0: Mothers education has no relation with GPA of childerens 

# Parametric

GPA_Mother <- GPA_Food[which(is.na(GPA_Food$GPA=='FALSE')&is.na(GPA_Food$mother_education=='FALSE')),]

nohighschool = GPA_Food[GPA_Food$mother_education==1,1]
highschool = GPA_Food[GPA_Food$mother_education!=1,1]
mean(nohighschool$GPA,na.rm = T)
sd(nohighschool$GPA,na.rm = T)
mean(highschool$GPA,na.rm = T)
sd(highschool$GPA,na.rm = T)
nrow(highschool)
mean(GPA_Food$GPA,na.rm = T)

f1=function()
{
  s1=rnorm(5,mean=3.4278,sd=0.7539647)
  s2=rnorm(112,mean=3.4278,sd=0.3604)
  return(abs(mean(s1)-mean(s2)))
}

#distribution
dist=replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col="red")

# compute the test statistic - tstat
tstat=abs(mean(nohighschool$GPA,na.rm = T)-mean(highschool$GPA,na.rm = T))
abline(v=tstat)

# computing the p-value
gap = abs(mean(dist)-tstat)  # find the difference between mean of sampling distribution and given sample value on distribution
gap
#lside = dist[dist<mean(dist)-gap] # finding the number of observations which falls on left side of the given sample value on sampling distribution
rside = dist[dist>mean(dist)+gap] # finding the number of observations which falls on right side of the given sample value on sampling distribution

pvalue = length(rside)/length(dist)
pvalue   # P value 0.176 : Hence we can not reject the null hypothesis that  there is no relationship between child's GPA and mothers education

# Non - Parametric

#H0: Mothers education has no relation with GPA of childerens

g=GPA_Food$GPA
sample(g)   # this randomly shuffles the values of g (So we achieve objective of randomly shuffling the data)
length(g)

f1=function()
{
  x=sample(g,replace = TRUE)
  z=abs(mean(x[1:5]-mean(x[6:117])))  # after random shuffle we take first 21 rows as one collection and last 23 rows as other collection, and check mean difference
  return(z)
}

#distribution
dist=replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col="red")

# compute the test statistic - tstat
tstat=abs(mean(nohighschool$GPA)-mean(highschool$GPA))
abline(v=tstat)

# computing the p-value
gap = abs(mean(dist)-tstat)  # find the difference between mean of sampling distribution and given sample value on distribution
gap
#lside = dist[dist<mean(dist)-gap] # finding the number of observations which falls on left side of the given sample value on sampling distribution
rside = dist[dist>mean(dist)+gap] # finding the number of observations which falls on right side of the given sample value on sampling distribution

pvalue = length(rside)/length(dist)
pvalue   # P value 0.01011 : Hence We can reject the null hypothesis that there is no relationship between child's GPA and mothers education

# we trust nonparametric test more and based on that we can say that there is relationship between GPA and mothers education

#_____________________________________________________________________________________________________________________________________________________

#H0: Fathers education has no relation with GPA of childerens

# Parametric

nohighschool = GPA_Food[GPA_Food$father_education==1,1]
highschool = GPA_Food[GPA_Food$father_education!=1,1]
mean(nohighschool$GPA,na.rm = T)
sd(nohighschool$GPA,na.rm = T)
mean(highschool$GPA,na.rm = T)
sd(highschool$GPA,na.rm = T)
nrow(highschool)
mean(GPA_Food$GPA,na.rm = T)

f1=function()
{
  s1=rnorm(4,mean=3.4278,sd=0.76268)
  s2=rnorm(113,mean=3.4278,sd=0.3714)
  return(abs(mean(s1)-mean(s2)))
}

#distribution
dist=replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col="red")

# compute the test statistic - tstat
tstat=abs(mean(nohighschool$GPA,na.rm = T)-mean(highschool$GPA,na.rm = T))
abline(v=tstat)

# computing the p-value
gap = abs(mean(dist)-tstat)  # find the difference between mean of sampling distribution and given sample value on distribution
gap
#lside = dist[dist<mean(dist)-gap] # finding the number of observations which falls on left side of the given sample value on sampling distribution
rside = dist[dist>mean(dist)+gap] # finding the number of observations which falls on right side of the given sample value on sampling distribution

pvalue = length(rside)/length(dist)
pvalue   # P value 0.3438 : Hence we can not reject the null hypothesis
# Non - Parametric

#H0: Fathers education has no relation with GPA of childeren

g=GPA_Food$GPA
sample(g)   # this randomly shuffles the values of g (So we achieve objective of randomly shuffling the data)
length(g)

f1=function()
{
  x=sample(g)
  z=abs(mean(x[1:4]-mean(x[4:117])))  # after random shuffle we take first 21 rows as one collection and last 23 rows as other collection, and check mean difference
  return(z)
}

#distribution
dist=replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col="red")

# compute the test statistic - tstat
tstat=abs(mean(nohighschool$GPA)-mean(highschool$GPA))
abline(v=tstat)

# computing the p-value
gap = abs(mean(dist)-tstat)  # find the difference between mean of sampling distribution and given sample value on distribution
gap
#lside = dist[dist<mean(dist)-gap] # finding the number of observations which falls on left side of the given sample value on sampling distribution
rside = dist[dist>mean(dist)+gap] # finding the number of observations which falls on right side of the given sample value on sampling distribution

pvalue = length(rside)/length(dist)
pvalue   # P value 0.0605 : Hence We can almost reject the null hypothesis 

# we trust nonparametric test more and based on that we can say that there is relationship between GPA and fathers education



#_____________________________________________________________________________________________________________________________________________________


#H0: Interest in sports has no relation with GPA of children 

# Parametric

GPA_Sports <- GPA_Food[which((is.na(GPA_Food$GPA)=='FALSE')&(is.na(GPA_Food$sports)=='FALSE')),]

No = GPA_Sports[GPA_Sports$sports==2,1]
Yes = GPA_Sports[GPA_Sports$sports==1,1]
mean(No$GPA,na.rm = T)
sd(No$GPA,na.rm = T)
mean(Yes$GPA,na.rm = T)
sd(Yes$GPA,na.rm = T)
nrow(Yes)
mean(GPA_Food$GPA,na.rm = T)

f1=function()
{
  s1=rnorm(41,mean=3.4278,sd=0.406)
  s2=rnorm(74,mean=3.4278,sd=0.358)
  return(abs(mean(s1)-mean(s2)))
}

#distribution
dist=replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col="red")

# compute the test statistic - tstat
tstat=abs(mean(No$GPA,na.rm = T)-mean(Yes$GPA,na.rm = T))
abline(v=tstat)

# computing the p-value
gap = abs(mean(dist)-tstat)  # find the difference between mean of sampling distribution and given sample value on distribution
gap
#lside = dist[dist<mean(dist)-gap] # finding the number of observations which falls on left side of the given sample value on sampling distribution
rside = dist[dist>mean(dist)+gap] # finding the number of observations which falls on right side of the given sample value on sampling distribution

pvalue = length(rside)/length(dist)
pvalue   # P value 0.207 : Hence we can not reject the null hypothesis 

# Non - Parametric

#H0: Interest in sports has no relation with GPA of children's 

g=GPA_Sports$GPA
sample(g)   # this randomly shuffles the values of g (So we achieve objective of randomly shuffling the data)
length(g)

f1=function()
{
  x=sample(g)
  z=abs(mean(x[1:41]-mean(x[42:115])))  
  return(z)
}

#distribution
dist=replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col="red")

# compute the test statistic - tstat
tstat=abs(mean(No$GPA)-mean(Yes$GPA))
abline(v=tstat)

# computing the p-value
gap = abs(mean(dist)-tstat)  # find the difference between mean of sampling distribution and given sample value on distribution
gap
#lside = dist[dist<mean(dist)-gap] # finding the number of observations which falls on left side of the given sample value on sampling distribution
rside = dist[dist>mean(dist)+gap] # finding the number of observations which falls on right side of the given sample value on sampling distribution

pvalue = length(rside)/length(dist)
pvalue   # P value 0.21366 : Hence We can  reject the null hypothesis 



#_____________________________________________________________________________________________________________________________________________________




#nonparametric two sample test using bootstrapping
#Ho =  average performace of male and female students are the same
#which means no difference in the average GPA of male and female students

data <- read_csv("~/.Business Analytics and Project management/OPIM-5603_Statistics in Business Analytics/group project/data.csv")


library(recoder)       #loading recoder package for recoding values

gender = recoder(data$Gender, '1:"female";2:"male"')  #recoding gender into female & male
gpa = as.numeric(data$GPA)


a = data.frame(gender, gpa)  #combining gender and gpa into a separate dataframe
a1 = na.omit(a)              #excluding missing values rows in the "a" dataframe


plot(a1$gender,a1$gpa, ylab = "gpa")      #plotting gender and gpa

#Hypothesis Testing

malegpa = a1[a1$gender=="male",2]
femalegpa = a1[a1$gender=="female",2]

n1 = length(malegpa)
n3 = length(a1$gpa)

tstat=abs(mean(malegpa)-mean(femalegpa))

f1 = function()           #function to return the absolute differnce in mean of male and female respondents
{
  s = sample(a1$gpa)
  mmale = mean(s[1:47])
  mfemale = mean(s[48:120])
  return(abs(mmale-mfemale))  
}

sdist = replicate(10000,f1())
plot(density(sdist))
polygon(density(sdist),col="yellow")
abline(v=tstat,lwd=2)

#calculating the pvalue
rset = sdist[sdist>=tstat]
p_value = length(rset)/length(sdist)
p_value 

#since pvalue is 0.4191, fail to reject null hypothesis

#Parametic version of the above two sample test
#involving categorical and numeric data


f2 = function()      #function to return absolute diff. between male and female gpa
{
  mmgpa = mean(rnorm(n = 47,mean = tmean,sd = sd(a1$gpa)))
  fmgpa = mean(rnorm(n = 73,mean = tmean,sd=sd(a1$gpa)))
  return(abs(mmgpa-fmgpa))
}

sdist = replicate(10000,f2())   #distribution of the above function

plot(density(sdist))
polygon(density(sdist),col="green")

tstat = abs(mean(malegpa)-mean(femalegpa))

#calucalting the pvalue
abline(v=tstat,lwd=2)
rset = sdist[sdist>=tstat]
p_value = length(rset)/length(sdist)
p_value

#fail to reject the null hypothesis since the pvalue is 0.4191
# the parametic and nonparametic approach yielded the same result



#################################################Testing
# Two sample t-test
m <- data[which((data$Gender==1)&(is.na(data$Gender)=='FALSE')&(is.na(data$GPA1)=='FALSE')),62]
f <- data[which((data$Gender==2)&(is.na(data$Gender)=='FALSE')&(is.na(data$GPA1)=='FALSE')),62]

data4 <- data[which((is.na(data$Gender)=='FALSE')&(is.na(data$GPA1)=='FALSE')),]
#to verify the homoskedasticity (homogeneity of variances)
var.test(m,f)

#t test
t.test(m,f, var.equal=TRUE, paired=FALSE)

#non-parametric equivalent wilcox test
wilcox.test(GPA1 ~ Gender, data=data4) 

###maximum likelihood test for mean
x <- data2$GPA1
LLmean = function(M) 
{   
  sum(dnorm(x,mean = M,sd = sd(data2$GPA1),log = T)) 
}
Mvalues = seq(0,4,by = 0.1) 
LLres = sapply(Mvalues,LLmean) 
plot(Mvalues,LLres,type="l",col="blue",xlab = "M Values",ylab="Log Likelihood ",main="Likelihood")
y = which.max(LLres) 
Mvalues[y]

library(bbmle)
LLmean = function(M) {   
  LLsum = sum(dnorm(x,mean = M,sd = sd(data2$GPA1),log = T))   
  return(-1*LLsum) } 
res = mle2(minuslogl = LLmean,start = list(M=10)) 

summary(res)

##by bootstrap sampling
s = sample(x,replace = T) 
f1 = function() {   
  s = sample(x,replace = T)  
  LLmean = function(M)   
  {     LLsum = sum(dnorm(s,mean = M,sd = sd(data2$GPA1),log = T))     
  return(-1*LLsum)   
  }   
  res = mle2(minuslogl = LLmean,start = list(M=10))   
  return(res@coef) } 

sdist = replicate(1000,f1()) 
mean(sdist) 
plot(density(sdist)) 
quantile(sdist,probs = c(0.025,1-0.025)) 

quantile(sdist,probs = c(0.025,1-0.025)) 

##factorial anova
model <- lm(GPA1 ~.,data=data2)
library(car)
Anova(model,
      type = "II")
l <-Anova(model,
          type = "II")
l
data5 <- data.frame(cbind(data$GPA1,data$weight1))
library(data.table)
data6 <- cbind(data$GPA1,data$weight1)
##Correlations
cor(data5, use="complete.obs", method="kendall")

# Correlations with significance levels
install.packages("Hmisc")
library(Hmisc)
rcorr(data6, type="pearson") 

library(corrgram)
names(data5) <- c("GPA","Weight")
corrgram(data5, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="GPA Vs Weight")
data51 <- na.omit(data5)
attach(data51)
plot(Weight,GPA, main="GPA Vs Weight", 
     xlab="Weight", ylab="GPA", pch=19)
# Add fit lines
abline(lm(GPA~Weight), col="red") # regression line (y~x) 
lines(lowess(Weight,GPA), col="blue") # lowess line (x,y)

#############################CHISQUARE TESTS
#NUTRITIONAL_CHECK, INDIAN_FOOD, THAI_FOOD, VITAMINS, EXERCISE,GREEK_FOOD,IDEAL_DIET_CODED,HEALTHY_FEELING are signifiacnt variables

tbl <- table(data$exercise,data$grade_level)
chisq.test(tbl)
dataset <- data[which((data$diet_current_coded==1)),]
dataset2 <- data[which((data$diet_current_coded==2)),]
dataset4 <- data[which((data$diet_current_coded==3)),]

dataset3 <- rbind(dataset,dataset2,dataset4)
dataset3$diet_current_coded <- ifelse(dataset3$diet_current_coded >=2, 2, 1)
tbl <- table(dataset3$ideal_diet_coded,dataset3$diet_current_coded)
chisq.test(tbl)
tbl <- table(dataset3$eating_out,dataset3$diet_current_coded)
chisq.test(tbl)

#Ch-square test betwen calories and nutritional_check
tbl <- table(data$calories_day,data$nutritional_check)
chisq.test(tbl)

#Ch-square test betwen calories and indian food
tbl <- table(data$calories_day,data$indian_food)
chisq.test(tbl)

#Ch-square test betwen calories and thai food
tbl <- table(data$calories_day,data$thai_food)
chisq.test(tbl)

#Ch-square test betwen calories and vitamins
tbl <- table(data$calories_day,data$vitamins)
chisq.test(tbl)

#Ch-square test betwen calories and exercise
tbl <- table(data$calories_day,data$exercise)
chisq.test(tbl)

#Ch-square test betwen calories and greek food
tbl <- table(data$calories_day,data$greek_food)
chisq.test(tbl)

#Ch-square test betwen calories and ideal diet
tbl <- table(data$calories_day,data$ideal_diet_coded)
chisq.test(tbl)

# One way Anova between GPA and cooking habit
test2 <- cbind(data$GPA1,data$cook)

# removing the NA and NaNs
test2 <- data.frame(test2[complete.cases(test2),])
names(test2)[1] <- "GPA"
names(test2)[2] <- "cook"

test2$cook <-as.factor(test2$cook)


res.aov <- aov(GPA ~ cook, data = test2)
# Summary of the analysis
summary(res.aov)

# One way Anova between GPA and type of cuisine
test3 <- cbind(data$GPA1,data$cuisine)

# removing the NA and NaNs
test3 <- data.frame(test3[complete.cases(test3),])
names(test3)[1] <- "GPA"
names(test3)[2] <- "cuisine"

test3$cuisine <-as.factor(test3$cuisine)


res.aov <- aov(GPA ~ cuisine, data = test3)
# Summary of the analysis
summary(res.aov)

# One way Anova between GPA and eating out frequency
test4 <- cbind(data$GPA1,data$eating_out)

# removing the NA and NaNs
test4 <- data.frame(test4[complete.cases(test4),])
names(test4)[1] <- "GPA"
names(test4)[2] <- "eating_out"

test4$eating_out <-as.factor(test4$eating_out)


res.aov <- aov(GPA ~ eating_out, data = test4)
# Summary of the analysis
summary(res.aov)

# One way Anova between GPA and father's education

test5 <- cbind(data$GPA1,data$father_education)

# removing the NA and NaNs
test5 <- data.frame(test5[complete.cases(test5),])
names(test5)[1] <- "GPA"
names(test5)[2] <- "father_edu"

test5$father_edu <-as.factor(test5$father_edu)


res.aov <- aov(GPA ~ father_edu, data = test5)
# Summary of the analysis
summary(res.aov)

# One way Anova between GPA and favorite food

test6 <- cbind(data$GPA1,data$fav_food)

# removing the NA and NaNs
test6 <- data.frame(test6[complete.cases(test6),])
names(test6)[1] <- "GPA"
names(test6)[2] <- "fav_food"

test6$fav_food <-as.factor(test6$fav_food)


res.aov <- aov(GPA ~ fav_food, data = test6)
# Summary of the analysis
summary(res.aov)

# One way Anova between GPA and indian food

test6 <- cbind(data$GPA1,data$indian_food)

# removing the NA and NaNs
test6 <- data.frame(test6[complete.cases(test6),])
names(test6)[1] <- "GPA"
names(test6)[2] <- "indian_food"

test6$indian_food <-as.factor(test6$indian_food)

## Now Kruskal Wallis test which is non-parametric equivalent of one-way ANOVA
test7 <- cbind(data$GPA1,data$indian_food)
test7 <- data.frame(test7[complete.cases(test6),])
names(test7)[1] <- "GPA"
names(test7)[2] <- "indian_food"

test7$indian_food <-as.factor(test7$indian_food)
kruskal.test(GPA ~ indian_food, data = test7)


# One way Anova between GPA and sports
test8 <- cbind(data$GPA1,data$sports)

# removing the NA and NaNs
test8 <- data.frame(test8[complete.cases(test8),])
names(test8)[1] <- "GPA"
names(test8)[2] <- "sports"

test8$sports <-as.factor(test8$sports)


res.aov <- aov(GPA ~ sports, data = test8)
# Summary of the analysis
summary(res.aov)

# Visualizing data
#install.packages("ggpubr")

library("ggpubr")

# One way Anova between GPA and calories per day
test1 <- cbind(data$GPA1,data$calories_day)

# removing the NA and NaNs
test1 <- data.frame(test1[complete.cases(test1),])
names(test1)[1] <- "GPA"
names(test1)[2] <- "calories_day"

test1$calories_day <-as.factor(test1$calories_day)
levels(test1$calories_day)

# Box plots
ggboxplot(test1, x = "calories_day", y = "GPA", 
          color = "calories_day", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("2", "3", "4"),
          ylab = "GPA", xlab = "calories_day")

# Mean plots

library("ggpubr")

ggline(test1, x = "calories_day", y = "GPA", 
       add = c("mean_se", "jitter"), 
       order = c("2", "3", "4"),
       ylab = "GPA", xlab = "calories_day")

# Compute the analysis of variance
res.aov <- aov(GPA ~ calories_day, data = test1)
# Summary of the analysis
summary(res.aov)

## we fail to reject the null hypothesis that mean GPA for all 3 calories groups are the same

#install.packages("Rcmdr")
library(car)

leveneTest(test1$GPA,test1$calories_day)
## we fail to reject the null hypothesis that the groups have equal variances which is the condition pre-requisite for one-way anova

#####################################################

## Now Kruskal Wallis test which is non-parametric equivalent of one-way ANOVA

kruskal.test(GPA ~ calories_day, data = test1) 

## p-value is the same as calculated before from on-way ANOVA


# Doing normal regression giving poor adjuseted r square

lm.fit <- lm(GPA1~ .,data=data3)
summary(lm.fit)

#Doing Stepwise regression to improve adjusted r-square

library(MASS)
fit <- lm(GPA1~.,data=data3)
step <- stepAIC(fit, direction="both")
step$anova # display results

lm.fit2 <- lm(GPA1 ~ Gender + breakfast + drink + eating_changes_coded + employment + 
                ethnic_food + exercise + fav_cuisine_coded + fav_food + fries + 
                grade_level + greek_food + healthy_feeling + income + indian_food + 
                italian_food + life_rewarding + mother_education + nutritional_check + 
                parents_cook + persian_food + thai_food + vitamins + weight1 + 
                eating_changes_coded1,data=data3)
summary(lm.fit2)

####testing the regression model
##Calculating RMSE for optimized linear regression model
index <- sample(1:nrow(data3),round(0.75*nrow(data3)))
train <- data3[index,]
test <- data3[-index,]
pfit <- lm(GPA1 ~ Gender + breakfast + drink + eating_changes_coded + employment + 
             ethnic_food + exercise + fav_cuisine_coded + fav_food + fries + 
             grade_level + greek_food + healthy_feeling + income + indian_food + 
             italian_food + life_rewarding + mother_education + nutritional_check + 
             parents_cook + persian_food + thai_food + vitamins + weight1 + 
             eating_changes_coded1,data=train)
pr.dt <- predict(pfit,test)
MSE.dt <- sum((pr.dt - test$GPA1)^2)/nrow(test)
RMSE <- sqrt(MSE.dt)

###Doing boxTidwell test### not working for more than 5 variables
boxTidwell(GPA1~Gender + breakfast + drink + eating_changes_coded + employment,
           data=data3)

#####tried log transformations--no improvement
reg3 = lm(log(GPA1)~ Gender + breakfast + drink + eating_changes_coded + employment + 
            ethnic_food + exercise + fav_cuisine_coded + fav_food + fries + 
            grade_level + greek_food + healthy_feeling + income + indian_food + 
            italian_food + life_rewarding + mother_education + nutritional_check + 
            parents_cook + persian_food + thai_food + vitamins + weight1 + 
            eating_changes_coded1,data = data3)
summary(reg3)

boxCox(reg3, family="yjPower", plotit = TRUE) 

##lasso regression gives R square of 0.81
# load the package
#install.packages("lars")
library(lars)
# load data

## Calculating R2 using Lasso Regression
library(lars)
# load data

x <- as.matrix(data3[,c(1:42,44)])
y <- as.matrix(data3[,43])
# fit model
fit <- lars(x, y, type="lasso")

# summarize the fit
summary(fit)
# select a step with a minimum error
best_step <- fit$df[which.min(fit$RSS)]
# make predictions
predictions <- predict(fit, x, s=best_step, type="fit")$fit
# summarize accuracy
mse <- mean((y - predictions)^2)
print(mse)
#Calculating R-squared
fit$R2


### testing lasso and ridge and calculating MSE
### Trying another form of lasso regression optimizing lamda
#install.packages("glmnet")
library(glmnet)
set.seed(489)
x <- as.matrix(data3[,c(1:42,44)])
y <- as.matrix(data3[,43])
lambda <- 10^seq(10, -2, length = 100)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)
#find the best lambda from our list via cross-validation
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)

bestlam <- cv.out$lambda.min
#make predictions
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
swisslm <- lm(GPA1~., data = data3, subset = train)
s.pred <- predict(swisslm, newdata = data3[test,])
#check MSE
MSE <-mean((s.pred-ytest)^2)
MSE

lasso.mod <- cv.glmnet(x, y, alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')
#lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = bestlam)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
MSE <- mean((lasso.pred-ytest)^2)
MSE

#####Visualization code
library(ggplot2)
library(ggplot)
food_coded_PRE <- data
attach(food_coded_PRE)

is.numeric(food_coded_PRE$coffee)

ggplot(food_coded_PRE, aes(x = Gender, fill = factor(as.numeric(GPA))))+
  geom_bar()

ggplot(food_coded_PRE, aes(x = breakfast, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = calories_day, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = cook, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

##not running on my system
ggplot(food_coded_PRE, aes(x = food_coded_PRE$`cuisine(M)`, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = food_coded_PRE$diet_current_coded, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = eating_out, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = employment, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = ethnic_food, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = exercise, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = fruit_day, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = grade_level, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = greek_food, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = income, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = indian_food, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = italian_food, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = marital_status, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = nutritional_check, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = on_off_campus, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = parents_cook, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = pay_meal_out, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = persian_food, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = food_coded_PRE$self_perception_weight, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = sports, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = thai_food, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = vitamins, fill = factor(Gender)))+
  geom_bar(position = 'dodge')

ggplot(food_coded_PRE, aes(x = weight, fill = factor(Gender)))+
  geom_bar(position = 'dodge')


#the boxplots which show the relationship between GPA and other variables.
ggplot(food_coded_PRE, aes(x = as.character(food_coded_PRE$Gender), y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = as.character(food_coded_PRE$breakfast), y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = food_coded_PRE$calories_day, y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = food_coded_PRE$cook, y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()#create the boxplot to see the relationship between GPA and the cook.

ggplot(food_coded_PRE, aes(x = food_coded_PRE$`cuisine(M)`, y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = as.character(food_coded_PRE$diet_current_coded), y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = as.character(food_coded_PRE$eating_out), y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = food_coded_PRE$employment, y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = as.character(food_coded_PRE$ethnic_food), y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = food_coded_PRE$exercise, y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = as.character(food_coded_PRE$fruit_day), y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = food_coded_PRE$grade_level, y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = as.character(food_coded_PRE$greek_food), y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = food_coded_PRE$income, y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = as.character(food_coded_PRE$indian_food), y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = food_coded_PRE$italian_food, y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = food_coded_PRE$marital_status, y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = as.character(food_coded_PRE$nutritional_check), y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = food_coded_PRE$on_off_campus, y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = as.character(food_coded_PRE$parents_cook), y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = as.character(food_coded_PRE$pay_meal_out), y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = food_coded_PRE$persian_food, y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = food_coded_PRE$self_perception_weight, y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = food_coded_PRE$sports, y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = as.character(food_coded_PRE$thai_food), y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(x = as.character(food_coded_PRE$vitamins), y = as.numeric(food_coded_PRE$GPA)))+
  geom_boxplot()

ggplot(food_coded_PRE, aes(as.numeric(x = food_coded_PRE$weight), y = as.numeric(food_coded_PRE$GPA)))+
  geom_point()

#pie plot 
ggplot(food_coded, aes(x = "", y = as.numeric(food_coded_PRE$GPA), fill = Gender))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "y")


ggplot(food_coded_PRE, aes(as.numeric(food_coded_PRE$GPA), fill = cook))+
  geom_bar()

data=lapply(food_coded_PRE,as.numeric)
data
as.numeric(food_coded_PRE$GPA)
install.packages("corrgram")
library(corrgram)
corrgram(data, order = T) 
is.numeric(food_coded_PRE$GPA)
#tile plot
ggplot(food_coded_PRE,aes(x = food_coded_PRE$cook, y = food_coded_PRE$GPA))+
  geom_tile(aes(fill = Gender))

