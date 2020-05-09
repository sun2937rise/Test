rm(list = ls(all = TRUE))
setwd("C:/Users/13127/OneDrive/Documents/research assistant")
# reading excle file through R
library(readxl)
data = read_excel("phase1_new.xlsx", sheet = 2)
str(data)

# converting missing data 999 and 888 to NA 
data[data == 999| data == 888] = NA

# checking the missing data
sum(is.na(data))
rowSums(is.na(data))
colSums(is.na(data))
# clustering the variables 
library(ClustOfVar)
data1 = scale(data)
fit = hclustvar(data1)
plot(fit)
groups=cutreevar(fit, k=5);groups # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

#stab <- stability(fit,B=40)
#plot(stab, main="Stability of the partitions")
#stab$matCR
#boxplot(stab$matCR, main="Dispersion of the ajusted Rand index")
#groups$var

cluster1 = data[,c(1,4,88:118)]
cluster2 = data[,c(2,7,8,18:23,25,27,29:40,42,45,48:49,51,55,56,58,62,63,66,67,77,78,80)]
cluster3 = data[,c(3,5,6,9:17,24,26,28,41,43,44,46,47,50,52,53,54,57,59:61,64,65,68:71)]
cluster4 = data[,c(72:76,79,81:87)]
cluster5 = data[,c(119:138)]

summary(cluster1)
summary(cluster2)
summary(cluster3)
summary(cluster4)
summary(cluster5)

# visiualizing missing data
library(VIM)
aggr_plot=aggr(cluster1,col=c('grey','red'),numbers=TRUE,sortVars=TRUE, 
               labels=names(cluster1),cex.axis=.7,gap=3,
               ylab=c("Histogram of missing data","Pattern"))

matrixplot(cluster1, sortby = 2) # This is useful to check if there is an association 
#between the value of a variable and the missingness of another one.
marginplot(cluster1[,c("ysr1","cyrm11")])

library(mice)
set.seed(1234)
imp1=mice(cluster1, m=1, maxit=40)
densityplot(imp1)
imp1$imp$ysr1

set.seed(1234)
imp2=mice(cluster2, m=1, maxit=40)
densityplot(imp2)
imp2$imp$ysr31

set.seed(1234)
imp3=mice(cluster3, m=1, maxit=40)
densityplot(imp3)
imp3$imp$ysr8

set.seed(1234)
imp4=mice(cluster4, m=1, maxit=40)
densityplot(imp4)
imp4$imp$school1
imp4$imp$school4

set.seed(1234)
imp5=mice(cluster5, m=1, maxit=40)
densityplot(imp5)
imp5$imp$cpss16

#library(Amelia)
#aml1 = amelia(cluster4, 1)
#compare.density(aml1, var="school4")
#compare.density(aml1, var="attach4")
#compare.density(aml1, var="goals2")

imputed_data1 = mice::complete(imp1)
imputed_data2 = mice::complete(imp2)
imputed_data3 = mice::complete(imp3)
imputed_data4 = mice::complete(imp4)
imputed_data5 = mice::complete(imp5)

imputed_data = cbind(imputed_data1,imputed_data2,imputed_data3,imputed_data4,imputed_data5)
sum(is.na(imputed_data))

write.csv(imputed_data, file = "imputed_newdata.csv")

##########################################################################################
library(readxl)
data2 = read_excel("imputed_newdata.xlsx", sheet = 3)
data3 = data2[,c(1:7,13:21,24)]
# Using Lasso refression for reducing the dimention of the predictors
library(caret)
library(tidyverse)
# Split the data into training and test set
set.seed(123)
training.samples <-data3$Internalizing_YSR %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- data3[training.samples, ]
test.data <- data3[-training.samples, ]

# Dumy code categorical predictor variables
x <- model.matrix(cbind(Internalizing_YSR,Externalizing_YSR)~., train.data)[,-1]
# Convert the outcome (class) to a numerical variable
y <- cbind(train.data$Internalizing_YSR,train.data$Externalizing_YSR) # Split data into train and test sets


library(glmnet)
glmnet(x, y, family = "mgaussian", alpha = 1, lambda = NULL)
# x: matrix of predictor variables
# y: the response or outcome variable.
# family: the response type. Use "binomial" for a binary outcome variable
# alpha: the elasticnet mixing parameter. Allowed values include:
# "1": for lasso regression
# "0": for ridge regression
# a value between 0 and 1 (say 0.3) for elastic net regression.
# lamba: a numeric value defining the amount of shrinkage. Should be specify by analyst.

# Find the best lambda using cross-validation(the optimal lambda is the one that minimize the MSE)
set.seed(123) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "mgaussian")
plot(cv.lasso)

cv.lasso$lambda.min #This lambda value will give the most accurate model
coef(cv.lasso, cv.lasso$lambda.min)
#Generally, the purpose of regularization is to balance accuracy and simplicity. 
# This means, a model with the smallest number of predictors that also gives a good 
# accuracy. To this end, the function cv.glmnet() finds also the value of lambda that 
# gives the simplest model but also lies within one standard error of the optimal 
# value of lambda. This value is called lambda.1se.

cv.lasso$lambda.1se #This lambda value will give a model with the smallest number of predictors
coef(cv.lasso, cv.lasso$lambda.1se)

# Fit the final model on the training data with lambda.min
lasso.model1 <- glmnet(x, y, alpha = 1, family = "mgaussian",
                       lambda = cv.lasso$lambda.min)

# Make predictions on the test data
x.test <- model.matrix(cbind(Internalizing_YSR,Externalizing_YSR) ~., test.data)[,-1]
y.test <- cbind(test.data$Internalizing_YSR,test.data$Externalizing_YSR)
lasso.pred1 <- predict(lasso.model1, s = cv.lasso$lambda.min, newx = x.test)
mean(((lasso.pred1[,,1]-y.test)^2)[,1])
mean(((lasso.pred1[,,1]-y.test)^2)[,2])

# Fit the final model on the training data with lambda.1se
lasso.model2 <- glmnet(x, y, alpha = 1, family = "mgaussian",
                       lambda = cv.lasso$lambda.1se)

# Make predictions on the test data
lasso.pred2 <- predict(lasso.model2, s = cv.lasso$lambda.1se, newx = x.test)
mean(((lasso.pred2[,,1]-y.test)^2)[,1])
mean(((lasso.pred2[,,1]-y.test)^2)[,2])

################################################################################
# For using manova there are some assumptions we need to check before apllying 
# manova function perform the Shapiro-Wilk test for multivariate normality(dependent variables should be normally distribute)

lm1 = lm(sqrt(Internalizing_YSR) ~ Dummy_gender+Total_Hope+Total_CPSStrauma+Total_reversedschool+
           Total_Parenttachment+Totalsafety+Personalskills_individualCYRM+
           Peersupport_individualCYRM+Socialskills_individualCYRM+
           Physicalcare_FamilyCYRM+Psychologicalcare_FamilyCYRM+
           Spiritual_ContextCYRM+Education_ContextCYRM+
           Cultural_ContextCYRM,data = data2)

lm2 = lm(sqrt(Externalizing_YSR) ~ Dummy_gender+Total_Hope+Total_CPSStrauma+
           Total_reversedschool+Total_Parenttachment+Totalsafety+
           Personalskills_individualCYRM+Peersupport_individualCYRM+
           Socialskills_individualCYRM+Physicalcare_FamilyCYRM+
           Psychologicalcare_FamilyCYRM+Spiritual_ContextCYRM+
           Education_ContextCYRM+Cultural_ContextCYRM,data = data2) 

#Assumption 1 - Linearity of Relationship
plot(lm1,1)
plot(lm2,1)
# There is no patern around horizontal line so linearity is met

#Assumption 2 - Independence of Variables
# visualizing the correlation between variables
cordata = cor(data2, use = "pairwise.complete.obs")
library(corrplot)
corrplot(cordata, method = "circle", main="Pearson Correlation")
corrplot(cordata, method = "color", outline = T, cl.pos = 'n',
         rect.col = "blue",  tl.col = "blue", addCoef.col = "black",
         number.digits = 2, mar=c(0,0,1,0),number.cex = 0.60, tl.cex = 0.7, cl.cex = 1,
         col = colorRampPalette(c("green","white","purple"))(100),
         main="Pearson Correlation")
# No high correlation between independent variables

#Assumption 3 - Normal Distribution of Residuals
plot(lm1,2) # not met
plot(lm2,2) # not met

# transformation with powertransform 
library(car)
powerTransform(lm1, family = "bcnPower")
powerTransform(lm2, family = "bcnPower")
# lambda = 0.5 so square root is suitable for transformation

#Assumption 4 - Homoscedasticity or Equal Variance of Variables
plot(lm1,3) # met
plot(lm2,3) # met

###########################################################################
# model 1()
data2$Internalizing_YSR = sqrt(data2$Internalizing_YSR)
data2$Externalizing_YSR = sqrt(data2$Externalizing_YSR)
data4 = data.frame(scale(data2))

fit1 = lm(cbind(Internalizing_YSR,Externalizing_YSR) ~ Dummy_gender + 
            Dummy_African + Dummy_Latinx,
          data = data4)
summary(fit1)

fit2 = lm(cbind(Internalizing_YSR,Externalizing_YSR) ~ 
            Total_Hope+Total_reversedschool,data = data4)
summary(fit2)

fit3 = lm(cbind(Internalizing_YSR,Externalizing_YSR) ~ Total_Parenttachment
          , data = data4)
summary(fit3)

fit4 = lm(cbind(Internalizing_YSR,Externalizing_YSR) ~ Totalsafety ,data = data4)
summary(fit4)

fit5 = lm(cbind(Internalizing_YSR,Externalizing_YSR) ~ 
            Total_Hope*Dummy_African + Total_Hope*Dummy_Latinx +
            Totalsafety*Dummy_African + Totalsafety*Dummy_Latinx,
           data = data2)
summary(fit5)

#data2$Internalizing_YSR = sqrt(data2$Internalizing_YSR)
#data2$Externalizing_YSR = sqrt(data2$Externalizing_YSR)

library(tidyverse)
Black_race = data2 %>% filter(Recoded_race == 1)
Hispanic_race = data2 %>% filter(Recoded_race == 2)
Asian_race = data2 %>% filter(Recoded_race == 3)

Black_race = data.frame(scale(Black_race)) 
Hispanic_race = data.frame(scale(Hispanic_race))
Asian_race = data.frame(scale(Asian_race))

# Black race
# model 1(only gender)

fit1 = lm(Internalizing_YSR ~ Dummy_gender
            , data = Black_race)
summary(fit1)
fit1_1 = lm(Externalizing_YSR ~ Dummy_gender
          , data = Black_race)
summary(fit1_1)
# model 2(gender and hope, cpss)

fit4 = lm(Internalizing_YSR ~ Dummy_gender+
             Total_Hope+Total_CPSStrauma, data = Black_race)
summary(fit4)
fit4_4 = lm(Externalizing_YSR ~ Dummy_gender+
              poly(Total_Hope,3)+poly(Total_CPSStrauma,3), data = Black_race)
summary(fit4_4)
# model 3(gender, hope, cpss, school)

fit7 = lm(Internalizing_YSR ~ Dummy_gender+
            poly(Total_Hope,3)+poly(Total_CPSStrauma,3)+Total_reversedschool
                                                             ,data = Black_race)
summary(fit7)
fit7_7 = lm(Externalizing_YSR ~ Dummy_gender+
            Total_Hope+Total_CPSStrauma+Total_reversedschool,data = Black_race)
summary(fit7_7)
# model 4(gender, hope, school, parent)

fit10 = lm(Internalizing_YSR ~ Dummy_gender+
             poly(Total_Hope,3)+poly(Total_CPSStrauma,3)+Total_reversedschool
           +Total_Parenttachment,
           data = Black_race)
summary(fit10)
fit10_10 = lm(Externalizing_YSR ~ Dummy_gender+
             Total_Hope+Total_CPSStrauma+Total_reversedschool+Total_Parenttachment,
           data = Black_race)
summary(fit10_10)
# model 5(gender, hope, school, parent, Totalsafety)

fit13 = lm(Internalizing_YSR ~ Dummy_gender+
             poly(Total_Hope,3)+
             poly(Total_CPSStrauma,3)+Total_reversedschool+Total_Parenttachment+
             Totalsafety,
           data = Black_race)
summary(fit13)
fit13_13 = lm(Externalizing_YSR ~ Dummy_gender+poly(Total_Hope,3)+
                poly(Total_CPSStrauma,3)+Total_reversedschool
              +Total_Parenttachment+Totalsafety,data = Black_race)
summary(fit13_13)

anova(fit1,fit4,fit7,fit10,fit13)
anova(fit1_1,fit4_4,fit7_7,fit10_10,fit13_13)
#library(sjPlot)
#tab_model(fit1,
#          pred.labels = c("Female","Male"),
#          
#          file="test.doc")
      
# Hispanic race
# model 1(only gender)

fit2 = lm(Internalizing_YSR ~ Dummy_gender,
          data = Hispanic_race)
summary(fit2)
fit2_2 = lm(Externalizing_YSR ~ Dummy_gender,
          data = Hispanic_race)
summary(fit2_2)
# model 2(gender and hope)

fit5 = lm(Internalizing_YSR ~ Dummy_gender+
            Total_Hope+Total_CPSStrauma, data = Hispanic_race)
summary(fit5)
fit5_5 = lm(Externalizing_YSR ~ Dummy_gender+
            Total_Hope+Total_CPSStrauma, data = Hispanic_race)
summary(fit5_5)
# model 3(gender, hope, school)

fit8 = lm(Internalizing_YSR ~ Dummy_gender+
          Total_Hope+Total_CPSStrauma+Total_reversedschool,data = Hispanic_race)
summary(fit8)
fit8_8 = lm(Externalizing_YSR ~ Dummy_gender+
            Total_Hope+Total_CPSStrauma+Total_reversedschool,data = Hispanic_race)
summary(fit8_8)

# model 4(gender, hope, school, parent)

fit11 = lm(Internalizing_YSR ~ Dummy_gender+
             Total_Hope+Total_CPSStrauma+Total_reversedschool+Total_Parenttachment,
           data = Hispanic_race)
summary(fit11)
fit11_11 = lm(Externalizing_YSR ~ Dummy_gender+
             Total_Hope+Total_CPSStrauma+Total_reversedschool+Total_Parenttachment,
           data = Hispanic_race)
summary(fit11_11)
# model 5(gender, hope, school, parent, Totalsafety)

fit14 = lm(Internalizing_YSR ~ Dummy_gender+
             Total_Hope+Total_CPSStrauma+Total_reversedschool+Total_Parenttachment+
             Totalsafety,data = Hispanic_race)
summary(fit14)
fit14_14 = lm(Externalizing_YSR ~ Dummy_gender+
             Total_Hope+Total_CPSStrauma+Total_reversedschool+Total_Parenttachment+
               Totalsafety, data = Hispanic_race)
summary(fit14_14)

anova(fit2,fit5,fit8,fit11,fit14)
anova(fit2_2,fit5_5,fit8_8,fit11_11,fit14_14)
# Asian race
# model 1(only gender)

fit3 = lm(Internalizing_YSR ~ Dummy_gender,
          data = Asian_race)
summary(fit3)
fit3_3 = lm(Externalizing_YSR ~ Dummy_gender,
          data = Asian_race)
summary(fit3_3)
# model 2(gender and hope)

fit6 = lm(Internalizing_YSR ~ Dummy_gender+
            Total_Hope+Total_CPSStrauma, data = Asian_race)
summary(fit6)
fit6_6 = lm(Externalizing_YSR ~ Dummy_gender+
            Total_Hope+Total_CPSStrauma, data = Asian_race)
summary(fit6_6)
# model 3(gender, hope, school)

fit9 = lm(Internalizing_YSR ~ Dummy_gender+
            Total_Hope+Total_CPSStrauma+Total_reversedschool,data = Asian_race)
summary(fit9)
fit9_9 = lm(Externalizing_YSR ~ Dummy_gender+
            Total_Hope+Total_CPSStrauma+Total_reversedschool,data = Asian_race)
summary(fit9_9)

# model 4(gender, hope, school, parent)

fit12 = lm(Internalizing_YSR ~ Dummy_gender+
             Total_Hope+Total_CPSStrauma+Total_reversedschool+Total_Parenttachment,
           data = Asian_race)
summary(fit12)
fit12_12 = lm(Externalizing_YSR ~ Dummy_gender+
             Total_Hope+Total_CPSStrauma+Total_reversedschool+Total_Parenttachment,
           data = Asian_race)
summary(fit12_12)
# model 5(gender, hope, school, parent, Totalsafety)

fit15 = lm(Internalizing_YSR ~ Dummy_gender+
             Total_Hope+Total_CPSStrauma+Total_reversedschool+Total_Parenttachment+
             Totalsafety,data = Asian_race)
summary(fit15)
fit15_15 = lm(Externalizing_YSR ~ Dummy_gender+
             Total_Hope+Total_CPSStrauma+Total_reversedschool+Total_Parenttachment+
               Totalsafety,data = Asian_race)
summary(fit15_15)

anova(fit3,fit6,fit9,fit12,fit15)
anova(fit3_3,fit6_6,fit9_9,fit12_12,fit15_15)

library(sjPlot)
tab_model(fit3,fit6,fit9,
       show.ci = F, show.se = F,
       file="sjt_linear_2.doc")

#there is an interactions 
fit16 = lm(cbind(Internalizing_YSR, Externalizing_YSR)  ~ 
             Total_Hope*Total_CPSStrauma+Total_Hope*Totalsafety, data = Black_race)
summary(fit16)

#there is an interactions between hope and parent for Hispanic race in Externalizing
# and hope and safety in Internalizing
#fit17 = manova(cbind(sqrt(Internalizing_YSR), sqrt(Externalizing_YSR)) ~ 
#                 Dummy_gender*Total_Hope*Total_Parenttachment*Totalsafety,
#              data = Hispanic_race)
#summary.aov(fit17)
#fit17$coefficients

#new_d <- expand.grid(Total_Hope=c(6,12,18,24,30,36),Totalsafety=c(5,10,15,20,25))
#new_d$Pred <- predict(fit17,new_d)
#library(ggplot2)
#ggplot(aes(x=Totalsafety,y=Internalizing_YSR,color=Total_Hope),
#       data=Hispanic_race) + 
#  geom_point() +
#  geom_line(aes(group=Total_Parenttachment),data=new_d) +
#  scale_color_continuous(low="green",high="red") 

####################################################################################




