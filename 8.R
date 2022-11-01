setwd("D:/Documents/UIUC/Assignments/Data stats")

library(readr)

dtset=read.csv("Heart Failure2.csv")

head(dtset)
str(dtset)

sum(is.na(dtset))

library(dplyr)     # data wrangling
library(ggplot2)   # plotting
library(rsample)   # training and testing splitting
library(caret)     # for logistic regression modeling and prediction outputs
library(vip)       # variable importance


set.seed(123)  # use a set seed point for reproducibility
split <- initial_split(dtset, prop = .7, strata = "DEATH_EVENT")
train <- training(split)
test  <- testing(split)


#Logistic Regression

#For explaining dependent variable

dtset$DEATH_EVENT <- as.factor(dtset$DEATH_EVENT)
names(dtset)
log_reg <- glm(
  DEATH_EVENT ~  age + anaemia  + creatinine_phosphokinase  + diabetes  + ejection_fraction  + high_blood_pressure  + platelets  + serum_creatinine  + serum_sodium  + sex  + smoking  + time,
  family = "binomial", 
  data = dtset
)

summary(log_reg) #Coefficients Not in exponential form
library(broom)
tidy(log_reg) #Coefficients Not in exponential form

#Coefficients in exponential form
log_reg %>% 
  gtsummary::tbl_regression(exp = TRUE) 

train$DEATH_EVENT<- as.factor(train$DEATH_EVENT)


#For Predicting dependent variable
log_reg = train(
  form =  DEATH_EVENT ~  age + anaemia  + creatinine_phosphokinase  + diabetes  + ejection_fraction  + high_blood_pressure  + platelets  + serum_creatinine  + serum_sodium  + sex  + smoking  + time,
  data = train,
  method = "glm",  
  family = "binomial"
)

dim(train)
pred <- predict(log_reg, test)
pred

#Confusion Matrix
confusionMatrix(pred, as.factor(test$DEATH_EVENT))

#Variables of Importance

vip(log_reg, num_features = 13)

#ROC Curves

log_reg_train <- glm(DEATH_EVENT ~  age + anaemia  + creatinine_phosphokinase  + diabetes  + ejection_fraction  + high_blood_pressure  + platelets  + serum_creatinine  + serum_sodium  + sex  + smoking  + time, data=train, family=binomial)

library(ROCR)

log_reg_test_prob <- log_reg_train %>% predict(test, type = "response")
log_reg_test_prob

preds <- prediction(as.numeric(log_reg_test_prob), test$DEATH_EVENT)

library(ROCit)
## Warning: package 'ROCit' was built under R version 3.5.2
ROCit_obj <- rocit(score=log_reg_test_prob,class=test$DEATH_EVENT)
plot(ROCit_obj)

summary(ROCit_obj)
