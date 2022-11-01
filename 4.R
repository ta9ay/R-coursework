setwd("D:/Documents/UIUC/Assignments/Data stats")
library(readr)

student_dataset<- read_csv("Student Grade Prediction.csv", col_names = TRUE)

head(student_dataset)

dim(student_dataset)

## Ques 1 A

library(corrplot)


str(student_dataset)

# View(student_dataset)

temp<- data.matrix(student_dataset)
temp <- as.data.frame(temp)

View(student_dataset)

corrplot(cor(temp, method = "spearman"), method="square")


## Ques 1 C
dataset2 <- temp[c(1:7,9:27,29:29,31)]
dataset2

model1 <- lm(G1 ~ ., data=dataset2)
model1

#Check VIF
library(DescTools)
VIF(model1)

summary((model1))

## Question 1 D

library(ggfortify)
autoplot(model1)

## Ques 1 E

null = lm(G1 ~ ., data=dataset2)
null

full = lm(G1 ~ ., data=dataset2)
full


#Stepwise Regression
train_Step = step(null, scope = list(upper=full), direction="both")
summary(train_Step)


## Question 2


library(glmnet)

x=model.matrix(G1 ~ ., data=dataset2)
y=dataset2$G1

cv.lasso <- cv.glmnet(x,y, typemeasure="mse", alpha=1)
cv.lasso

ls(cv.lasso)

plot(cv.lasso)

Lambda.best <- cv.lasso$lambda.min

predict(cv.lasso, s = Lambda.best, type = "coefficients")

## OLS

model2 <- lm(G1 ~ sex + address + famsize + Medu + Fjob + reason + traveltime + studytime + failures + schoolsup + famsup + higher + freetime + goout + Dalc + health, data=dataset2)
summary(model2)
