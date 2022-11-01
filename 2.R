
install.packages("readr")

#Setup Working Directory

setwd("D:/Documents/UIUC/Assignments/Data stats")

#Read in Data

library(readr)

student_dataset<- read_csv("StudentGradePrediction.csv", col_names = TRUE)


#Check data is read in correctly
View(student_dataset)

head(student_dataset)


#Check Dimensions of Dataset
dim(student_dataset)
#395 x 33


########################################################
#Wilcoxon Ranked Test

#Question 1

library(plyr)
library(RVAideMemoire)
byf.shapiro(as.matrix(student_dataset$G3)~romantic,data=student_dataset)
wilcox.test(student_dataset$G3~student_dataset$romantic)


##############################################################################################

#Question 2
shapiro.test(student_dataset$G1)
shapiro.test(student_dataset$G3)

wilcox.test(student_dataset$G3,student_dataset$G1,paired=TRUE)

##############################################################################################

#Question 3

library(gmodels)

student_dataset$sex_categorical <- revalue(student_dataset$sex, c("M"="1", "F"="0"))
student_dataset$sex_categorical <- as.numeric(student_dataset$sex_categorical)

shapiro.test(student_dataset$sex_categorical)

student_dataset$highered_categorical <- revalue(student_dataset$higher, c("yes"="1", "no"="0"))
student_dataset$highered_categorical <- as.numeric(student_dataset$highered_categorical)

shapiro.test(student_dataset$highered_categorical)

CrossTable(student_dataset$sex,student_dataset$higher,digits = 2, expected=TRUE, prop.r=TRUE, 
           prop.c = TRUE, prop.chisq = FALSE, chisq = TRUE, fisher = TRUE, format="SPSS")


##############################################################################################

#Question 4



student_dataset$G1_cat <-cut(student_dataset$G1, c(0,15,100))
table(student_dataset$G1_cat)

student_dataset$G3_cat <-cut(student_dataset$G3, c(0,15,100))
table(student_dataset$G3_cat)

library(gmodels)

CrossTable(student_dataset$G1_cat,student_dataset$G3_cat,digits = 2, prop.r=TRUE, 
           prop.c = TRUE, format="SPSS")


mcnemar.test(student_dataset$G1_cat,student_dataset$G3_cat)

##############################################################################################

#Qestion 5


p_extrasup <- prop.test(x = 51, n = 395, p = 0.50, 
                        correct = FALSE)
p_extrasup

##############################################################################################

#Question 6


table(student_dataset$sex, student_dataset$activities)

prop.test(x = c(96, 105), n = c(208, 187), correct = FALSE)



