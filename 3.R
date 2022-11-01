setwd("D:/Documents/UIUC/Assignments/Data stats")

library(readr)

student_dataset<- read_csv("Student Grade Prediction.csv", col_names = TRUE)

head(student_dataset)

dim(student_dataset)

#Question 1

shapiro.test(student_dataset$G1)
shapiro.test(student_dataset$studytime)
# not normal

cor.test(student_dataset$G1, student_dataset$studytime, alternative = "two.sided", method = "spearman", conf.level = 0.95)
# p-value is 0.00121


# Question 2
shapiro.test(student_dataset$age)
shapiro.test(student_dataset$absences)

cor.test(student_dataset$age, student_dataset$absences, alternative = "two.sided", method = "spearman", conf.level = 0.95)
# p value is 0.0029


#Question 3

library(corrplot)
which( colnames(student_dataset)=="famrel" )
which( colnames(student_dataset)=="absences" )
m <- cor(x = student_dataset$G3, y = student_dataset[24:30], method="spearman")
corrplot(m,method="number",col = gray.colors(100))

#Question 4

library(ggplot2)

my_graph <- ggplot(student_dataset, aes(x = Dalc, y = G1)) + geom_point(aes(color = sex)) + 
  stat_smooth(method = "lm", col = "#9b21c4",se = FALSE, size = 2)
my_graph
