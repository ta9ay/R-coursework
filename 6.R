setwd('D:/Documents/UIUC/Assignments/Data stats')

responses <- read.csv(file="Young People Survey for R Lab - Responses.csv", header=TRUE, sep=",")
head(responses)
View(responses)

dim(responses)
names(responses)

#Check for Missing Values (i.e. NAs)

#For All Variables
sum(is.na(responses))
#571 total missing values (571 cells with missing data)


#Treat Missing Values

#Listwise Deletion
responses2 <- na.omit(responses)

#Check new data has no missing data
sum(is.na(responses2))

hobbies_interests = responses2[, 1:3]
spending = responses2[,4:8]