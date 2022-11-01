library(MASS)
library(psych)
setwd("D:/Documents/UIUC/Assignments/Data stats")
dt =  read.csv("Heart Failure.csv", sep = ",")
View(dt)
str(dt)

dt$DEATH_EVENT <- as.factor(dt$DEATH_EVENT)

#With Cross Validation
# The dependent variable must be categorical
dtLDA <- lda(DEATH_EVENT ~ ., data=dt, CV=TRUE)
# dtLDA
dtLDA

table_cv <- table(dt$DEATH_EVENT, dtLDA$class)
sum(diag(table_cv)/sum(table_cv))
#82.9%

dtLDA2 <- lda(DEATH_EVENT ~ ., data=dt)
# dtLDA2

plot(dtLDA2, xlab = "LD1", ylab = "LD2")

#without CV
p <- predict(dtLDA2, data=dt[,1:12])$class
p

table1<-table(p, dt$DEATH_EVENT)
table1

sum(diag(table1)/sum(table1))
# 85.28%

mean(p== dt$DEATH_EVENT)
# 0.8528%


# Using training and testing sets

library(caTools)
set.seed(111)

sample = sample.split(dt,SplitRatio = 0.70)
train =subset(dt,sample ==TRUE)
test=subset(dt, sample==FALSE)


dtLDA = lda(DEATH_EVENT ~ ., data=train)
dtLDA

plot(dtLDA)

prd<-predict(dtLDA, test)$class

table2 <- table(prd, test$DEATH_EVENT)
table2
sum(diag(table2)/sum(table2))
# 89.13%



# Using training and testing with CV
dtLDA2 = lda(heartdisease ~ ., data=train, CV=FALSE)


coef(dtLDA)
coef(dtLDA2)


library(caret)

modelFit<- train(DEATH_EVENT ~ ., method='lda',preProcess=c('scale', 'center'), data=train)


confusionMatrix(test$DEATH_EVENT, predict(modelFit, test))

###########
###########

library(fpc)
library(dbscan)

setwd("D:/Documents/UIUC/Assignments/Data stats/p")
data <- read.csv("CommunitiesDataSetDeletedCols.csv", sep = ",")

str(data)
data<-data[,4:100]
data$OtherPerCap <- as.numeric(data$OtherPerCap)
data<- na.omit(data)

names(data)
data2<- data[,c(22:27,97)]

str(data2)

set.seed(123)
data_db <- fpc::dbscan(data2, eps = 1, MinPts = 5)
data_db

library("factoextra")
fviz_cluster(data_db, data = data, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())
