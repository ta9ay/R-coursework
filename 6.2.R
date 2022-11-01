setwd("D:/Documents/UIUC/Assignments/Data stats")

dt = read.csv("CommunitiesDataSetDeletedCols.csv", sep = ",")
names(dt)

employment <- dt[,36:41]
education <- dt[,33:35]

names(employment)
names(education)

library(yacca)
sum(is.na(dt))

c=cca(education,employment)
c

ls(c)

c$chisq
c$df
summary(c)

helio.plot(c, cv=3)
