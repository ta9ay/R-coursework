dt = read.csv("Young People Survey for R Lab - Responses.csv", header = TRUE, sep = ",")
View(dt)

sum(is.na(dt))

dt2<-na.omit(dt)

sum(is.na(dset2))

names(dt2)
hobbies_interests <- dt2[,c(32:63)]
spending <- dt2[,c(134:140)]

library(yacca)
c=cca(hobbies_interests,spending)
c

ls(c)

c$chisq
c$df
summary(c)

helio.plot(c, cv=3)
