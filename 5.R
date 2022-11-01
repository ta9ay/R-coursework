#Setup Working Directory

setwd("D:/Documents/UIUC/Assignments/Data stats")

#Read in Data

library(readr)

dataset<- read_csv("BIG5.csv", col_names = TRUE)
Dt<-dataset

head(Dt)
dim(Dt)
#19719 Observations 50 variables
sum(is.na(Dt))
#no missing values

#to view all columns
str(Dt, list.len=ncol(Dt))

library(psych)


# Check for Correlation
options("scipen"=100, "digits"=3)

round(cor(Dt), 2)
MCorrTest = corr.test(Dt, adjust="none")
MCorrTest

MCT = MCorrTest$p
MCT
round(MCT,2)

# Now, for each element, see if it is < .01 (or whatever significance) and set the entry to true = significant or false
MTest = ifelse(MCT < .01, T, F)
MTest

# Now lets see how many significant correlations there are for each variable.  We can do this by summing the columns of the matrix
colSums(MTest) - 1  # We have to subtract 1 for the diagonal elements (self-correlation)


#Create PCA
P = prcomp(Dt,center=T, scale=TRUE)
P

#Scree Plots

#Knee Method
library(factoextra)
fviz_eig(P)
# 5 components

#Kaiser-Meyer-Olkin eigenvalue>1
plot(P)
abline(1, 0)
# 8 components determined

P0<- psych::principal(Dt,rotate = "varimax", nfactors = 6, scores = TRUE)
P0
print(P0$loadings, cutoff=0.4, sort=T)

#PCA Model #1
P1<- psych::principal(Dt,rotate = "varimax", nfactors = 5, scores = TRUE)
P1

print(P1$loadings, cutoff=0.4, sort=T)

#Discard N4 and 09
names(Dt)
temp_D<-Dt[,c(1:13, 15:48,50)]
names(temp_D)

#PCA Model #2
P2<- psych::principal(temp_D,rotate = "varimax", nfactors = 5, scores = TRUE)
P2

print(P2$loadings, cutoff=0.4, sort=T)



#Calculating scores

scores <- P1$scores

cor(scores)

summary(scores)



#Conducting Factor Analysis

fit = factanal(Dt,5)
print(fit$loadings, cutoff=.4, sort=T)

names(Dt)
temp1<-Dt[,c(1:13, 15:29,31:48,50)]
names(temp1)

fit = factanal(temp1, 5)
print(fit$loadings, cutoff=.38, sort=T)

