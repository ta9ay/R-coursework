install.packages("visualize")
library(visualize)

######## Ques 5 ########
visualize.norm(stat=-1.35,mu=0,sd=1,section="lower")
visualize.norm(stat=1.48,mu=0,sd=1,section="upper")
visualize.norm(stat=c(-0.4,1.5),mu=0,sd=1,section="bounded")
visualize.norm(stat=c(-2,2),mu=0,sd=1,section="tails")


######## Ques 6 ########
# Men 30-40: 
mean_men=4313
sd_men=583 

# Women 30-34: 
mean_women=5261 
sd_women=807

Z_score_Leo<-(4948-mean_men)/sd_men
Z_score_Leo

Z_score_Mary<-(5513-mean_women)/sd_women
Z_score_Mary


pnorm(Z_score_Leo,lower.tail=FALSE)
pnorm(Z_score_Mary,lower.tail=FALSE)
