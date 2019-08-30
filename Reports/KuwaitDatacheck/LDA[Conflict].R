###We can perform LDA on the Kuwait data set to predict whether or not 
### an individual will rate the government highly based on key indicators like their view of the economic situation etc
##
#we will begin by training a model onto the Kuwait ABV data set, and then see if we can use that model to predict a response on the questions of interset
library(haven)
library(tidyverse)
library(ggthemes)
library(pander)
library(naniar)
library(expss)
library(haven)
cat("\014") 
remove(list = ls()) 
abv <- read_dta("/Users/patrickharned/Google Drive (pharned@princeton.edu)/Data/AB1-5 Merged English Internal_Kuwait.dta")

kuwait5=kuwait5%>%
  filter(wave==5)

vars=c('wave','q2011','q101','q104','q2042','q2043','q2054','q2056','q2057','q209','q2182','q2185','q201a1')
predictors=c('q101','q104','q2042','q2043','q2054','q2056','q2057','q209','q2182','q2185','q201a1')
kuwait5 =subset(select(abv,vars))
t(colnames(kuwait5))


columns <- kuwait5[, 1 : 12]
columns[ columns > 90 ] <- NA
columns[, 1 : 12] <- columns
columns=na.omit(columns)

columns=columns%>%
  mutate(q2011=ifelse(q2011<3,1,0))
columns

glm.fits=glm(q2011~q101+q2043+q2054+q2056+q209+q2182+q2185,
             data=columns,family=binomial )
summary (glm.fits)

##Q2011= 1 I trust government to a great extent
##Q101= economic situation, with 1 being very good and 4 being very bad. 
##q2043Government rating on improving the gap between the rich and the poor
####q2054 - how easy it is to obtain help from the police
###q2056 service provision, registering a busines
###q209 Statement: The government does all it can to provide its citizens with necessary services.
###q2182 Statement: Political leaders are concerned with the needs of ordinary citizens.
###q2185 Statement: Sometimes, politics are so complicated that I cannot understand what is happening.

require(ggplot2)
ggplot(columns, aes(q2182, as.numeric(q2011)-1, color=as.numeric(q101))) +
  stat_smooth(method="loess", formula=y~x,
              alpha=0.2, size=2, aes(fill=q101)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Political Leaders Concerned with Citizens Needs") + ylab("Percent Saying Country is Headed in the Right Direction")

plot(glm.fits)

glm.probs=predict(glm.fits,type="response") #The predict function is used to predict the probability that a citizen approves of the government, given the values of the predictors. type = response tells R to ouput probabilities of the form P(y=1|X).
glm.probs[1:10]

#in order to make a prediction as to whether a citizen will approve of the government, we convert the predicted probabilities(each on being the individiual probability of an observation), into class labels, Approve or Dissaprove
##the following commands create a vector of class predictions based on whether the predicted probability of government approval is greater than or less than .05


glm.pred=rep("Disaprove", 746)
glm.pred[glm.probs>.5]="Approve"

##now we can create a confusion matrix with the table command in order to determine how many observations where correctly classified
attach(columns)
table(glm.pred, q2011)
detach(columns)

##since 1 is approve, our model correctly predicted that 270 observations would approve of the government, and 302 disaprove.
##we have an error rate of 23% so the model is fairly accurate. We need to now create a vector corresponding to wave 3, on which we can test this