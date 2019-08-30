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
abv <- read_dta("/Volumes/GoogleDrive/My Drive/Data/AB1-5 Merged English Internal_Kuwait.dta")
search()###check the list of attached packages and data sets
kuwait5=abv%>%
  filter(wave==5)

predictors=c('wave','q2011','q101','q104','q2042','q2043','q2054','q2185')
kuwait5 =subset(select(kuwait5,predictors))
t(colnames(kuwait5))

####Removing nas
columns <- kuwait5[, 1 : 8]
columns[ columns > 90 ] <- NA
columns[, 1 : 8] <- columns
kuwait5_clean=na.omit(columns)

kuwait5_clean=kuwait5_clean%>% ####codes response 1 and 2 as 1, and the rest as 0, to prepare for classification
  mutate(q2011=ifelse(q2011<3,"Trust","Don't Trust"))
kuwait5_clean$q2011=as.factor(kuwait5_clean$q2011)

glm.fits_5=glm(q2011~q101+q104+q2043+q2185, ###fit a classifcation model, with q2011 as the 
               data=kuwait5_clean,family=binomial)
summary (glm.fits_5)


contrasts(kuwait5_clean$q2011)

glm.probs=predict(glm.fits_5,type="response")
glm.probs[1:10]



glm.pred=rep("Don't Trust" ,1143)

glm.pred[glm.probs >.5]=" Trust"

table(glm.pred ,kuwait5_clean$q2011)
mean(glm.pred==kuwait5_clean$q2011 )



####Kuwait 3

kuwait3=abv%>%
  filter(wave==3)

predictors=c('wave','q2011','q101','q104','q2042','q2043','q2054','q2185')
kuwait3 =subset(select(kuwait3,predictors))
t(colnames(kuwait3))

####Removing nas
columns <- kuwait3[, 1 : 8]
columns[ columns > 90 ] <- NA
columns[, 1 : 8] <- columns
kuwait3_clean=na.omit(columns)

kuwait3_clean=kuwait3_clean%>% ####codes response 1 and 2 as 1, and the rest as 0, to prepare for classification
  mutate(q2011=ifelse(q2011<3,"Trust","Don't Trust"))
kuwait3_clean$q2011=as.factor(kuwait3_clean$q2011)

glm.fits_3=glm(q2011~q101+q104+q2043+q2185, ###fit a classifcation model, with q2011 as the 
             data=kuwait3_clean,family=binomial)
summary (glm.fits_3)


contrasts(kuwait3_clean$q2011)

glm.probs_3=predict(glm.fits_3,type="response")
glm.probs[1:10]


glm.pred=rep("Don't Trust" ,1007)
glm.pred[glm.probs_3 >.5]=" Trust"

table(glm.pred ,kuwait3_clean$q2011)

mean(glm.pred==kuwait3_clean$q2011 )



###In order to test the model, we need first to fit the model onto the subset of the wave 5 data, then test the model against the wave 3 data


glm.fits_train=glm(q2011~q101+q104+q2043+q2185, ###fit a classifcation model, with q2011 as the 
               data=kuwait5_clean,family=binomial)
glm.probs=predict(glm.fits_train,kuwait3_clean, type="response")

glm.pred=rep("Don't Trust",1007)
glm.pred[glm.probs >.5]="Trust"

table(glm.pred,kuwait3_clean$q2011)
mean(glm.pred==kuwait3_clean$q2011)










###We want to compare this difference against other countries




ABI_ABV_Trend_BBC <- read_dta("/Volumes/GoogleDrive/My Drive/Data/ABI_ABV_Trend_BBC.dta")
remove(list = ls()) 


classification_function= function(dataset){
  predictors=c('wave','q2011','q101','q104','q2042','q2043','q2054','q2185')
  
  dataset=abv
  dataset_predictors =subset(select(dataset,predictors))##subset based on predictors

  train=(dataset_predictors$wave==5)
  test=(dataset_predictors$wave==3)
  
  dataset_test=dataset_predictors[test,]
  dataset_train=dataset_predictors[train,]
 
   ####Removing nas from test
  columns <- dataset_test[, 1 : 8]
  columns[ columns > 90 ] <- NA
  columns[, 1 : 8] <- columns
  dataset_test=na.omit(columns)


  dataset_test=dataset_test%>% ####codes response 1 and 2 as 1, and the rest as 0, to prepare for classification
    mutate(q2011=ifelse(q2011<2,"Trust","Don't Trust"))
  dataset_test$q2011=as.factor(dataset_test$q2011)
  ###train
  columns <- dataset_train[, 1 : 8]
  columns[ columns > 90 ] <- NA
  columns[, 1 : 8] <- columns
  dataset_train=na.omit(columns)
  
  q2011_test = dataset_test$q2011

  dataset_train=dataset_train%>% ####codes response 1 and 2 as 1, and the rest as 0, to prepare for classification
    mutate(q2011=ifelse(q2011<2,"Trust","Don't Trust"))
  dataset_train$q2011=as.factor(dataset_train$q2011)
  

  
  
  glm.fits_train=glm(dataset_train$q2011~q101+q104+q2043+q2185, ###fit a classifcation model, with q2011 as the 
                     data=dataset_train,family=binomial)
  
  glm.probs=predict(glm.fits_train, dataset_test, type="response")
  
  longness=length(glm.probs)
  attach(dataset_test)
  glm.pred=rep("Don't Trust",longness)
  glm.pred[glm.probs >.5]="Trust"
  print(mean(glm.pred==q2011_test))
  detach(dataset_test)
  
}


classification_function(abv)

