library(haven)
library(tidyverse)
library(ggthemes)
library(pander)
library(naniar)
library(expss)
library(haven)
remove(list = ls()) 

abv <- read_dta("/Volumes/GoogleDrive/My Drive/Data/ABI_ABV_Trend_BBC.dta")

countries = unique(c(abv$country))####create a vector of unique countries in the data set

countries_function =function(integer){
  abv%>%
    filter(country == integer)
}
countries = lapply(countries, countries_function)



        



classification_function= function(dataset){
  predictors=c('wave','q2011','q101','q104','q2042','q2043','q2054','q2185')
  
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
  mean = (mean(glm.pred==q2011_test))
  detach(dataset_test)
}

classification_function(countries[[2]])

means= lapply(countries, classification_function)

plot1_frame=tibble("country"= c("yemen","Iraq", "Jordan", "Lebanon", "Libya", "Morocco", "Palestine", "Tunisia", "Sudan", "Kuwait"), mean=c(67,97,88,78,97,96,61,83,85,60))
ggplot(data = plot1_frame, aes(x=reorder(country,-mean), y=mean))+
  geom_histogram(stat = "identity")
