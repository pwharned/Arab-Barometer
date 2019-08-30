
After loading the data, I have decided to start with all the variables in the state of the economy, institutional trust, and government performance sectins. 

knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggthemes)
library(pander)
library(expss)
library(haven)
cat("\014") 
remove(list = ls()) 
abv <- read_dta("/Volumes/GoogleDrive/My Drive/Data/ABI_ABV_Trend_BBC.dta")
#first we need to subset the data set. we will create a list object containing all of the variables we want to work with. 


ab2 = abv%>%
  filter(country==5, wave==3|wave==5)


###create a subsetting function
filtered_variable=function(data,filter_variable,value){
  filter_variable<-enquo(filter_variable)
  data %>%
    mutate(variable=as.numeric(ifelse(!!filter_variable==value,1,0)))%>%
    group_by(wave)%>%
    dplyr::summarise(question=round(mean(variable,na.rm=TRUE)*100))
  
}


ab3=filtered_variable(ab2, q609, 1)




###create a plotting function
plot1 = function(dtaf,var1, subtitle){
  use_labels(dtaf,{
    ggplot(data = dtaf, aes(x=wave, y=var1)) + 
      geom_point(stat = "identity", color ='orange', fill='orange')+ 
      geom_text(aes(label = var1),nudge_y = 2.5)+
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      ggtitle(var_lab(var1), subtitle = subtitle)+ ###notice how here we call Var_lab(var1) inside the function to import the variable label as the title of the graph
      ylab("Percent")+
      ylim(0,100)+
      xlim(2, 5)+
      stat_smooth(method = 'lm', se = FALSE)+
    theme(panel.background = element_blank())+
      theme(plot.title = element_text(hjust=0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.text.y=element_text(angle=45, hjust=1))
  })}




plot1(ab3, ab3$question, "`subtitle")



