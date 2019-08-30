
library(tidyverse)
library(ggthemes)
library(pander)
library(expss)
library(haven)
cat("\014") 
remove(list = ls()) 
abv <- read_dta("/Volumes/GoogleDrive/My Drive/Data/AB1-5 Merged English Internal_Kuwait.dta")

function1 =function(variable){
  variable = enquo(variable)  
  abv%>%
       group_by(wave)%>%
       summarise(variable = mean(!!variable))
  }
function1(q100)

for (col in 1:50){
    temp=abv[,col]
     table=function1(temp)
     print(table)
}
ab3 = abv%>% group_by(wave)%>%
  summarise_each(funs(mean))

ab3

ab1=ab3%>%
  select_if(~!any(is.na(.)))

ls(ab1)
mylist1=ls(ab1)
mylist = enquo(mylist)
print(mylist)

for (i in mylist1){
  temp =enquo(i)
  mylist2=list()
  mylist2[[temp]]=temp
  mylist
}
mylist

apply(abv, 2, function1)

attach(abv)
mylist =(q1001,q1002,q1003,q101,q1010,q1016,q102,q103,q104,q105,q2011,q2013,q2014,q2017,q2042,q2043,q2054,q210,q213,q214,q216,q2185,q301, q302,q404, q409, q511, q512, q5161, q5162, q5163, q5164, q5211, q5214, q5215, q6013, q6014, q7001, q7002,q7003,q7004,q7008,q7009, q703, q705,q707)
sapp\