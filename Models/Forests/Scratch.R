<<<<<<< HEAD
###Model Syntax for machine learning on Arab Barometer Data Wave 5

library(haven)
remove(list = ls()) 

abv1= read_dta("/Volumes/GoogleDrive/Shared drives/Arab Barometer/AB5/Data/Release Data/ABV_Crossectional_Data_Release_ENG.dta")

#getting a subset of variables will be tricky
pathname="/Users/patrickharned/Google Drive(pwharned@gmail)/Coding/R Projects/Captions/Captions and Titles.xlsx"
library(readxl)
labeling = read_excel(pathname, sheet = 4) ##import graph titles from excel sheet
labeling_names =names(labeling)
library(tidyverse)

labeling_names=c()
for (name in names(labeling)){
  labeling_names[1]=name
}

library(expss)
lab_list= list()
data= data%>%
  select(one_of(labeling_names))
lab_list= list()
for (i in seq_along(data)){
  name = names(data)[i]
  lab= val_lab(data[name])
  lab_list[[i]]=lab
  
}

recoding_functioner = function(dataframe){
  function(variable)
    for (i in seq_along(dataframe)) {
      if(starts_with(deparse(var_lab(variable),as.character(dataframe[i,2]))))
        variable= ifelse(variable%in%c(dataframe[2,2]))
      else{
        print(paste("Could not recode",variable,sep = ":"))
      }
    }
}

recoding_subtitles = read_excel(pathname, sheet = 5)##import subtitles and recoding variables 
recoding_1_function = recoding_functioner(recoding_subtitles[1:10,2:3])
recoding_1_2_function= recoding_functioner(recoding_subtitles[17:36,2:3])
recoding_3_4_function=recoding_functioner(recoding_subtitles[37:39,2:3])
recoding_5_function= recoding_functioner(recoding_subtitles[14:16,2:3])
recoding_1_greater_function =recoding_functioner(recoding_subtitles[13,2:3])
recoding_4_function=recoding_functioner(recoding_subtitles[12,2:3])
recoding_2_function= recoding_functioner(recoding_subtitles[11,2:3])

variable_list = c("Q100","Q2061A","Q101","Q101A","Q102","Q513","country")


data1= abv1%>%
  select(variable_list)%>%
  as.data.frame()

data1[is.na(data1)]=0

##we need a set of functions for vectorizing data. 
glimpse(data)

##We have 352 Variables. Some of these variabels are country dependent, meaning we get a lot of NA observations. We need a way to get this down to a manageable set of variables

nested_data = data1%>%
  group_by(country)%>%
  nest()


nested_data%>%
  mutate(model= map(data,~lm(formula = Q513~., data=.x)))




###Model Syntaxfor machine learning on Arab Barometer Data Wave 5
