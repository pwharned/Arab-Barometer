setwd("/Users/pharned/Documents/Arab-Barometer/DataBooks")
library(list)
source('~/Documents/Arab-Barometer/Wraps/Functions.R')
abv = abv_en




##Q104B requires special coding.
find_variable("migration: destination", abv_en)


abv$Q104B_GCC = mutate(abv_en, ifelse(Q104B_KSA+Q104B_UAE+Q104B_QA+Q104B_BA+Q104B_KU+Q104B_OM%in%c(1:7), 1, 0))
abv$Q104B_MENA =  mutate(abv_en, ifelse(Q104B_EG+Q104B_JO+Q104B_LEB+Q104B_MO+Q104B_AL+Q104B_TUN+Q104B_TUR%in%c(1:7), 1, 0))
abv$Q104B_EU =  mutate(abv_en, ifelse(Q104B_EEU+Q104B_FR+Q104B_GER+Q104B_ESP+Q104B_IT+Q104B_WEUOTHER+Q104B_WEU+Q104B_UK%in%c(1:7), 1, 0))
abv$Q104B_US =  mutate(abv_en, ifelse(Q104B_US%in%c(1:7), 1, 0))
abv$Q104B_CAN = mutate(abv_en, ifelse(Q104B_CAN%in%c(1:7), 1, 0))

  

##Q301
abv$Q301B=ifelse(abv$Q301B%in%c(4,5),1,0)
abv$Q301A =  ifelse(abv$Q301A==1, 1, 0)
abv$Q301 = ifelse(abv$splita==1, abv$Q301A, abv$Q301B)

##Q860
source("/Users/pharned/Documents/Arab-Barometer/DataBooks/Q860.R")

exclude = unlist(unique(map(names(labeling), function(x){
  if(x%nin%names(abv)){
    return(x)
  }
})))

Subset =names(labeling)[names(labeling)%nin%exclude]


labs = map(names(abv[Subset]), function(x) val_lab(abv[[x]]))%>%
  unique()
values=list()
labs[[8]]=NULL ##remove a large element from the labs list

for (i in seq_along(abv)) {
  print(names(abv)[[i]])
  print(val_lab(abv[[i]]))
  
}

for (i in seq_along(labs)){
  print(labs[[i]])
  value = map_int(c(1:5),function(x) (x = as.integer(readline(prompt = "Enter a list of values you want to recode for this val_lab  "))))
  value = na.omit(value)
  print(value)
  if(length(value)==0){
    values[[i]]=NULL
  }else {
    values[[i]]=value
  }
}



binarize = function(dataframe, Variable){
  variable_name = names(dataframe[Variable])
  Variable =dataframe[[Variable]]
  val_lab_variable = val_lab(Variable)
    temp_lab=paste(deparse(val_lab(Variable)), collapse = "")
    for(j in seq_along(labs)){
      if(paste(deparse(labs[[j]]), collapse = "")==temp_lab){
        if(is.null(values[[j]])==TRUE){
          Variable = ifelse(Variable%in%c(95:99), 0, Variable)
          variable_list = c()
          for (i in val_lab_variable[val_lab_variable%nin%c(0,90:99)]){
            Variable =ifelse(Variable%in%c(i),1,0)
            assign(paste(variable_name, i, sep = "_"),Variable)
          }
          return(unique(variable_list))
          print(variable_list)
        }else{
          Variable = ifelse(Variable%in%values[[j]], 1, 0)
          names(Variable)=variable_name
          return(Variable)
        }
      }else{
        next
      }
    }
  }



binarize(abv, "Q7141A")
dataframe_to_recode = abv%>%
  filter(!is.na(wt),!is.na(id))

variables_to_recode = Subset

dataframe_to_recode[variables_to_recode]

Variables =map(Subset, ~binarize(dataframe = dataframe_to_recode, Variable  = .x))%>%
  set_names(Subset)
  
Variables%>%
  unique()
    
variables%>%
  as_tibble()
  cbind(dataframe_to_recode[c("id", "country", "wt")])%>%
  select(country, id, wt, everything())%>%
  recode_country()

country_grouping_function=function(dataframe, varlist){
  
  dataframe%>%
    group_by(Country)%>%
    summarise_at(varlist,list(~weighted.mean(., w=wt, na.rm = TRUE)))
}


new_frame = country_grouping_function(variables, variables_to_recode)
overall_plots = map(variables_to_recode, ~plotterizer(dataframe = new_frame, x=.x))%>%
  set_names(paste(variables_to_recode, "overall",sep = "_"))


  