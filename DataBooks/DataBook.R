setwd("/Users/pharned/Documents/Arab-Barometer/DataBooks")
library(list)
source("/Users/pharned/Google Drive/Coding/R Projects/Arab-Barometer-Wraps/Functions.R")
abv = abv_en
`%nin%`=negate(`%in%`)
###grab value labs so we know how to recode


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

exclude = unique(map(names(labeling), function(x){
  if(x%nin%names(abv)){
    print(x)
  }
}))



labs = map(names(abv_en[names(labeling[-exclude])]), function(x) print(val_lab(abv_en[[x]])))%>%
  unique()
values=list()

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

labs_values = as.tibble(labs, values)

recode = function(dataframe, variable){
  print(val_lab(dataframe[[variable]]))
  for (i in seq_along(labs)) {
    if(length(labs[[i]])!=length(val_lab(dataframe[[variable]]))){
      next
    }else{
      if( sum( names(labs[[i]])==names(val_lab( dataframe[[variable]] )))==length( labs[[i]] ) ){
        print("MATCH")
        print(values[i])
        variable = ifelse(dataframe[[variable]]%in%values[[i]], 1, 0)
        return(variable)
      }
    }
  }
}

dataframe_to_recode = abv_en%>%
  filter(!is.na(wt),!is.na(id))

variables_to_recode = names(labeling[-exclude])

dataframe_to_recode[variables_to_recode]

variables = map(names(dataframe_to_recode[variables_to_recode]), ~recode(dataframe = dataframe_to_recode, variable = .x))%>%
  set_names(names(dataframe_to_recode[variables_to_recode]))%>%
  as.data.frame()%>%
  as_tibble()%>%
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


  