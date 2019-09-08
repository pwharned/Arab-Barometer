setwd("/Users/pharned/Documents/Arab-Barometer/DataBooks")

source("/Users/pharned/Google Drive/Coding/R Projects/Arab-Barometer-Wraps/Functions.R")

`%nin%`=negate(`%in%`)
###grab value labs so we know how to recode
for (i in seq_along(names(labeling))){
  if(names(labeling)[[i]] %nin% names(abv_en)){
    print(i)
  }
}
exclude = c(13,43,44,72,79,92,102,133,134)

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

labs_values = cbind(labs, values)

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


  