

abv_en1=recode_country(abv_en)

recoder = function(x, list){   ###create a function we use to recod variables to get the mean
  x=ifelse(x%in%list,1,0)
}

variable_kist= c("Q421", "Q404","Q521A_3", "Q521A_4","Q218_5")


test1= abv_en1%>%
  mutate(Age= ifelse(Q1001%in%c(18:29), "18-29",ifelse(Q1001%in%c(30:120),"30+", NA)))%>%
  mutate(Education = ifelse(Q1003%in%c(1:4), "Max Secondary", ifelse(Q1003%in%c(5:7), "Max higher", NA)))%>%
  mutate_at(variable_kist[2:5],recoder,c(1,2))%>%
  mutate(Q421=recoder(Q421,c(6)))%>%
  filter(!is.na(wt),!is.na(id),!is.na(Education))


grouping_function=function(dataframe, x=NA, varlist){

  print(x)
  dataframe%>%
    group_by(Country, !!x)%>%
    summarise_at(varlist,list(~weighted.mean(., w=wt, na.rm = TRUE)))%>%
    filter(!is.na(!!x))
}

by_variables = lapply(c("Age", "Education"), sym)


summaries = map(by_variables, ~grouping_function(dataframe = test1, x=.x, varlist = variable_kist))
 
##lets figure out later how to add names to the elements of the list

map(summaries,~ map(variable_kist, ~ plotterizer(dataframe = .y,x=.x, fill = "Age", pallette = "Age"),.y=.x))####wor


Age_plots = map(variable_kist, ~ plotterizer(dataframe = summaries[[1]],x=.x, fill = "Age", pallette = "Age"))%>%
  set_names(names(abv_en[variable_kist]))####wor

Education_plots = map(variable_kist, ~ plotterizer(dataframe = summaries[[2]],x=.x, fill = "Education", pallette = "Education"))%>%
  set_names(names(abv_en[variable_kist]))####wor


country_grouping_function=function(dataframe, varlist){
  
  dataframe%>%
    group_by(Country)%>%
    summarise_at(varlist,list(~weighted.mean(., w=wt, na.rm = TRUE)))
}

test2= test1%>%
  filter(Q409%in%c(1:5))

media_usage_list = unlist(find_variable("media usage", abv_en)[6:12])
media_usage_list = map_chr(media_usage_list, paste)%>%
  set_names(nm=NULL)

media_usage_summaries = country_grouping_function(dataframe = test2, varlist = media_usage_list)

media_usage_plots = map(media_usage_list, ~ plotterizer(dataframe = media_usage_summaries,x=.x))%>%
  set_names(nm=names(media_usage_summaries[media_usage_list]))####wor


trend_graphs = trend_data%>%
  mutate_at(c("q2185", "q404"),recoder,c(1,2))%>%
  mutate(q409=recoder(q409,c(1:3)))%>%
  mutate_at(c("q2185", "q404","q409"),function(x){x*trend_data$wt})%>%
  group_by(wave)%>%
  summarise_at(c("q2185", "q404","q409"), list(~mean(., na.rm=TRUE)))



