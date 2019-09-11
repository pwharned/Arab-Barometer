
source('~/Documents/Arab-Barometer/Wraps/Functions.R')
setwd("/Users/pharned/Documents/Arab-Barometer/Reports/Social Media")
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

###map(summaries,~ map(variable_kist, ~ plotterizer(dataframe = .y,x=.x, fill = "Age", pallette = "Age"),.y=.x))####wor


Age_plots = map(variable_kist, ~ plotterizer(dataframe = summaries[[1]],x=.x, fill = "Age", pallette = "Age"))%>%
  set_names(paste(names(abv_en[variable_kist]),"age",sep = "_"))####wor

Education_plots = map(variable_kist, ~ plotterizer(dataframe = summaries[[2]],x=.x, fill = "Education", pallette = "Education"))%>%
  set_names(paste(names(abv_en[variable_kist]), "education",sep = "_"))####wor

country_grouping_function=function(dataframe, varlist){
  
  dataframe%>%
    group_by(Country)%>%
    summarise_at(varlist,list(~weighted.mean(., w=wt, na.rm = TRUE)))
}

overall_summaries = country_grouping_function(test1, variable_kist)
overall_plots = map(variable_kist, ~plotterizer(dataframe = overall_summaries, x=.x))%>%
  set_names(paste(names(abv_en[variable_kist]), "overall",sep = "_"))


test2= test1%>%
  filter(Q409%in%c(1:5))

media_usage_list = unlist(find_variable("media usage", abv_en)[6:12])
media_usage_list = map_chr(media_usage_list, paste)%>%
  set_names(nm=NULL)

media_usage_summaries = country_grouping_function(dataframe = test2, varlist = media_usage_list)

media_usage_plots = map(media_usage_list, ~ plotterizer(dataframe = media_usage_summaries,x=.x))%>%
  set_names(nm=paste(names(media_usage_summaries[media_usage_list]), "overall",sep = "_"))####wor

trend_graphs = trend_data%>%
  mutate(wave_Na=ifelse(wave%in%c(4:5)&is.na(wt)==TRUE,TRUE,FALSE))%>%
  filter(wave_Na==FALSE)%>%
  mutate_at(c("q2185", "q404"),recoder,c(1,2))%>%
  mutate(q409=ifelse(q409%in%c(1:3)&wave%in%c(1:3),1,ifelse(q409%in%c(1:4)&wave%in%c(4),1,ifelse(q409%in%c(1:5)&wave%in%c(5),1,0))))%>%
  group_by(wave)%>%
  rename_at(c("q2185", "q404","q409"), toupper)%>%
  rename(Q218_5 = Q2185)%>%
  summarise_at(c("Q218_5", "Q404","Q409"),list(~weighted.mean(., w=wt, na.rm = TRUE)))%>%
  mutate(Year = as.factor(c("2006", "2010-11","2012-14", "2016-17","2018-19")))
  
trend_plots = map(c("Q218_5", "Q404","Q409"), ~trend_graph_function(dataframe =trend_graphs, x="wave", y=.x, pallette="light blue 4"))%>%
  set_names(paste(c("Q218_5", "Q404","Q409"),"trend", sep = "_"))

plot.bind = list(trend_plots, Education_plots, Age_plots, media_usage_plots, overall_plots)%>%
  flatten()


setwd("/Users/pharned/Documents/Arab-Barometer/Reports/Social Media/Plots")

for (i in seq_along(plot.bind)) {
  plot = plot.bind[[i]]
  ggsave(filename = paste(names(plot.bind)[[i]], ".png",sep = "_"), plot = plot, width =7, height = 7, device = "png",path = paste(getwd()))
  

}


    