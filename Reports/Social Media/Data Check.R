library(srvyr)
library(tidyverse)
library(rvest)

abv_en1=recode_country(abv_en)

recoder = function(x, list){   ###create a function we use to recod variables to get the mean
  x=ifelse(x%in%list,1,0)
}

rounder = compose(
  round,
  function(x){x*100},
  partial(weighted.mean, w=wt, na.rm = TRUE)
  
)

test1= abv_en1%>%
  mutate(Q404=recoder(Q404,c(1,2)), Q421=recoder(Q421,c(6)))%>%
  mutate(Age= ifelse(Q1001%in%c(18:29), "18-29",ifelse(Q1001%in%c(30:120),"30+", NA)))%>%
  mutate(Education = ifelse(Q1003%in%c(1:4), "Max Secondary", ifelse(Q1003%in%c(6,7), "Max higher", NA)))%>%
  filter(!is.na(wt),!is.na(id))%>%
  group_by(Country)%>%
  summarise_at(c("Q404","Q421"),~rounder)
 


plotterizer = function(dataframe, x, fill = NA,pallette=NA, color=NA){
  fill=dataframe[[fill]]
  legend_title=names(dataframe)[[2]]
  title1 = title_function(x)
  subtitle = subtitle_function(x)
  variable = dataframe[[x]]
  plot = ggplot(dataframe, aes(reorder(Country,variable), round(variable*100), fill=fill))+
    geom_bar(stat = "identity",position = position_dodge(width = 1), width = .8, fill = color)+
    coord_flip()+
    geom_text(aes(label=round(variable*100), hjust = -.5),size=2.6, position = position_dodge(width = 1))+
    theme_bw()+
    theme(legend.position = "bottom", axis.text.x = element_text(size = 10),
          plot.caption = element_text(size = 11, hjust = 0),  
          plot.title = element_text(size=14, hjust=0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"),
          axis.text.y = element_text(angle = 45, size = 12),
          text = element_text(family = "Arial"),
          panel.border = element_rect(colour = "black", fill=NA, size=.6),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())+ylim(0,100)+
    labs(caption = "Notes: Weighted Estimates.\nSource: Arab Barometer, Wave 5", fill=legend_title)+
    xlab("")+ 
    ylab("Percent")+ggtitle(title1, subtitle = subtitle)
  if(is.na(pallette==TRUE)){
    return(plot)
  }else{
    plot=plot+scale_fill_ab(pallette)
    return(plot)
  }
  
}

variable_kist= c("Q421", "Q404")

variable_kist = lapply(variable_kist,sym)

grouping_function=function(dataframe, x=NA, varlist){

  print(x)
  dataframe%>%
    group_by(Country, !!x)%>%
    summarise_at(varlist,list(~weighted.mean(., w=wt, na.rm = TRUE)))%>%
    filter(!is.na(!!x))
}

by_variables = lapply(c("Age", "Education"), sym)


summaries = map(by_variables, ~grouping_function(dataframe = test1, x=.x))
 
##lets figure out later how to add names to the elements of the list

map(summaries,~ map(variable_kist, ~ plotterizer(dataframe = .y,x=.x, fill = "Age", pallette = "mix"),.y=.x))####wor


map(variable_kist, ~ plotterizer(dataframe = summaries[[1]],x=.x, fill = "Age", pallette = "mix"))####wor

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

media_usage_plots = map(media_usage_list, ~ plotterizer(dataframe = media_usage_summaries,x=.x, color = country_color))%>%
  set_names(nm=names(media_usage_summaries[media_usage_list]))####wor




