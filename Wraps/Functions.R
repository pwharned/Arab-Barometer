
library(expss)
library(haven)
library(readxl)
library(tidyverse)
library(ggrepel)
library(extrafont)

if(startsWith(getwd(),"C")==TRUE){
  source("C:/Users/Patrick Harned/Google Drive/Coding/R Projects/Arab-Barometer-Wraps/AB_Colors.R")
  labeling = read_xlsx("C:/Users/Patrick Harned/Google Drive/Coding/R Projects/Captions/Captions and Titles.xlsx", sheet=4)
  abv <- read_dta("G:/Shared drives/Arab Barometer/AB5/Data/Release Data/ABV_Crosssectional_Data_Release_ARA.dta")
  trend_data <- read_dta("G:/Shared drives/Arab Barometer/AB1-4/ABI_ABV_Trend_File.dta")
  }else{
    source("/Users/pharned/Google Drive/Coding/R Projects/Arab-Barometer-Wraps/AB_Colors.R")
    labeling=read_xlsx("/Users/pharned/Google Drive/Coding/R Projects/Captions/Captions and Titles.xlsx", sheet = 4)
    for (i in map(c("trend_data","abv_en","abv_ara"), exists)){
      if(i==FALSE){
        ##check if data is present, if it isnt, load the data.
      trend_data = read_dta("/Volumes/GoogleDrive/Shared drives/Arab Barometer/AB5/Data/BBC/ABI_ABV_Trend_BBC.dta")
      abv_ara <- read_dta("/Volumes/GoogleDrive/Shared drives/Arab Barometer/AB5/Data/Release Data/ABV_Crosssectional_Data_Release_ARA.dta")
      abv_en <- read_dta("/Volumes/GoogleDrive/Shared drives/Arab Barometer/AB5/Data/Release Data/ABV_Crossectional_Data_Release_ENG.dta")
    } else{
      trend_data=trend_data
      abv_ara=abv_ara
      abv_en = abv_en
    } ###if they are loaded, refresh the data frames
      
    }
  }



explore = function(dataframe){
  explore = list()
  for (i in seq_along(dataframe)) {
    name=names(dataframe)[[i]]
    variable= var_lab(dataframe[[i]])
    explore[[i]]=variable
    names(explore)[[i]]=name
    remove(name, variable)
  }
  return(explore)
}

find_variable = function(string, dataframe){
  position = grep(string, explore(dataframe), ignore.case = TRUE)
  find_variable=list()
  for (i in seq_along(position)) {
    pos=position[[i]]
    name = explore(dataframe)[[pos]]
    variable = names(explore(dataframe))[[pos]]
    find_variable[[i]]=variable
    names(find_variable)[[i]]=name
  }
  remove(position, pos)
  return(find_variable)
}




recode_country=function(dataframe){
  dataframe$Country=dataframe$country
  for(i in seq_along(val_lab(dataframe$country))){
    j= names(val_lab(dataframe$country))[[i]]
    k= val_lab(dataframe$country)[[i]]
    dataframe$Country = ifelse(dataframe$country==k,j, dataframe$Country)
  }
  return(dataframe)
}




title_function= function (variable){
  title = c(labeling[[variable]])
  if(!is.na(title[2])){
    paste(variable, str_wrap(title[1],width = 60),title[2], sep = "\n ")
  }else{
    paste(variable, str_wrap(title[1],width = 60), sep = "\n ")
  }
}

short_title =function (variable){
  title = c(labeling[[variable]])
  paste(title[2], sep = "\n ")
}


long_subtitle_function= function (variable){
  title = c(labeling[[variable]])
  paste(title[3],title[4], sep="\n",collapse = "\n")
}

subtitle_function= function (variable){
  title = c(labeling[[variable]])
  paste(title[3],sep="\n",collapse = "\n")
}

individual_country_plot <- function(data, x, y, country) {
  
  title = title_function(deparse(substitute(y)))
  
  title = str_wrap(paste(country, substitute(y), title, sep = ":"), width = 70)
  subtitle = subtitle_function(deparse(substitute(y)))
  x <- enquo(x)
  y <- enquo(y)
  
  
  ggplot(data,aes(!!x,!!y,fill=!!x)) + geom_col()+geom_bar(stat = "identity")+
    coord_flip()+ggtitle(title, subtitle = subtitle)+theme_bw()+
    scale_fill_manual(values=c('#2096BA','#21718A'))+
    theme(plot.title = element_text(size=12), plot.subtitle = element_text(size = 10),axis.text.y = element_text(angle = 45),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ggtitle(title)+
    geom_text(aes(label=round(!!y*100)), vjust=1.9,hjust=-.3 , color="black", size=3.5)
}



grouped_country_plot_filled <- function(data, x, y, by=NULL, colour = NULL) {
  
  subtitle = paste(paste("(",deparse(substitute(group)),")", sep =""),subtitle(deparse(substitute(y))),sep="\n")
  
  title = short_title(deparse(substitute(y)))
  
  subtitle = subtitle_function(deparse(substitute(y)))
  x <- enquo(x)
  y <- enquo(y)
  by = enquo(by)
  
  ggplot(data,aes(reorder(!!x, +!!y),!!y, fill = !!by)) +geom_bar(stat = "identity", width = .5, fill=colour)+
    coord_flip()+ggtitle(title, subtitle = subtitle)+theme_bw()+
    theme(legend.position = "none",  plot.title = element_text(size=12, hjust=0.5), plot.subtitle = element_text(size = 10, hjust = 0.5),axis.text.y = element_text(angle = 45),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ggtitle(title)+ labs(caption = "Notes: Weighted Estimates.\n Source: Arab Barometer, Wave 5")+
    geom_text(aes(label=round(!!y)), hjust=-.2 , color="black", size=3.5)+xlab("") +ylab("Percent")+theme(text = element_text(family = "Arial"),
panel.border = element_rect(colour = "black", fill=NA, size=.6),plot.caption = element_text(hjust = 0),plot.subtitle = element_text(face = "italic")  )
}


grouped_country_plot <- function(data=data, x, y, group = NULL, colour = NULL, title = title_function, subtitle= subtitle_function) {
  

  subtitle = subtitle(deparse(substitute(y)))
  
  title = short_title(deparse(substitute(y)))
  x <- enquo(x)
  y <- enquo(y)
  group <- enquo(group)
    ggplot(data = data,aes(reorder(!!x, +!!y),!!y, fill = !!group)) +geom_col(width = .5, position = position_dodge(width = 1))+
    coord_flip()+ggtitle(title, subtitle = subtitle)+theme_bw()+scale_fill_manual(values = colour)+
    theme(legend.position = "bottom", axis.text.x = element_text(size = 10),plot.caption = element_text(size = 11),  plot.title = element_text(size=16, hjust=0.5), plot.subtitle = element_text(size = 10, hjust = 0.5),axis.text.y = element_text(angle = 45, size = 12),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
     labs(caption = "Notes: Weighted Estimates.\nSource: Arab Barometer, Wave 5")+
    geom_text(aes(label=round(!!y)),color="black", size=3, hjust = -.2)+xlab("") +ylab("Percent")+theme(text = element_text(family = "Arial"),
                                                                                                     panel.border = element_rect(colour = "black", fill=NA, size=.6),plot.caption = element_text(hjust = 0),plot.subtitle = element_text(face = "italic"))+ylim(0,100)
}

trend_graph_function = function (data=NULL, x, y, group=NULL, title= title_function, subtitle=subtitle_function, colour=colour){
  
    subtitle = subtitle(deparse(substitute(y)))

  title = title(deparse(substitute(y)))
  x <- enquo(x)
  y <- enquo(y)
  group = enquo(group)
  
    ggplot(data,aes(x= !!x, y= !!y, group = !!group))+geom_point(aes(colour = !!group))+geom_line(aes(colour = !!group))+
    ggtitle(title, subtitle = subtitle)+theme_bw()+
    theme(plot.title = element_text(size=12, hjust=0.5), plot.caption = element_text(size = 11),
          plot.subtitle = element_text(size = 10, hjust = 0.5),axis.text.y = element_text(angle = 45),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())+scale_color_manual(values = colour)+
          ggtitle(title)+ labs(caption = "Notes: Weighted Estimates.\nSource: Arab Barometer")+ylim(1,100)+
         geom_text_repel(aes(label=round(!!y)), vjust = -1.2, color="black", size=3.5)+xlab("") +
         ylab("Percent")+
        theme(text = element_text(family = "Arial"),
          panel.border = element_rect(colour = "black", 
                                      fill=NA, size=.6),
          plot.caption = element_text(hjust = 0),plot.subtitle = element_text(face = "italic"))
}



plotting_function = function(data, x, y, color, title = title){
  x= enquo(x)
  y=enquo(y)
  
  ggplot(data,aes(reorder(!!x, !!y),!!y)) +geom_col(width = .5, position = position_dodge(width = 1), fill = color)+
    coord_flip()+theme_bw()+
    theme(legend.position = "bottom", axis.text.x = element_text(size = 10),
          plot.caption = element_text(size = 12, hjust = 0),  plot.title = element_text(size=20, hjust=0.5), 
          plot.subtitle = element_text(size = 16, hjust = 0.5, face = "italic"),
          axis.text.y = element_text(angle = 45, size = 12),panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(.2,.2,.2,.2), "cm"))+
    labs(caption = paste(labeling[["Arabic Notes"]][2], labeling[["Arabic Notes"]][3:4], sep="\n"))+
    geom_text(aes(label=round(!!y)),color="black", size=3, hjust = -.2)+xlab("") +ylab(labeling[["Arabic Notes"]][5])+
    theme(text = element_text(family = "Arial"), 
          panel.border = element_rect(colour = "black", fill=NA, size=.6))+
    ylim(0,100)+ggtitle(str_wrap(labeling[[title]][4],width = 40), subtitle = labeling[[title]][5])
  
}    


colored_bar_plot = function(data, x, y, title){
  x = enquo(x)
  y= enquo(y)
  label=data[["Label"]]
  
  ggplot(data,aes(reorder(!!x, !!y),!!y, fill= !!x))+geom_bar(stat = "identity")+scale_fill_ab("mix", labels =label)+
    coord_flip()+theme_bw()+
    theme(legend.title =element_blank(),legend.position = "right", 
          axis.text.x = element_text(size = 10),plot.caption = element_text(size = 8),  
          plot.title = element_text(size=20, hjust=0.5), plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
          axis.text.y = element_text(angle = 45, size = 12),panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), axis.ticks.y  = element_blank(), axis.text.y.left  = element_blank())+
    labs(caption = paste(labeling[["Arabic Notes"]][2], labeling[["Arabic Notes"]][3:4], sep="\n"))+
    geom_text(aes(label=round(!!y)),color="black", size=3, hjust = -.2)+xlab("") +
    ylab(labeling[["Arabic Notes"]][5])+theme(text = element_text(family = "Arial"), 
                                              panel.border = element_rect(colour = "black", fill=NA, size=.6),
                                              plot.caption = element_text(hjust = 0, size = 12),
                                              legend.text = element_text(size=12),
                                              plot.margin = unit(c(.2,.2,.2,.2), "cm"))+ylim(0,100)+
    ggtitle(str_wrap(labeling[[title]][4],width = 40), subtitle = labeling[[title]][5])
  
}


plotterizer = function(dataframe, x, fill=NA,  pallette=NULL){
  legend_title=fill
  print(legend_title)
  fill=dataframe[[fill]]
  title1 = short_title(x)
  subtitle = subtitle_function(x)
  variable = dataframe[[x]]
  ggplot(dataframe, aes(Country, variable*100, fill=fill))+
    geom_col(position = position_dodge(width = 1.2))+
    coord_flip()+
    geom_text(aes(label=round(variable*100), hjust = -.5),size=2.6, position = position_dodge(width = .7))+
    scale_fill_ab(pallette)+theme_bw()+
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
  
}


####Colors
women_color = '#796391'
country_color = "#DF6E21"
education_color= c("#FBA950","#F28232")
gender_color = c("#CE83A3","#796391")
gender_color_trend = c("#796391", "#CE83A3",'#DF6E21')
trend_colors = c("#FBA950","#5C5883","#7CBBC7","#CE83A3")
age_colors = c("#21718A","#2096BA")

