
for(i in c("expss", "haven","readxl","tidyverse","ggrepel")){library(i,character.only = TRUE)}



if(startsWith(getwd(),"C")==TRUE){
  source("C:/Users/Patrick Harned/Google Drive/Coding/R Projects/Arab-Barometer-Wraps/AB_Colors.R")
  labeling = read_xlsx("C:/Users/Patrick Harned/Google Drive/Coding/R Projects/Captions/Captions and Titles.xlsx", sheet=4)
  abv <- read_dta("G:/Shared drives/Arab Barometer/AB5/Data/Release Data/ABV_Crosssectional_Data_Release_ARA.dta")
  trend_data <- read_dta("G:/Shared drives/Arab Barometer/AB1-4/ABI_ABV_Trend_File.dta")
  }else{
    source("/Users/pharned/Google Drive/Coding/R Projects/Arab-Barometer-Wraps/AB_Colors.R")
    while(sum(map_lgl(c("trend_data", "abv_en","abv_ara"),exists))<3){
    trend_data = read_dta("/Volumes/GoogleDrive/Shared drives/Arab Barometer/AB5/Data/BBC/ABI_ABV_Trend_BBC.dta")
    abv_ara <- read_dta("/Volumes/GoogleDrive/Shared drives/Arab Barometer/AB5/Data/Release Data/ABV_Crosssectional_Data_Release_ARA.dta")
    abv_en <- read_dta("/Volumes/GoogleDrive/Shared drives/Arab Barometer/AB5/Data/Release Data/ABV_Crossectional_Data_Release_ENG.dta")
  }
    labeling=read_xlsx("/Users/pharned/Google Drive/Coding/R Projects/Captions/Captions and Titles.xlsx", sheet = 4)
}


`%nin%`= negate(`%in%`)

explore = function(dataframe){
  
  map(dataframe, var_lab)%>%
    set_names(names(dataframe))
}




find_variable = function(string, dataframe, names=FALSE){
  if(names == FALSE){
    position = grep(string, explore(dataframe), ignore.case = TRUE)
  }else{
    position = grep(string, names(explore(dataframe)), ignore.case = TRUE)
  }
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


recoderizer = function(dataframe, variable, string, list){
  variable1=dataframe[[variable]]
  labcheck=names(val_lab(variable1))
  labcheck = paste(labcheck, collapse = ", ")
  print(labcheck)
  if(grepl(string, labcheck)==TRUE){
    print(paste("Recoding ",variable, sep = " "))
    print(labcheck)
    dataframe[[variable]] = ifelse(dataframe[[variable]] %in% list,1,0)
    print(variable)
    return(dataframe)
  }else{
    print(paste("Didnt find match",variable, sep = " "))
  }
}

title_function= function (variable){
  title = c(labeling[[variable]])
  if(length(title[2])==0){
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


trend_graph_function = function (dataframe=NULL, x, y, pallette=NA){
  
  title1 = title_function(y)
  subtitle = subtitle_function(y)
  x = dataframe[[x]]
  y= dataframe[[y]]
  
    ggplot(dataframe,aes(x= x, y= y*100, group=1))+geom_point(color=ab_cols(pallette), aes(x=x))+geom_line(aes(x=x),color = ab_cols(pallette))+
    ggtitle(title, subtitle = subtitle)+theme_bw()+
    theme(plot.title = element_text(size=12, hjust=0.5), plot.caption = element_text(size = 11),
          plot.subtitle = element_text(size = 10, hjust = 0.5),axis.text.y = element_text(angle = 45),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
          ggtitle(title1, subtitle = subtitle )+ labs(caption = "Notes: Weighted Estimates.\nSource: Arab Barometer")+ylim(1,100)+
         geom_text_repel(aes(label=round(y*100)), vjust = -1.2, color="black", size=3.5)+xlab("") +
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



plotterizer = function(dataframe, x, fill = NA, pallette=NA){
  fill=dataframe[[fill]]
  legend_title=names(dataframe)[[2]]
  title1 = title_function(x)
  subtitle = subtitle_function(x)
  variable = dataframe[[x]]
  if(length(fill)==0){
    plot=ggplot(dataframe, aes(reorder(Country,variable), round(variable*100), fill=fill))+
      geom_bar(stat = "identity",position = position_dodge(width = 1), width = .8, fill=country_color)
  }else{
    plot=ggplot(dataframe, aes(reorder(Country,variable), round(variable*100), fill=fill))+
      geom_bar(stat = "identity",position = position_dodge(width = 1), width = .8)
  }
  plot=plot+
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
####Colors
grouping_function=function(dataframe, x=NA, group=NA, varlist){
  
  print(x)
  dataframe%>%
    group_by(!!x)%>%
    summarise_at(varlist,list(~weighted.mean(., w=wt, na.rm = TRUE)))%>%
    filter(!is.na(!!x))
}


