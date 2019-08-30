library(srvyr)
abv_en1=recode_country(abv_en)

test1= abv_en1%>%
  mutate(Q404=ifelse(Q404%in%c(1,2),1,0))%>%
  mutate(Age= ifelse(Q1001%in%c(18:29), "18-29",ifelse(Q1001%in%c(30:120),"30+", NA)))%>%
  filter(!is.na(wt),!is.na(id))%>%
  mutate(Q421=ifelse(Q421==6, 1, 0))%>%
  group_by(Country, Age)%>%
  summarise_at(c("Q404","Q421"),list(~weighted.mean(., w=wt, na.rm = TRUE)))%>%
  filter(!is.na(Age))


plotterizer = function(dataframe, x, fill=NA,  pallette=NULL){
  legend_title=fill
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

variable_kist= c("Q421", "Q404")



map(variable_kist,~plotterizer(dataframe=test1,x=.x, pallette = "mix", fill = "Age"))
