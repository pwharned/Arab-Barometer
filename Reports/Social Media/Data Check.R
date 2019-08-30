library(srvyr)
abv_en1=recode_country(abv_en)
test1= abv_en1%>%
  mutate(Q404=ifelse(Q404%in%c(1,2),1,0))%>%
  mutate(Age= ifelse(Q1001%in%c(18:29), "18-29",ifelse(Q1001%in%c(30:120),"30+", NA)))%>%
  filter(!is.na(wt),!is.na(id))%>%
  mutate(Q421=ifelse(Q421==6, 1, 0))%>%
  group_by(Country, Age)%>%
  summarise_at(c("Q404","Q421"),list(~weighted.mean(., w=wt)))

abv_design= svydesign(ids = ~id, weights = ~wt, strata = ~stratum, data =test1, nest= TRUE )

plotterizer = function(variable){

  ggplot(test1, aes(Country, variable*100, fill=Age))+geom_col(position = "dodge")+
    coord_flip()+geom_text_repel(aes(label=round(variable*100)))+
    scale_fill_ab("mix")+theme_bw()+
    theme(legend.position = "bottom", axis.text.x = element_text(size = 10),
          plot.caption = element_text(size = 11),  
          plot.title = element_text(size=16, hjust=0.5), plot.subtitle = element_text(size = 10, hjust = 0.5),
          axis.text.y = element_text(angle = 45, size = 12),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    labs(caption = "Notes: Weighted Estimates.\nSource: Arab Barometer, Wave 5")+
    xlab("")+ 
    ylab("Percent")+theme(text = element_text(family = "Arial"),
    panel.border = element_rect(colour = "black", fill=NA, size=.6),
    plot.caption = element_text(hjust = 0),
    plot.subtitle = element_text(face = "italic"))+ylim(0,100)
  
  
}

variable_kist= c("Q421", "Q404")
lapply(test1[variable_kist],plotterizer)
