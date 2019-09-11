library(survey)
library(plotly)




iran_plot=abv%>%
  filter(!is.na(wt),!is.na(wt))%>%
  mutate(Q7141A_Israel=ifelse(Q7141A%in%c(4),1,ifelse(is.na(Q7141A)==TRUE,Q7141A, 0)))%>%
  mutate(Q7141A_Iran = ifelse(Q7141A%in%c(5),1, ifelse(is.na(Q7141A)==TRUE,Q7141A,0)))%>%
  recode_country()%>%
  group_by(Country)%>%
  summarise_at(c("Q7141A_Israel","Q7141A_Iran"), list(~weighted.mean(., w=wt, na.rm = TRUE)))%>%
  mutate_at(c("Q7141A_Israel","Q7141A_Iran"), function(x)round(x*100))%>%
  mutate(Q7141A_Iran = -abs(Q7141A_Iran))%>%
  print(n=100)




Iran = "#DF6E21"
Israel = "#21718A"
arabic_plot =ggplot(iran_plot_arabic, aes(reorder(Country, Q7141A_Israel)))+geom_bar(aes(y= Q7141A_Iran, fill = Israel),
                                                                  stat = "identity")+
geom_bar(aes(y= Q7141A_Israel, fill=Iran), stat = 'identity')+
  coord_flip()+
  theme_bw()+
  geom_text(aes(y = 5,label =Q7141A_Israel))+geom_text(aes(y=-5, label = abs(Q7141A_Iran)))+
  scale_fill_manual(values = c("#DF6E21", "#21718A"), labels = c("Iran", "Israel"))+
  scale_y_continuous(limits = c(-100,100), breaks=c(-100, -75,-50,-25,0,25,50,75,100),
                     labels=c("100", "75","50","25","0","25","50","75","100"))+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption = element_text(size = 11, hjust = 0),  
        plot.title = element_text(size=20, hjust=0.5),
        plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic"),
        axis.text.y = element_text(angle = 45, size = 12),
        axis.text.x=element_text(size = 10),
        text = element_text(family = "Arial"),
        axis.title.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  labs(caption = "Notes: Weighted Estimates.\nSource: Arab Barometer, Wave 5")+
  xlab("")+ ggtitle(labeling[["Q709B"]], subtitle = "% saying Iran versus Israel")
 
iran_plot_arabic=abv_ara%>%
  filter(!is.na(wt),!is.na(wt))%>%
  mutate(Q7141A_Israel=ifelse(Q7141A%in%c(4),1,ifelse(is.na(Q7141A)==TRUE,Q7141A, 0)))%>%
  mutate(Q7141A_Iran = ifelse(Q7141A%in%c(5),1, ifelse(is.na(Q7141A)==TRUE,Q7141A,0)))%>%
  recode_country()%>%
  group_by(Country)%>%
  summarise_at(c("Q7141A_Israel","Q7141A_Iran"), list(~weighted.mean(., w=wt, na.rm = TRUE)))%>%
  mutate_at(c("Q7141A_Israel","Q7141A_Iran"), function(x)round(x*100))%>%
  mutate(Q7141A_Iran = -abs(Q7141A_Iran))%>%
  print(n=100)


arabic_plot =ggplot(iran_plot_arabic, aes(reorder(Country, Q7141A_Israel)))+geom_bar(aes(y= Q7141A_Iran, fill = Israel),
                                                                                     stat = "identity")+
  geom_bar(aes(y= Q7141A_Israel, fill=Iran), stat = 'identity')+
  coord_flip()+
  theme_bw()+
  geom_text(aes(y = 5,label =Q7141A_Israel))+geom_text(aes(y=-5, label = abs(Q7141A_Iran)))+
  scale_fill_manual(values = c("#DF6E21", "#21718A"), labels = c("Iran", "Israel"))+
  scale_y_continuous(limits = c(-100,100), breaks=c(-100, -75,-50,-25,0,25,50,75,100),
                     labels=c("100", "75","50","25","0","25","50","75","100"))+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption = element_text(size = 11, hjust = 0),  
        plot.title = element_text(size=20, hjust=0.5),
        plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic"),
        axis.text.y = element_text(angle = 45, size = 12),
        axis.text.x=element_text(size = 10),
        text = element_text(family = "Arial"),
        axis.title.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  labs(caption = paste(labeling$`Arabic Notes`[2:3], sep = "\n"))+
  xlab("")+ ggtitle(labeling[["Q709B"]], subtitle = "% saying Iran versus Israel")


arabic_plot = arabic_plot+ggtitle(labeling[["Q7141A"]][4], subtitle = labeling[["Q7141A"]][6])
  


Q709b_abv=abv%>%
  filter(!is.na(id), !is.na(wt))%>%
  recode_country()%>%
  mutate(Q709B=ifelse(Q709B%in%c(1,2),1,0))%>%
  group_by(Country)%>%
  summarise(weighted.mean(Q709B,w=wt, na.rm = TRUE))%>%
  filter(Country!="Kuwait")
   
names(Q709b_abv)= c("Country", "Q709B")


Q709b_abv_ara = abv_ara%>%
  filter(!is.na(id), !is.na(wt))%>%
  recode_country()%>%
  mutate(Q709B=ifelse(Q709B%in%c(1,2),1,0))%>%
  group_by(Country)%>%
  summarise(Q709B =weighted.mean(Q709B,w=wt, na.rm = TRUE))%>%
  filter(Q709B!=0)

 Q709b_arabic = plotterizer(Q709b_abv_ara, "Q709B")+ggtitle(labeling[["Q709B"]][[4]], subtitle = labeling[["Q709B"]][[4]])+
   labs(caption = paste(labeling$`Arabic Notes`[2:3], sep = "\n"))
