
library(survey)
library(sjlabelled)
Egypt= abv%>%
  filter(country==5)

egypt_design= svydesign(ids = ~id, weights = ~wt,strata = ~stratum,data = Egypt,nest = TRUE)
egypt_table = svytable(~Q2061A, design = egypt_design)%>%
  as.data.frame()%>%
  mutate(Q2061a = Freq/sum(Freq))%>%
  mutate(Q2061a=Q2061a*100)%>%
  mutate(Labels = Q2061A)%>%
  add_labels(Q2061A,labels = val_lab(abv$Q2061A))%>%
  mutate(Labels = names(val_lab(abv_ara$Q2061A)[1:12]))%>%
  group_by(Q2061A)%>% 
  mutate(Q2061a=round(Q2061a))
 





ggplot(egypt_table,aes(x="",y=Q2061a,fill=Q2061A,))+
  geom_col(position = position_stack(reverse = TRUE))+
  scale_fill_ab("pie", reverse = TRUE, labels = egypt_table$Labels)+
  ggtitle(str_wrap(labeling[["Q2061A"]][4], width = 50), subtitle = labeling[["Q2061A"]][5])+theme_minimal() +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), 
        plot.subtitle = element_text(hjust = .5, size = 14, face = "italic"), 
        plot.title = element_text(hjust =.5, size = 20),panel.grid = element_blank(),
        panel.border = element_blank(),legend.text = element_text(size = 12), 
        axis.ticks = element_blank(), legend.position = "bottom", 
        panel.grid.minor = element_blank(), axis.text.x = element_blank(), 
        panel.grid.major = element_blank(),legend.key.size = unit(0.3, "cm"))+xlab("")+geom_text(aes(y=Q2061a/2+c(0, cumsum(Q2061a)[-length(Q2061a)]), 
                                                                                                     label = Q2061a),size= 5,colour="white", check_overlap = TRUE)+coord_polar("y", start = 0)
