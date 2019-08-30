##youth report graphs
library(haven)
library(expss)
library(dplyr)
library(survey)
rm(list=ls())

ABV_Crosssectional_Data_Release_ARA <- read_dta("/Volumes/GoogleDrive/Shared drives/Arab Barometer/AB5/Data/Release Data/ABV_Crosssectional_Data_Release_ARA.dta")
variable_list= c("Q609","Q404","Q421","Q521A_3","Q204A_1","Q204_2","Q104","Q521_1","Q521_4","Q601_14","Q601_3","Q601_1","Q700A_1")
data1 = ABV_Crosssectional_Data_Release_ARA
val_lab_list = list()
for(i in seq_along(variable_list)){
  variable=variable_list[i]
  variable=ABV_Crosssectional_Data_Release_ARA[[variable]]
  val_lab_list[i]=list(val_lab(variable))
}
new_val_lab_list=list()
for (i in seq_along(unique(val_lab_list))) {
  temp=val_lab_list[i]
  new_val_lab_list[i]=paste(temp)
}
new_val_lab_list=new_val_lab_list[1:9]
recoding = list( c(1),c(1,2), c(6), c(1,2),c(1,2),c(1,2),c(1),c(1,2),c(1,2) )
recoding_frame=tibble(new_val_lab_list, recoding)

for (i in seq_along(unique(recoding_frame$new_val_lab_list))){
  temp_list=list()
  label = recoding_frame$new_val_lab_list[i]
  coding = recoding_frame$recoding[[i]]
  print(coding)
  for (j in seq_along(variable_list)){
    temp_variable = variable_list[j]
    variable_label = val_lab(ABV_Crosssectional_Data_Release_ARA[[temp_variable]])
    temp_list[j]=list(variable_label)
    for (k in seq_along(temp_list)) {
      if(paste(temp_list[k],collapse = "")==label){
        data1[[temp_variable]]=ifelse(ABV_Crosssectional_Data_Release_ARA[[temp_variable]]%in%coding,1,0)
        val_lab(data1[[temp_variable]])=val_lab(ABV_Crosssectional_Data_Release_ARA[[temp_variable]])
        var_lab(data1[[temp_variable]])=var_lab(ABV_Crosssectional_Data_Release_ARA[[temp_variable]])
        print(data1[[temp_variable]])
        }
    }
    
    }
}

data2=data1%>%
  filter(!is.na(wt),!is.na(id))%>%
  select(variable_list, id, wt, stratum, country, psu)




data_1_design = svydesign(ids = ~id, weights = ~wt, strata = ~stratum, data = data2, nest = TRUE)



for (i in seq_along(variable_list)){
  variable_name = variable_list[[i]]
  variable= data_1_design$variables[[variable_name]]
  temptibble2= as.tibble(svytable(~country+variable, data_1_design))%>%
  group_by(country)%>%
  mutate(prop = n/sum(n), variable_name=1-prop, variable_name=variable_name*100)%>%
  filter(variable!=0)%>%
  select(country, variable_name)
  names(temptibble2)[names(temptibble2)=="variable_name"] <- variable_name
  #assign(variable_name,temptibble2)
  print(variable_name)
  print(temptibble2)
  plot1 = plot_function_temp(temptibble2)
  print(plot1)
}


      grouped_country_plot(data = temptibble2, x=country, y=name, group = country_color, colour = country_color)

  
plot_function_temp = function(data){
  title=names(data[2])
  v=sym(title)
  v= enquo(v)
  print(v)
  ggplot(data, aes(x=country, y=!!v))+geom_col()+ggtitle(labeling[[title]])+theme_bw()+
}

plot_function_temp(temptibble2)

ggplot(temptibble2,aes(x=country, y=Q700A_1))+geom_col()
labeling[[names(temptibble2[2])]]
