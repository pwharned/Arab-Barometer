setwd("/Users/pharned/Documents/Arab-Barometer/DataBooks")
library(list)
source('~/Documents/Arab-Barometer/Wraps/Functions.R')
source('~/Documents/Arab-Barometer/Data Validation/Data_Recode(9:19).R')
source('~/Documents/Arab-Barometer/DataBooks/Binarizer.R')

Media_Usager = find_variable("Q412A", abv, names = TRUE)%>%
  as.character()


country_grouping_function=function(dataframe, varlist){
  
  dataframe%>%
    group_by(Country)%>%
    summarise_at(varlist,list(~weighted.mean(., w=wt, na.rm = TRUE)))
}
##Q104B requires special coding.

abv = abv%>%
  transmute(Q104B_GCC =case_when(
    Q104B_KSA == 1 |Q104B_UAE == 1|Q104B_QA == 1|Q104B_BA ==1 |Q104B_KU ==1 | Q104B_OM==1~ 1, 
    Q104B_KSA == 0 &Q104B_UAE == 0&Q104B_QA == 0&Q104B_BA ==0 &Q104B_KU ==0 & Q104B_OM==0~0
  ))%>%
  transmute(Q104B_MENA = case_when(
    Q104B_EG == 1 |Q104B_JO == 1|Q104B_LEB == 1|Q104B_MO ==1 |Q104B_AL ==1 | Q104B_TUN==1|Q104B_TUR==1~ 1, 
    Q104B_EG == 0 &Q104B_JO == 0&Q104B_LEB == 0&Q104B_MO ==0 &Q104B_AL ==0 & Q104B_TUN==0|Q104B_TUR==0~0
  ))%>%
  transmute(Q104B_EU = case_when(
    Q104B_EEU == 1 |Q104B_FR == 1|Q104B_GER == 1|Q104B_ESP ==1 |Q104B_IT ==1 | Q104B_WEUOTHER==1|Q104B_UK==1|Q104B_WE==1~ 1, 
    Q104B_EEU == 0 |Q104B_FR == 0|Q104B_GER == 0|Q104B_ESP ==0 |Q104B_IT ==0 | Q104B_WEUOTHER==0|Q104B_UK==0|Q104B_WE==0~ 1
  ))
  

##Q301
abv$Q301B=ifelse(abv$Q301B%in%c(4,5),1,0)
abv$Q301A =  ifelse(abv$Q301A==1, 1, 0)
abv$Q301 = ifelse(abv$splita==1, abv$Q301A, abv$Q301B)


##Q860
source("/Users/pharned/Documents/Arab-Barometer/DataBooks/Q860.R")

exclude = unlist(unique(map(names(labeling), function(x){
  if(x%nin%names(abv)){
    return(x)
  }
})))

Subset =names(labeling)[names(labeling)%nin%exclude]

Variables_For_Unique_Coding = c("Q2061A", "Q2061B", "Q104A", "Q841", "Q7141A", "Q7141B")

Multiplizer = function(x, dataframe){
  variable_name = paste(x)
  x =dataframe[[x]]
  Lab = val_lab(x)[val_lab(x)%nin%c(90:99)]
  print(Lab)
  print(variable_name)
  i=1
  output =list()
  while (i<length(Lab)+1) {
    print(i)
    Variable_Name = paste(variable_name,i, names(Lab)[i], sep = "_")
    New_Variable = ifelse(x%in%c(i),1, ifelse(x%in%c(NA), NA, 0))
    output[[i]]=assign(Variable_Name, New_Variable)
    print(mean(New_Variable, na.rm = TRUE))
    names(output)[i]=Variable_Name
    i=i+1
  }
  return(output)
}


Unique_Variables = sapply(Variables_For_Unique_Coding, Multiplizer, dataframe=abv)
Unique_Names=map(Unique_Variables, names)%>%
  unlist%>%
  str_split("_")%>%
  map(1)
Unique_Values = map(Unique_Variables, names)%>%
  unlist%>%
  str_split("_")%>%
  map(2)
Unique_Names = paste(Unique_Names, as.character(Unique_Values), sep = "_")

Subtitles_Frame = map(Unique_Variables, names)%>%
  unlist%>%
  str_split("_")%>%
  map(3)%>%
  as.data.frame()%>%
  gather()%>%
  mutate(Names = Unique_Names)%>%
  mutate(Subtitle = value)%>%
  select(Names, Subtitle)%>%
  spread(Names, Subtitle)

Unique_Variables_Frame= Unique_Variables%>%
  flatten()%>%
  set_names(paste(Unique_Names))%>%
  bind_cols()
Unique_Variables_Frame= cbind(Unique_Variables_Frame, select(abv, wt, country, id, splita))%>%
  recode_country()%>%
  country_grouping_function(Unique_Names)
Unique_Variables_Plots = map(Unique_Names, ~plotterizer(dataframe =Unique_Variables_Frame, x=.x, y="Country" ))
  names(Unique_Variables_Plots)=Unique_Names
Fix_plots = function(plot_list){
  for (i in names(plot_list)) {
  if (sum(dim(plot_list[[i]][[1]]))==2){
    plot_list[[i]]=NULL
  }
  }
  return(plot_list)
}
Unique_Variables_Plots=Fix_plots(Unique_Variables_Plots)


variables_to_recode = Subset[Subset %nin% c("Q104B_GCC","Q104B_US", "Q104B_MENA", "Q104B_EU","Q301","Q860B","Q860C","Q860D",Variables_For_Unique_Coding,Media_Usager)]

Variables = sapply(variables_to_recode, binaraizer, dataframe = abv, USE.NAMES = TRUE)

try =   str_split(names(Variables), "=")

Subtitles = map(try, 2)
Subtitles_Frame = as.data.frame(unlist(Subtitles))%>%
  mutate(names = paste(variables_to_recode))%>%
  `colnames<-`(c("Subtitle", "Names"))%>%
  mutate(Subtitle = as.character(Subtitle))%>%
  spread(Names, Subtitle)%>%
  as_tibble()



Q104B_frame = filter(abv, Q104==1)%>%
  country_grouping_function(c("Q104B_GCC", "Q104B_MENA","Q104B_EU","Q104B_US","Q104B_CAN","Q301",Media_Usager))
Q104B_plots = map(c("Q104B_GCC", "Q104B_MENA","Q104B_EU","Q104B_US","Q104B_CAN"),~plotterizer(dataframe = Q104B_frame, x=.x, y="Country") )%>%
  set_names(c("Q104B_GCC", "Q104B_MENA","Q104B_EU","Q104B_US","Q104B_CAN"))




New_Frame = bind_cols(Variables)%>%
  setnames(variables_to_recode)

Complete = cbind(New_Frame,select(abv, id, wt, country, splita)) 

Complete = Complete%>%
  filter(!is.na(wt))%>%
  recode_country()

  
Overal_Summaries = country_grouping_function(Complete, variables_to_recode)
overall_plots = map(variables_to_recode, ~plotterizer(dataframe = new_frame, x=.x, y="Country"))%>%
  set_names(paste(variables_to_recode, "overall",sep = "_"))


split_function = function(dataframe, varlist, splitavalue){
 dataframe = filter(dataframe, splita==splitavalue)
  dataframe%>%
    group_by(Country)%>%
    summarise_at(varlist,list(~weighted.mean(., w=wt, na.rm = TRUE)))
}


split_listA_is_on1 = map_chr(c(paste("Q108", c(1,2,3,4), sep = "_"), paste("Q201B", c(6,13,20,31,12), sep = "_"), paste("Q204B", c(13,15), sep = "_"), 
                           paste("Q204C", c(13,15), sep = "_"), "Q211B", "Q301A", "Q514","Q601_9", paste("Q604A", c(1,3), sep = "_"), 
                           paste("Q604B", c(1,3), sep = "_"), paste("Q701C", c(2,4), sep = "_"), "Q703","Q7141A"), as.character)

split_listA_is_TWO = map_chr(c(paste("Q201C", c(37, 38, 39, 32, 40), sep = "_"), "Q211C", "Q301B", "Q514A", "Q852", "Q853A", "Q853B", "Q854","Q855A","Q855B","Q601_9A",
                           paste("Q605A", c(1,2,3,4), sep = "_"), "Q605B", paste("Q701C", c(5,6), sep = "_"), "Q705", "Q7141B", "Q1017"), as.character)


Splita_1 = split_function(Complete, variables_to_recode, 1)

Splita_1_overall_plots = map(variables_to_recode, ~plotterizer(dataframe = Splita_1, x=.x, y="Country"))%>%
  set_names(variables_to_recode)
Splita_1_overall_plots=Splita_1_overall_plots[split_listA_is_on1]%>%
  set_names(paste(split_listA_is_on1, "overall", sep = "_"))


