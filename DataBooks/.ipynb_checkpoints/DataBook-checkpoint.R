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
  mutate(Q104B_GCC =case_when(
    Q104B_KSA == 1 |Q104B_UAE == 1|Q104B_QA == 1|Q104B_BA ==1 |Q104B_KU ==1 | Q104B_OM==1~ 1, 
    Q104B_KSA == 0 &Q104B_UAE == 0&Q104B_QA == 0&Q104B_BA ==0 &Q104B_KU ==0 & Q104B_OM==0~0
  ))%>%
  mutate(Q104B_MENA = case_when(
    Q104B_EG == 1 |Q104B_JO == 1|Q104B_LEB == 1|Q104B_MO ==1 |Q104B_AL ==1 | Q104B_TUN==1|Q104B_TUR==1~ 1, 
    Q104B_EG == 0 &Q104B_JO == 0&Q104B_LEB == 0&Q104B_MO ==0 &Q104B_AL ==0 & Q104B_TUN==0|Q104B_TUR==0~0
  ))%>%
  mutate(Q104B_EU = case_when(
    Q104B_EEU == 1 |Q104B_FR == 1|Q104B_GER == 1|Q104B_ESP ==1 |Q104B_IT ==1 | Q104B_WEUOTHER==1|Q104B_UK==1~ 1, 
    Q104B_EEU == 0 |Q104B_FR == 0|Q104B_GER == 0|Q104B_ESP ==0 |Q104B_IT ==0 | Q104B_WEUOTHER==0|Q104B_UK==0~ 0
  ))

val_lab(abv$Q104B_MENA)= c("they want to emigrate to a MENA country"=1, "not mentioned" = 2)
val_lab(abv$Q104B_GCC)= c("they want to emigrate to a GCC country"=1, "not mentioned" = 2)
val_lab(abv$Q104B_EU)= c("they want to emigrate to a EU country"=1, "not mentioned" = 2)

##Q301
abv$Q301B=ifelse(abv$Q301B%in%c(4,5),1,0)
abv$Q301A =  ifelse(abv$Q301A==1, 1, 0)
abv$Q301 = ifelse(abv$splita==1, abv$Q301A, abv$Q301B)
val_lab(abv$Q301)=c("yes"=1)
abv=abv%>%
  recode_country()%>%
  filter(!is.na(wt))%>%
  mutate_at(Media_Usager, function(x)x=ifelse(x%in%c(1),1,ifelse(x%in%c(0), 2, x)))

for (i in Media_Usager) {
  val_lab(abv[[i]])=c("they use this social media platform"=1,"not mentioned = 2")
  
}

##Q860
source("/Users/pharned/Documents/Arab-Barometer/DataBooks/Q860.R")

exclude = unlist(unique(map(names(labeling), function(x){
  if(x%nin%names(abv)){
    return(x)
  }
})))

Variables_For_Unique_Coding = c("Q100", "Q2061A", "Q2061B", "Q104A", "Q841", "Q7141A", "Q7141B", "Q701H","Q421","Q516A","Q705","Q703")


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

Subset =names(labeling)[names(labeling)%nin%c(exclude)]
Subset=Subset[Subset%nin%c(Variables_For_Unique_Coding, "Q860B","Q860C","Q860D", paste("Q104A",c(1:8),sep = "_"))]


Variables = sapply(Subset, binaraizer, dataframe = abv, USE.NAMES = TRUE)

try =   str_split(names(Variables), "=")

Subtitles = map(try, 2)
Subtitles_Frame = as.data.frame(unlist(Subtitles))%>%
  mutate(names = paste(Subset))%>%
  `colnames<-`(c("Subtitle", "Names"))%>%
  mutate(Subtitle = as.character(Subtitle))%>%
  spread(Names, Subtitle)%>%
  as_tibble()

New_Frame = bind_cols(Variables)%>%
  setnames(Subset)

Complete = cbind(New_Frame,select(abv, id, wt, Country, splita)) 

Overal_Summaries = country_grouping_function(Complete, Subset)

overall_plots = map(Subset, ~plotterizer(dataframe = Overal_Summaries, x=.x, y="Country"))%>%
  set_names(paste(Subset, "overall",sep = "_"))

Backwards_Coding = c("Q424","Q423_3", "Q423_2","Q423_1","Q512","Q511","Q855A","Q855B")
map(Backwards_Coding, function(x)print(val_lab(abv[[x]])))
