to_be_fixed = c("Q201B_6 Yemen if Q201B_6==NA and splita==1 -> 99
Q201B_12 Iraq,Yemen if Q201B_12==NA and splita==1 -> 99
Q201B_13 Yemen  if Q201B_13==NA and splita==1 -> 99
Q201B_20 Yemen  if Q201B_20==NA and splita==1 -> 99
Q201C_27 Iraq if Q201C_37==NA  and splita==2 -> 99
Q201B_31 Yemen  if Q201B_31==NA and splita==1 -> 99
Q201B_37 Yemen  if Q201B_37==NA and splita==2 -> 99
Q201C_40 Iraq if Q201C_40==NA  and splita==2 -> 99
Q204B_13 Palestine if Q204B_13==NA and splita==1 -> 99

Q211B Yemen, if Q211B==NA and splita==1 -> 99 plus 1 case incorrect filter. 

Q211C Iraq,Yemen if Q211C==NA and splita==2 -> 99. 

Q301A,Palestine, Yemen if Q301A==NA and splita==1 ->99. 

Q301B,Palestine  if Q301B==NA and splita==1 ->99. 


Q514, Yemen if Q514==NA and split==1 ->99

Q514A, Iraq, Palestine, Yemen if Q514A==NA and splita==2 ->99
 
Q601_9, if Q601_9== NA and splita==1

Q601_9A if Q601_9A==NA and splita==2


Q604A_1 Iraq,Yemen if Q604A_1==NA and splita==1 -> 99 Refused

Q604A_3 Iraq,Yemen if Q604A_3==NA and splita==1 -> 99 Refused

Q604A_6 Palestine, Yemen if Q604A_6==NA and splita==1 -> 99 Refused


Q604A_7 Palestine, Yemen if Q604A_6==NA and splita==1 -> 99 Refused


Q604B_1 Iraq if Q604B_1==NA and splita==2 -> 99 refused, 


Q604B_3 Iraq if Q604B_3==NA and splita==2 -> 99 refused, 


Q604B_6 Palestine  if Q604A_6==NA and splita==2 -> 99 Refused 


Q604B_7 Palestine  if Q604A_6==NA and splita==2 -> 99 Refused 

Q605A_1 if Q605A_1==NA and splita==2-> 99 refused.


Q605A_2, if Q605A_2==NA and splita==2-> 99 refused.


Q605A_3, if Q605A_3==NA and splita==2-> 99 refused.


Q605A_4 if Q605A_4==NA and splita==2-> 99 refused.

Q605B_1 if Q605B_1 ==NA and splita==2-> 99 refused.

Q605B_2 if Q605B_1 ==NA and splita==2-> 99 refused.

Q605B_3 if Q605B_1 ==NA and splita==2-> 99 refused.


Q605B_4 if Q605B_4==NA and splita==2-> 99 refused.


Q701C_5 if Q701C_5==NA and splita==2-> 99 refused

Q701C_6 Iraq if Q701C_6==NA and splita==2-> 99 refused


Q701C_2 and if Q701C_2==NA and splita==1-> 99 refused

Q701C_4 if Q701C_4==NA and splita==1-> 99 refused


Q703 Iraq,Yemen if  Q703==NA and splita==1 ->99 refused. 

Q705 Iraq,Yemen if  Q705==NA and splita==1 ->99 refused. 

Q7141A Iraq,Yemen if  Q7141A ==NA and splita==1 ->99 refused. 

Q7141B Iraq,Yemen if Q7141A ==NA and splita==2 ->99 refused. 



Q852 Iraq,Morocco,  if Q852==NA and splita==2 -> 99 refused.  


Q854 Iraq, Morocco if Q854==NA and splita==2 ->99. 

Q854 if 854!=NA if splita==1



Q855A Iraq, Morocco if Q855A==NA and splita==2 ->99. 

Q855B Iraq, Morocco if Q855B==NA and splita==2 ->99.  

Q855B if 855B!=NA and splita==1


Q860A Iraq, Palestine if Q860A==NA and splitb==1 ->99

Q860B Iraq, Palestine if Q860A==NA and splitb==2 ->99

Q860C Iraq, Egypt if Q860C==NA and splitb==3 ->99

Q860D Palestine if Q860D==NA and splitb==3 ->99

") 
list_to_be_fiexed  = strsplit(to_be_fixed,"Q")
list_to_be_fiexed = paste("Q", list_to_be_fiexed[[1]], sep = "")


length(list_to_be_fiexed)

without_unederscore = str_extract_all(list_to_be_fiexed, "^Q\\w+")%>%
  flatten()%>%
  unique()
splits = str_extract_all(list_to_be_fiexed, "split\\w+==\\d")%>%
  unlist()

splits_frame  = cbind(splits, without_unederscore)%>%
  as_tibble()%>%
  mutate(without_unederscore=unlist(without_unederscore))%>%
  mutate(splits=ifelse(without_unederscore%in%c("Q201C_37","Q201C_40","Q211C","Q514","Q601_9A","Q701C_5","Q701C_6","Q852","Q855B"), "splita==2",splits))%>%
  mutate(splits=ifelse(without_unederscore%in%c("Q201B_31", "Q211B","Q604A_6","Q604A_7","Q7141A","Q514"),"splita==1",splits))%>%
  filter(without_unederscore%nin%c("Q604A_6","Q604A_7"))

splitsa_1 =splits_frame%>%
  filter(splits%in%c("splita==1"))
splitsa_2  =splits_frame%>%
  filter(splits%in%c("splita==2"))%>%
  mutate(without_unederscore=ifelse(without_unederscore=="Q605B_2","Q605B",without_unederscore))%>%
  filter(without_unederscore%nin%c("Q201C_27","Q201B_37","Q604B_6","Q604B_7","Q605B_1","Q605B_2","Q605B_3","Q605B_4"))

check_splitsA= function(data, split, variable){
  data%>%
    filter_at(c(variable), any_vars(.%in%c(NA)))%>%
    filter(splita==split&country%in%c(7,15,22,13))%>%
    select(id, country, variable, splita, splitb, Q1012, Q1012A)
}

check_splitsB= function(data, split, variable){
  data%>%
    filter_at(c(variable), any_vars(.%in%c(NA)))%>%
    filter(splitb==split&country!=9&country!=10)%>%
    select(id, country, variable, splita, splitb, Q1012, Q1012A)
}


splitsAis1_NAS = map(unlist(splitsa_1$without_unederscore), ~check_splitsA(data = abv, split = 1, variable = .x))%>%
  set_names(unlist(splitsa_1$without_unederscore))
splitssAis2_NAS = map(unlist(splitsa_2$without_unederscore), ~check_splitsA(data = abv, split = 2, variable = .x))%>%
  set_names(unlist(splitsa_2$without_unederscore))

splits_names = list(splitsa_1$without_unederscore,splitsa_2$without_unederscore)%>%
  flatten()


fix_splits = list(splitsAis1_NAS,splitssAis2_NAS)%>%
  flatten()%>%
  set_names(splits_names)


for (i in seq_along(fix_splits)) {
  tib = fix_splits[[i]]
  if (dim(tib)[1]>1000){
    print(names(fix_splits)[[i]])
    print(i)
}}




fix_splits[["Q204B_13"]]=fix_splits[["Q204B_13"]]%>% ##needs to be filtered by Yemen
  filter(country!=22)
##omit(fix_splits[[8]])

fix_splits[["Q601_9"]] =  fix_splits[["Q601_9"]]%>% ##mquestion failed in Palestine?? Question not asked in Yemen, Questionarre needs to be updated 
  filter(country%nin%c(15, 22))



fix_splits[["Q701C_2"]]=fix_splits[["Q701C_2"]]%>%###was this question askedf in palestine?? 
  filter(country%nin%c(22,15))

fix_splits[["Q701C_4"]]=fix_splits[["Q701C_4"]]%>%###was this question askedin palestine?? 
  filter(country%nin%c(22, 15))

fix_splits


sum(map_dbl(fix_splits, nrow))


