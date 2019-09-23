
abv= abv_en

#Q104A Lebanon, Yemen if Q104A = sysmis and q104=1 --> 99 Refused. 
#Q104C  Yemen if Q104C=sysmis and q104=1 --> 99 Refused. Yemen incorrect filter. some answers for q104<>1
abv = abv%>%
  mutate(Q104A = ifelse(Q104A%in%c(NA)&Q104==1&country%in%c(10,22), 99, Q104A))%>%
  mutate(Q104A = ifelse(Q104A%nin%c(NA)&Q104!=1&country%in%c(10,22), NA, Q104A))%>%
  mutate(Q104C = ifelse(Q104C%in%c(NA)&Q104==1&country%in%c(10,22), 99, Q104C))%>%
  mutate(Q104C = ifelse(Q104C%nin%c(NA)&Q104!=1&country%in%c(10,22), NA, Q104C))



#Yemen incorrect filter. some answers for q104<>1


#Q108_x: Palestine,Yemen, if Q108_x=sysmis and splita=1 -> 99. 
abv = abv%>%
  mutate_at(vars(starts_with("Q108")), function(x)x=ifelse(x%in%c(NA)&abv$splita==1&abv$country%in%c(15,22), 99, x))


#Q204B_13 Palestine if Q204B_13=sysmis and splita=1 -> 99
#Q201B_31 Yemen  if Q201B_31=sysmis and splita=1 -> 99
#Q201B_6 Yemen if Q201B_6=sysmis and splita=1 -> 99
#Q201B_12 Iraq,Yemen if Q201B_12=sysmis and splita=1 -> 99
#Q201B_13 Yemen  if Q201B_13=sysmis and splita=1 -> 99
#Q201B_20 Yemen  if Q201B_20=sysmis and splita=1 -> 99


#Q201C_27 Iraq if Q201C_37=sysmis  and splita=2 -> 99

#Q201B_37 Yemen  if Q201B_37=sysmis and splita=2 -> 99
#Q201C_40 Iraq if Q201C_40=sysmis  and splita=2 -> 99



abv = abv%>%
  mutate_at(vars(starts_with("Q201B")), function(x)x=ifelse(x%in%c(NA)&abv$country%in%c(22,15,7)&abv$splita==1, 99, x))%>%
  mutate_at(vars(starts_with("Q201C")), function(x)x=ifelse(x%in%c(NA)&abv$splita==2&abv$country!=9, 99, x))

#Q211 Lebanon,Palestine if Q211=SYSMIS and Q210 in (1,2,3) -> 99. Libya dirty code 0->sysmis (filter). 
abv= abv%>%
  mutate(Q211 = ifelse(Q211%in%c(NA)&Q210%in%c(1,2,3), 99, Q211))

###Palestine,Yemen: incorrect filter, some answers for q210=not at all, dk,refused


abv = abv%>%
  mutate(Q211 = ifelse(Q211%nin%c(NA)&Q210%nin%c(1,2,3), NA, Q211))


#Q211A Lebanon, Palestine if Q211A=sysmis and Q210 in (1,2,3) -> 99 


abv= abv%>%
  mutate(Q211A= ifelse(Q211A%in%c(NA)&Q210%in%c(1,2,3), 99, Q211A))

#Kuwait,Yemen no filtering. Q211B Yemen, if Q211B=sysmis and splita=1 -> 99 plus 1 case incorrect filter. Kuwait no filtering

abv= abv%>%
  mutate(Q211B=ifelse(Q211B%in%c(NA)&splita==1, 99, Q211B))


#Q211C Iraq,Yemen if Q211C=sysmis and splita=2 -> 99. Kuwait no filtering 

abv= abv%>%
  mutate(Q211C=ifelse(Q211C%in%c(NA)&splita==2, 99, Q211C))
#Q301A,Palestine, Yemen if Q301A=sysmis and splita=1 ->99.


abv = abv%>%
  mutate(Q301A=ifelse(Q301A%in%c(NA)&splita==1, 99, Q301A))

##Q301B,Palestine  if Q301B=sysmis and splita=1 ->99. 
#Q301B should be NA for splita ==1, because it was only asked of splita==2(see questionaire)

#Q301A. Yemen incorrect filter. some answers for splita=2. Kuwait no filter.

abv = abv%>%
  mutate(Q301A = ifelse(Q301A%nin%c(NA)&splita==2&country==22, NA,Q301A ))

#Q301B Yemen incorrect filter. some answers for splita=1. 

abv = abv%>%
  mutate(Q301B = ifelse(Q301B%nin%c(NA)&splita==1&country==22, NA,Q301B ))

#Q514, Yemen if Q514=sysmis and splita=1 ->99. 

abv = abv%>%
  mutate(Q514 = ifelse(Q514%in%c(NA)&splita==1&country==22, 99,Q514 ))
  
#Q514A, Iraq, Palestine, Yemen if Q514A=sysmis and splita=2 ->99. 
abv = abv%>%
  mutate(Q514A = ifelse(Q514A%in%c(NA)&splita==2&country %in%c(7,22,15), 99,Q514A ))




#Q604A_1 Iraq,Yemen if Q604A_1=sysmis and splita=1 -> 99 Refused
abv = abv%>%
  mutate(Q604A_1 = ifelse(Q604A_1%in%c(NA)&splita==1&country %in%c(7,22,15), 99,Q604A_1 ))


#Q604A_3 Iraq,Yemen if Q604A_3=sysmis and splita=1 -> 99 Refused
abv = abv%>%
  mutate(Q604A_3 = ifelse(Q604A_3%in%c(NA)&splita==1&country %in%c(7,22,15), 99,Q604A_3 ))

#Q701C_2 Q701C_4 Iraq if Q701C_xxx=sysmis and splita=1-> 99 refused

abv = abv%>%
  mutate(Q701C_2 = ifelse(Q701C_2%in%c(NA)&splita==1&country %in%c(7), 99,Q701C_2 ))%>%
  mutate(Q701C_4 = ifelse(Q701C_4%in%c(NA)&splita==1&country %in%c(7), 99,Q701C_4 ))
  



#Q514: Yemen incorrect filter. some answers for splita=2

abv = abv%>%
  mutate(Q514 = ifelse(Q514%nin%c(NA)&splita==2&country %in%c(22), NA,Q514 ))

#Q514A : Yemen incorrect filter. some answers for splita=1
abv = abv%>%
  mutate(Q514A = ifelse(Q514A%nin%c(NA)&splita==1&country %in%c(22), NA,Q514A ))

#Q602_4A Lebanon asked only to Muslim! No answers for 1 Strongly dislike!
  
abv = abv%>%
  mutate(Q602_4A=ifelse(Q602_4A%nin%(NA)&Q1012!=1, NA,Q602_4A))


#Q602_4B Filter?? SYSMIS=99? suposed to be filtered by religious sect
  
abv = abv%>%
  mutate(Q602_4B=ifelse(Q602_4B%nin%(NA)&Q1012!=2, NA,Q602_4B))



#Q604B_1 Iraq if Q604B_1=sysmis and splita=2 -> 99 refused, 


abv = abv%>%
  mutate(Q604B_1=ifelse(Q604B_1%in%(NA)&splita==2&country%in%c(15,22), 99,Q604B_1))
  
#Yemen incorrect filter.Q604B_1 some answers for splita=1 
abv = abv%>%
  mutate(Q604B_1=ifelse(Q604B_1%nin%(NA)&splita==1&country%in%c(15,22), NA,Q604B_1))

#Q604B_3 Iraq if Q604B_3=sysmis and splita=2 -> 99 refused, 
#I cant find these observations
#Yemen incorrect filter. some answers for splita=1
abv = abv%>%
  mutate(Q604B_3=ifelse(Q604B_3%nin%(NA)&splita==1&country%in%c(15,22,7), NA,Q604B_3))

#Q605 Lebanon, Asked only to Muslim - affects one observation
abv = abv%>%
  mutate(Q605=ifelse(Q605%nin%c(NA)&Q1012!=1, NA, Q605))


#Q605A_xxx,Q605B All countries, if Q605A_xxx=sysmis and splita=2-> 99 refused. True only of muslims in Iraq
abv = abv%>%
  mutate(Q605A_1=ifelse(Q605A_1%in%c(NA)&splita==2&Q1012==1&country!=9, 99,Q605A_1))%>%
  mutate(Q605A_2=ifelse(Q605A_2%in%c(NA)&splita==2&Q1012==1&country!=9, 99,Q605A_2))%>%
  mutate(Q605A_3=ifelse(Q605A_3%in%c(NA)&splita==2&Q1012==1&country!=9, 99,Q605A_3))%>%
  mutate(Q605A_4=ifelse(Q605A_4%in%c(NA)&splita==2&Q1012==1&country!=9, 99,Q605A_4))%>%
  mutate(Q605B=ifelse(Q605B%in%c(NA)&splita==2&Q1012==1&country!=9, 99,Q605B))%>%
  mutate(Q605=ifelse(Q605%in%c(NA)&splita==2&Q1012==1&country!=9, 99,Q605))

#Yemen incorrect filter. some answers for splita=2 - No need to recode as the whole sample received the quesiton
#  Q607_7 Lebanon, 
  
abv = abv%>%
    mutate(Q607_7 = ifelse(Q607_7%nin%c(NA)&Q1012!=1&Q1012A!=10, NA,Q607_7))

#Q701C _5 _6 Iraq if Q701C_xxx=sysmis and splita=2-> 99 refused
abv = abv%>%
    mutate(Q701C_5=ifelse(Q701C_5%in%c(NA)&splita==2&country!=9, 99,Q701C_5))%>%
    mutate(Q701C_6=ifelse(Q701C_6%in%c(NA)&splita==2&country!=9, 99,Q701C_6))
  
  

#Q703 Iraq,Yemen if if Q703=sysmis and splita=1 ->99 refused. Seems to be systematic programming issue. 
  
abv =   abv%>%
    mutate(Q703=ifelse(Q703%in%c(NA)&splita==1, 99, Q703))
  

#Yemen incorrect filter, some answers for splita=1
  
abv =   abv%>%
    mutate(Q703= ifelse(Q703%nin%c(NA)&splita==2&country!=9, NA, Q703))
  
  
#Q705 Iraq,Yemen if if Q705=sysmis and splita=1 ->99 refused. SPLITA ==2
  
abv =  abv%>%
    mutate(Q705 = ifelse(Q705%in%c(NA)&splita==2&country!=9,99, Q705))
  
# Yemen incorrect filter, some answers for splita=1
  
  
abv = abv%>%
    mutate(Q705=ifelse(Q705%nin%c(NA)&splita==1&country!=9, NA, Q705))
  
  
#Q7141A Iraq,Yemen if if Q7141A =sysmis and splita=1 ->99 refused. Kuwait no filter. 
  
abv =  abv%>%
    mutate(Q7141A = ifelse(Q7141A%in%c(NA)&splita==1, 99,Q7141A))
  
#Yemen, Palestine incorrect filter, some answers for splita=2  
  
abv = abv%>%
  mutate(Q7141A=ifelse(Q7141A%nin%c(NA)&splita==2&country!=9, NA, Q7141A))


#Q7141B Iraq,Yemen if if Q7141A =sysmis and splita=2 ->99 refused. 

abv = abv%>%
  mutate(Q7141B = ifelse(Q7141B%in%c(NA)&splita==2&country!=9, 99,Q7141B))

#Yemen, Palestine incorrect filter, some answers for splita=2

abv = abv%>%
  mutate(Q7141B=ifelse(Q7141B%nin%c(NA)&splita==1&country!=9, NA, Q7141B))


#Q851B Palestine,Yemen if Q851B=sysmis and Q851a!=1 -> 99 refused. 

abv = abv%>%
  mutate(Q851B = ifelse(Q851B%in%c(NA)&Q851A==1&country!=9, 99,Q851B))



#Palestine,Yemen incorrect filter. some answers for Q851A=refused, dk or no. 
abv = abv%>%
  mutate(Q851B = ifelse(Q851B%nin%c(NA)&Q851A!=1&country!=9, NA,Q851B))



#Q851C Yemen if Q851C=sysmis and Q851B=1 -> 99 refused. 
abv = abv%>%
  mutate(Q851C = ifelse(Q851C%in%c(NA)&Q851B==1&country!=9, 99,Q851C))
  


#Yemen incorrect filter. some answers for Q851B=refused, dk or no or INAP ( not sure what this means)

abv = abv%>%
  mutate(Q851C = ifelse(Q851C%nin%c(NA)&Q851B!=1&country!=9, NA,Q851C))

##Q852 Iraq,Morocco  if Q852=sysmis and splita=2 -> 99 refused. 
abv = abv%>%
  mutate(Q852 = ifelse(Q852%in%c(NA)&splita==2&country!=9, 99,Q852))



##Kuwait no filter. Yemen incorrect filter, some answers for splita=1
abv =abv%>%
  mutate(Q852 = ifelse(Q852%nin%c(NA)&splita==1&country!=9, NA,Q852))

#Q853A Morocco if splita=2 and Q1002=1 and q853A=sysmis ->99 Refused. 

abv = abv%>%
  mutate(Q853A= ifelse(Q853A%in%c(NA)&splita==2&Q1002==1&country==13, 99,Q853A))
  
#Iraq, Filter, some female have answered. 

abv = abv%>%
  mutate(Q853A= ifelse(Q853A%nin%c(NA)&splita==2&Q1002==2&country==7, NA,Q853A))
#Q854 Iraq, Morocco if Q854=sysmis and splita=2 ->99. 
abv = abv%>%
  mutate(Q854= ifelse(Q854%in%c(NA)&splita==2&country%in%c(7,13), 99,Q854))
  

#Kuwait no filter. Yemen, few incorrect answers for splita=1
abv = abv%>%
  mutate(Q854= ifelse(Q854%nin%c(NA)&splita==1&country!=9, NA,Q854))


#Q855A Iraq, Morocco if Q855A=sysmis and splita=2 ->99. Kuwait no filter. Yemen, few incorrect answers for splita=1

abv = abv%>%
  mutate(Q855A=ifelse(Q855A%in%c(NA)&splita==2, 99, Q855A))


#Yemen, few incorrect answers for splita=1
abv =abv%>%
  mutate(Q855A=ifelse(Q855A%nin%c(NA)&splita==1&country!=9, NA, Q855A))

#Q855B Iraq, Morocco if Q855B=sysmis and splita=2 ->99.  
abv = abv%>%
  mutate(Q855B=ifelse(Q855B%in%c(NA)&splita==2&country!=9, 99, Q855B))

#Yemen, few incorrect answers for splita=1
abv = abv%>%
  mutate(Q855B=ifelse(Q855B%nin%c(NA)&splita==1&country!=9, NA, Q855B))



##Recode goveranates in Kuwait with prefix 9000….1:etc


##Q1001B: Iraq,Libya, Morocco, Palestine, Tunisia Revise codes 98,99... are they 998,999?


##recode Q1018 if country ==Lebanon and Q1018 ==0, Q1018=NA


##Q1018: Lebanon: Sysmis assumed 0? Yes should be coded as 0 correct in our data set. 
abv = abv%>%
  mutate(Q1018=ifelse(Q1018%in%c(NA)&country ==10, 0,Q1018))

write_dta(abv, path = "/Volumes/GoogleDrive/Shared drives/Arab Barometer/AB5/Data/Release Data/September_16_Update.dta")
