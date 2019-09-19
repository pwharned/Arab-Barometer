
#for every country, for every variable, there should be 


  
  split_listA_is_on1 = map(c(paste("Q108", c(1,2,3,4), sep = "_"), paste("Q201B", c(6,13,20,31,12), sep = "_"), paste("Q204B", c(13,15), sep = "_"), 
                         paste("Q204C", c(13,15), sep = "_"), "Q211B", "Q301A", "Q514","Q601_9", paste("Q604A", c(1,3), sep = "_"), 
                         paste("Q604B", c(1,3), sep = "_"), paste("Q701C", c(2,4), sep = "_"), "Q703","Q7141A"), sym)
  
  split_listA_is_TWO = map(c(paste("Q201C", c(37, 38, 39, 32, 40), sep = "_"), "Q211C", "Q301B", "Q514A", "Q852", "Q853A", "Q853B", "Q854","Q855A","Q855B","Q601_9A",
                         paste("Q605A", c(1,2,3,4), sep = "_"), "Q605B", paste("Q701C", c(5,6), sep = "_"), "Q705", "Q7141B", "Q1017"), sym)

  
test  = abv%>%
  group_by(country)%>%
  nest()%>%
  mutate(tables = map(data, ~group_by(., splita)%>%summarise_all(mean, na.rm=TRUE)))%>%
  mutate(tables = map(tables, ~gather(.x , "Splita", "response")))

Country_names = names(val_lab(abv$country))[-c(11,12,9)]


split_listA_is_on1%>%
  map(nice_function)%>%
  print(100)


nice_function = function(x){
  x= enquo(x)
    group_by(abv,!!x)%>%
    count(splita)
}

    

test_list%>%
  map(nice_function)





    