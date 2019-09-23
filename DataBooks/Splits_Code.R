


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

