setwd("/Users/pharned/Documents/Arab-Barometer/DataBooks")
library(list)
source('~/Documents/Arab-Barometer/Wraps/Functions.R')

abv = abv_en



binaraizer = function(Variable, dataframe){
  print(Variable)
  Variable_Name = paste(substitute(Variable), ":", collapse = "")
  
  Variable  = dataframe[[Variable]]
  
  Lab = subset(val_lab(Variable),val_lab(Variable)%nin%c(90:100))
  Recode_Value = Lab[1]
  Recode_list = c()
  Cut_off = length(Lab)
  i=1
  output = list()
    while(length(Recode_list)<Cut_off){
        Recode_list[i]=i
        New_Variable_Name = paste(Variable_Name, "=", paste(names(Lab)[Recode_list], collapse  = " or "), collapse = "_")
        New_Variable = ifelse(Variable%in%Recode_list,1,0)
        output[[i]]= assign(New_Variable_Name, New_Variable)
        names(output)[[i]]=New_Variable_Name
        i=i+1
      
        #assign(New_Variable_Name, New_Variable)
    
    }
  output= output[!sapply(output, is.null)]
  
  if(grepl("once", names(output))==TRUE|grepl("religious",names(output))==TRUE){
    print(paste("Once/Religious",  length(output)))
    print(paste("Returning second Element which is ",names(output)[2]))
    out = output[2]
  }
  else if(length(output)==6){
    print(paste("Length of the Output is ",  length(output)))
    print(paste("Returning fifth Element which is ",names(output)[5]))
    out = output[5]
  }

  else if(grepl("political divide", names(output))==TRUE){
    print(paste("Political Divide",  length(output)))
    print(paste("Returning first Element which is ",names(output)[1]))
    out = output[1]
  }
  
  else if(length(output)%in%c(1,2,3,8)){
    print(paste("Length of the Output is ",  length(output)))
    print(paste("Returning first Element which is ",names(output)[1]))
    out = output[1]
  }

  else if(length(output)%in%c(4,5)){
    print(paste("Length of the Output is ",  length(output)))
    print(paste("Returning 2rd element which is ",names(output)[2]))
    out = output[2]
  }else{
    print(paste("Length of the Output is ",  length(output)))
    print(paste("Returning last Element which is ",names(output)[length(output)]))
    out = output[length(output)]
  }
 
}


  
  
  

  