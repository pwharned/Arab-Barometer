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
  Cut_off = round((length(Lab)+.1)/2)
  i=1
  output = list()
    while(length(Recode_list)<Cut_off){
        Recode_list[i]=i
        New_Variable_Name = paste(Variable_Name, "=", paste(names(Lab)[Recode_list], collapse  = ","), collapse = "_")
        i=i+1
        New_Variable = ifelse(Variable%in%Recode_list,1,0)
        output[[i]]= assign(New_Variable_Name, New_Variable)
        names(output)[[i]]=New_Variable_Name
      
        #assign(New_Variable_Name, New_Variable)
    
    }
  output= output[!sapply(output, is.null)]
  print(names(output[length(output)]))
  print("Values to Recode are ", Recode_list)

  if(length(output)==2){
    print(paste("Length of the Output is ",  length(output)))
    print(paste("Returning second Element which is ",names(output)[2]))
    out = output[2]
    return(out)
    
  }
  if(length(output)==3){
    print(paste("Length of the Output is ",  length(output)))
    print(paste("Returning 3rd element which is ",names(output)[2]))
    out = output[2]
    return(out)
  }
  if(length(output)<1){
    
    }else{
    print(paste("Length of the Output is ",  length(output)))
    print(paste("Returning last Element which is ",names(output)[length(output)]))
    out = output[length(output)]
    return(out)
  }
 
  
  
}

binaraizer(Q409, abv)


  eval(quote(Q409), abv)

y = function(x, dataframe){
  eval(quote(x), dataframe, abv)
}  
y(Q409, abv)  
  
  
  

  