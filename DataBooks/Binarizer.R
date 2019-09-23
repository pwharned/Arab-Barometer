setwd("/Users/pharned/Documents/Arab-Barometer/DataBooks")

Backwards_Coding = c("Q424","Q423_3", "Q423_2","Q423_1","Q512","Q511","Q855A","Q855B","Q609A","Q511","Q512","Q513")

binaraizer = function(Variable, dataframe){
  print(Variable)
  if(Variable %in% Backwards_Coding){
    i = length(val_lab(dataframe[[Variable]]))
    operation = function(x){x-1}
  }
  else{
    i=1
    operation = function(x){x+1}
  }
  Recode_list = c()
  Variable_Name = paste(substitute(Variable), ":", collapse = "")
  
  Variable  = dataframe[[Variable]]
  
  Lab = subset(val_lab(Variable),val_lab(Variable)%nin%c(90:100))
  Cut_off = length(Lab)+1
  output = list()
    while(length(na.omit(Recode_list))!=Cut_off){
        Recode_list[i]=i
        New_Variable_Name = paste(Variable_Name, "=", paste(na.omit(names(Lab)[Recode_list]), collapse  = " or "), collapse = "_")
        New_Variable = ifelse(Variable%in%Recode_list,1,ifelse(Variable %in%c(NA),NA,0))
        output[[i]]= assign(New_Variable_Name, New_Variable)
        names(output)[[i]]=New_Variable_Name
        i = operation(i)
        print(i)

    }
  output= output[!sapply(output, is.null)]
  
  if(grepl("once", names(output))==TRUE|grepl("religious",names(output))==TRUE){
    print(paste("Once/Religious",  length(output)))
    print(paste("Returning second Element which is ",names(output)[2]))
    out = output[2]
  }

  else if(length(output)%in%c(1,2,3,8)){
    print(paste("Length of the Output is ",  length(output)))
    print(paste("Returning first Element which is ",names(output)[1]))
    out = output[1]
  }

  else if(length(output)%in%c(4,5,6)){
    print(paste("Length of the Output is ",  length(output)))
    print(paste("Returning 2rd element which is ",names(output)[2]))
    out = output[2]
  }else if (length(output)%in%c(7)){
    print(paste("Length of the Output is ",  length(output)))
    print(paste("Returning fifth Element which is ",names(output)[5]))
    out = output[5]
  }
 
}


thisfunction = function (variable, dataframe){
  print(variable)
  variable = dataframe[[variable]]
  Lab = val_lab(variable)[val_lab(variable)%nin%c(90:99)]
  Seq = seq(Lab)
  print(Seq)
}  
  
  map(Backwards_Coding, thisfunction, dataframe = abv_ara)

  