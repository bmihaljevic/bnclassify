fit_beta<-function(input,parents,Class,variable){
  
  dataFrame<-data.frame(input)
  valueClass<-unique(dataFrame[Class])
  dataFrame <- as.data.frame(lapply(dataFrame, as.numeric))
  formula<-paste(variable,"~")
  
  for (i in 1:length(parents)){
    
    if (parents[i:i]!=Class){
      formula<-paste(formula,parents[i:i])#generate the formula
    }
    
    i=i+1
    if( is.na(parents[i:i])){ #no more parent variable
      #obtain differet values of the class
      classPosition<-grep(Class,colnames(dataFrame),value=F)#position of the property class in the data frame
      
      for (k in 1:nrow(valueClass)){
        #generate subset
        data <- subset(dataFrame, dataFrame[classPosition] == k)#subset according to the value of the class
        
        #calculate  coeficients
        if ((length(parents)==1)&&(parents== Class)){
          formula<-paste(paste(variable,"~"),parents)
          e<-lm(formula,data)$coef[1]
        }
        else{
          e<-lm(formula,data)$coef
        }        
        
        #generate  result
        if (k==1){
          result<-cbind(e)
          newRowName<-as.character(valueClass[k,1])
        }
        else{
          result<-cbind(result,e)
          newRowName<-c(newRowName,as.character(valueClass[k,1]))
        }
        
      }
      colnames(result)<-newRowName
      return (result)
    }
    
    else{#if there are more parent variable, continue adding parent to the formula
      if (parents[i:i]!=Class){
        formula<-paste(formula,'+')
        
      }
    }
    
  }
  
} 
lp_implement_clg <-function(bnc_dag,dataset, smooth){
  bnc_dag<-families(bnc_dag)
  bnc_fit_clg<-list()
  
  for (i in 1:(length(bnc_dag)-1)){
    
    Parents<-bnc_dag[[i]][2:length(bnc_dag[[i]])]
    variable<-bnc_dag[[i]][1]
    bnc_fit_clg[[variable]]<-(fit_beta(dataset,Parents,bnc_dag[[length(bnc_dag)]],variable))
    
  }
  bnc_fit_clg<-structure(bnc_fit_clg,class='bnc_fit_clg')
  return(bnc_fit_clg)
}
has_continuous_features <- function() {
 #  Return true if at least one feature is continous
}