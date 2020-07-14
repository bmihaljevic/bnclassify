#' learn the coefficients and desviations of a Bayesian network structure
#'
#' More ditailed description
#'
#' @param x The bnc_dag object. The Bayesian network classifier structure.
#' @param dataset The data frame.The data frame from which to learn coefficients.
#'
#' @return bnc_fit_clg. bnc_fit_clg is the object that contains coefficients and desviations of each node in the structure
#'
#' @details GaussianImplement learns the coefficients and desviations of each node in the bayesian network structure by using the function lm().
#'
#' GaussianImplement only returns the coefficients and desviations for numerical node. In case of categorical node, it will return a error information.
#'
#' When the parents of the node are categorical + numerical, it will returns the coefficients and desiations by using the numerical parents based on the different combination of categorical parents.
#' For example:
#'
#' We have a node speed, the parents are velocity(numeric), wheel(categorical,4 levels) and accelerator(categorical, 2 level).
#'  The node speed will have 8 different combination of subest: lm(speed~velocity,subset).
#'
#' @examples
#'structure<-tan_cl('Species',as.data.frame(lapply(iris,as.factor)))
#'plot(structure)
#'x<-GaussianImplement(structure,iris)

#'x$Sepal.Width$coef$setosa # get the coefficient of the node 'Sepal.Width' using the subset filtered by the combination Species=setosa

#'x$Sepal.Width$sd$setosa # get the desviations of the node 'Sepal.Width' using the subset filtered by the combination Species=setosa
#' @export

GaussianImplement<-function(x,dataset){
  #result_check<-check_continuos_variable(dataset)
  
  # all parents varieble are categorical then stop
  #if(result_check==FALSE){
  #params <- 'Error: the dataset does not have continuous variables'
  # return(bn)
  #}
  stopifnot(check_continuos_variable(dataset))
  # numeric + categorical variable
  #else{
  params <- families2coef(families(x), dataset = dataset)
  x$params<-params
  class(x) <- c('bnc_bn',class(x),'bnc_fit_clg')
  return(x)
  #}
}

check_continuos_variable <- function(dataSet) {
  #   Check dataset has continuous variable
  for (i in 1:ncol(dataSet)){
    if (class(dataSet[,i])=='numeric' || class(dataSet[,i])=='integer' ){
      return(TRUE)
    }
  }
  return(FALSE)
}

Categorical_Numeric_list <- function(parents,dataset,Class){
  # separate the parenst node in categorical list and numeric list
  list<-list(factor=NULL,numeric=NULL)
  for (i in 1:length(parents)){
    if (class(dataset[,parents[i:i]])=='numeric' ||class(dataset[,parents[i:i]])=='integer'  ){
      list$numeric<- append(list$numeric,parents[i:i])
    }
    else {
      list$factor<-append(list$factor,parents[i:i])
    }
  }
  return(list)
}


generate_formula <- function(variable,list){
  #get the formula that will be used in function lm
  formula<-paste(variable,"~")
  for(i in 1:length(list$numeric)){
    formula<-paste(formula,list$numeric[i])
    if (i != length(list$numeric)){
      formula<-paste(formula,'+')
    }
  }
  return(formula)
}

get_combination<-function(categorical,dataset){
  #get different combinations of categorical variable
  
  #generate the chain of the categorical variable
  for(i in 1:length(categorical)){
    if(i==1){
      list<-unique(dataset[categorical[i:i]])
      label<-as.vector(unlist(unique(dataset[categorical[i:i]])))
    }
    else{
      list<-append(list,unique(dataset[categorical[i:i]]))
      label<-append(label,as.vector(unlist(unique(dataset[categorical[i:i]]))))
    }
  }
  res<-t(combn(label,length(categorical)))# obtain all type of combinations
  #filter the combinations
  new1=''
  for (i in 1:nrow(res)){
    for (j in 1:length(list)){
      
      #first filter: avoid repetition of classes that come from the same category variable
      if(j!=1){
        for (k in 1:(j)){
          if((res[i,j] %in% list[[k]])==TRUE){
            break
          }
        }
      }
      #second filter
      if ((res[i,j] %in% list[[j]]) == FALSE){
        break
      }
      #third filter: avoid the repetition of the combinations
      if(j==length(list)&&res[i,1] %in% list[[1]]){
        new1<-rbind(new1,res[i,])
        new1<-unique(new1)
      }
    }
  }
  return(new1[-1,])
}

get_coeficiet<-function(combination,dataset,list,formula,variable){
  #depending on the subset, get the coefficient
  coef<-NULL
  sd<-NULL
  for (i in 1:nrow(combination)){
    data<-dataset
    for (j in 1:ncol(combination)){
      tryCatch({ data<-subset(data,data[list$factor[j]]==combination[i,j])},error=function(e){})
    }
    
    colapsed<-paste(combination[i,1:ncol(combination)],collapse=",")
    if (nrow(data)==0){
      coef<-cbind(coef,0)
      sd<-cbind(sd,0)
    }
    else{
      position <- gregexpr("~",formula)
      check_formula <- substr(formula,(position[[1]]),(position[[1]]+3))
      if (check_formula=='~  +'){
        coef<-cbind(coef,mean(data[variable][,1]))
        sd<-cbind(sd,sd(data[variable][,1]))
      }
      else{
        lm_result<-lm(formula,data)
        coef<-cbind(coef,t(t(lm_result$coef)))
        sd<-cbind(sd,t(t(sqrt(sum(lm_result$residual^2)/(nrow(data)-length(list$numeric)-1)))))
        if(NA %in% coef){
          name <- rownames(coef)[which(is.na(coef), arr.ind = TRUE)[1,1]]
          cat('the variable',name,'is highly correlated with others variables.')
          stop('The program cannot continue unless it is deleted')
        }
      }
    }
    colnames(coef)[i] <- colapsed
    colnames(sd)[i] <- colapsed
  }
  x<-list(coef=data.frame(coef),sd=data.frame(sd))
  return(x)
}


fit_beta<-function(dataset,parents,Class,variable){
  #get the coefficients
  dataFrame<-data.frame(dataset)
  list_cat_num<-Categorical_Numeric_list(parents,dataFrame,Class)
  formula_lm <- generate_formula(variable,list_cat_num)
  
  # fit
  if (!(Class %in% list_cat_num$factor)){
    list_cat_num$factor<-append(list_cat_num$factor,Class)
  }
  combination<-get_combination(list_cat_num$factor,dataset)
  if (class(combination)!='matrix'){
    combination<-(as.matrix(combination))
  }
  res<- get_coeficiet(combination,dataset,list_cat_num,formula_lm,variable)
  return(res)
}

families2coef <- function(families,dataset){
  # obtain the coefficients for each node
  # the result is a object bnc_fit_clg
  bnc_fit_clg<-list()
  
  for (i in 1:(length(families)-1)){
    Parents<-families[[i]][2:length(families[[i]])]
    variable<-families[[i]][1]
    class<-families[[length(families)]]
    if(class(dataset[,variable])!='numeric' && class(dataset[,variable])!='integer'){
      bnc_fit_clg[[variable]]<-c('Error:not meaningful for node as factor')
    }
    else{
      bnc_fit_clg[[variable]]<-(fit_beta(dataset,Parents,class,variable))
    }
  }
  
  bnc_fit_clg<-structure(bnc_fit_clg,class='bnc_fit_clg')
  return(bnc_fit_clg)
}
