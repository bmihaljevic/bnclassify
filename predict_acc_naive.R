#' Estimate the accuracy with Naive Bayes or Tree Augment Bayes
#'
#' Compute the conditional probability and estimate the predicted label of a categorical class given idependent predictor variables with Naive Bayes or Tree Augment Bayes
#'
#'
#' More ditailed description
#'
#' @param structure The bnc_dag object. The Bayesian network classifier structure.
#' @param dataset The data frame.
#' @param gaussianParams the gaussian parameters. It must be a bnc_fit_clg object.
#' @param prob TRUE: the function returns the conditional probability. FALSE: it returns the predicted label
#' @return prediction. the conditional probability of the class or predicted label depending on the value of prob
#'
#' @details predict_acc_naive() returns predicted label or conditional probabilities after computing the conditioal a-posterior probabilities given
#' the data specified by dataset and the params obtained from the function Beta_Implement().
#'
#' the conditional a-posterior probabilities are computed by using the chain rules of the Bayesian network:
#'
#'     p(yi|x)=(p(x|yi)*p(yi))/p(x)
#'
#' y is the class that we want to predict and i is the diferent labels of the class
#'
#' x is the variables that are used to compute the prediction. x is obtained using the 'to' option of the input variable 'structure'
#'
#' -p(yi|x): it is calculated using the Probability density function. In case x are multidimensional: being naive Bayesian, all the random variables are independent,
#' so it will multiply the probability density of each node
#'
#' -p(yi):it is the probability of the different labels in the class
#'
#' -p(x):for the same sample x, p(x) is a constant so the prediction can be computed without taking into account this value
#' @examples
#' library(bnclassify)
#'
#'structure<-nb('Species',as.data.frame(lapply(iris,as.factor)))
#'gaussianParams<-BetaImplement(structure,iris)
#'acc<-predict_acc_naive(iris,structure,gaussianParams)
#'#Using the accuracy function of the package 'bnclassify'
#'accuracy(acc, iris$Species)
#'
#'#'structure<-tan_cl('Species',as.data.frame(lapply(iris,as.factor)))
#'gaussianParams<-BetaImplement(structure,iris)
#'acc<-predict_acc_naive(iris,structure,gaussianParams)
#'#Using the accuracy function of the package 'bnclassify'
#'accuracy(acc, iris$Species)
#' @export
#'
predict_acc_naive <- function(data,structure,gaussianParams,prob=FALSE){
  #get the conditional probability of each node
  conditionalProbability_List<-local_predict(data,structure,gaussianParams)
  #get the probability pf the class
  prior<-get_prior(data,structure)
  #prediction: obtain the prediction
  prediction<-prediction(conditionalProbability_List,prior,prob)
  return(prediction)
}
get_likelyhood<-function(var,params,class){
  likelihood <- as.numeric((1 / (params$sd[class]* sqrt(2 * pi))) * exp(-(var[1] - var[2])^2 / (2 * params$sd[class]^2)))
  }

get_conditionalProbability <- function(data,structure,variable,gaussianParams){
  #calculate the conditional probability of the corresponding node
  classType <- levels(data[,structure$.class])
  #define conditional probability matrix
  conditionalProbability<-matrix(nrow= nrow(data), ncol=length(classType))
  colnames(conditionalProbability)<-(classType)
  for (i in 1:length(classType)){
  #compute average
    x <- gaussianParams$coef[classType[i]]
      for (j in rownames(x)){
        if (j == '(Intercept)'||j=='1'){average = x[1,1] }
        else{average = average + (x[j,1]*data[,j])
        }
      }
  #compute conditional probability of each class
    conditionalProbability[,i] <- apply(as.matrix( cbind(data[variable],average)),1,get_likelyhood,gaussianParams,classType[i])
  }
 return(conditionalProbability)
}

local_predict <- function(data,structure,gaussianParams){
  #obtain the name of the childrens
  edges<-as.data.frame(structure$.dag$edges)
  to <- levels(edges[which(edges$from==structure$.class),2])
  #define conditional probability list
  conditionalProbability_List <- list()
  for (j in 1:length(to)){
    gd<-get_conditionalProbability(data,structure,to[j],gaussianParams[[to[j]]])#conditional probability of the corresponding node
    conditionalProbability_List<-append(conditionalProbability_List,list(gd))## add
    names(conditionalProbability_List)[j] <- to[j]
    }
  return(conditionalProbability_List)
}

get_prior <- function(data,structure){
#calculate the probability of each member in the class
classType <- levels(data[,structure$.class])
#define prior matrix
prior<-matrix(nrow= 1, ncol=length(classType))
colnames(prior)<-(classType)
for (i in 1:length(classType)){
  prior[,i] = sum(data[,structure$.class] == classType[i]) / length(data[,structure$.class])
}
return(prior)
}

normalization <- function(probability){
  #normalization
  row<-nrow(probability)
  col<-ncol(probability)
  x_min_temp<-apply(probability,2,min)
  x_min<-matrix(rep(x_min_temp,row),byrow=TRUE,ncol=col)        #??Ҫ??????????????
  x_extreme_temp<-apply(probability,2,max)-apply(probability,2,min)
  x_extreme<-matrix(rep(x_extreme_temp,row),byrow=TRUE,ncol=col)#??Ҫ??????????????
  probability<-abs(probability-x_min)/x_extreme
  return(probability)
}

naive <- function(gaussianDensity){
  tmp <- 1
  for (i in 1:length(gaussianDensity)){
    tmp<-gaussianDensity[[i]]*tmp
  }
  return(tmp)
}

prediction <- function(gaussianDensity,prior,prob){
  #naive structure: node independ
  naiveDensity <- naive(gaussianDensity)
  prior<-matrix(prior,nrow=nrow(naiveDensity),ncol=ncol(naiveDensity),byrow=T)
  #chai rule:
  probability <- naiveDensity*prior
  #normalization
  normalizedProb <- normalization(probability)
  #predic the label for each row: The corresponding label with the highest probability for each row
  predictedLabel <- apply(normalizedProb, 1, function(t) colnames(normalizedProb)[which.max(t)])
  if(prob ==FALSE){
  return(factor(predictedLabel))}
  else{return(as.data.frame(normalizedProb))}
  }
