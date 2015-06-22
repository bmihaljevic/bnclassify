#' Algorithms for learning Bayesian network classifiers from data.
#' 
#' @docType package
#' @name bnclassify
#' @importFrom stats predict
#' @references Bielza C and Larra\~{n}aga P (2014), Discrete Bayesian network 
#'   classifiers: A survey. \emph{ACM Computing Surveys}, \bold{47}(1), Article
#'   5.
#'   
#'   Friedman N, Geiger D and Goldszmidt M (1997). Bayesian network classifiers.
#'   \emph{Machine Learning}, \bold{29}, pp. 131--163.
NULL

#' Congress Voting Data Set.
#' 
#' @source \url{http://sourceforge.net/projects/weka/files/datasets/UCI and StatLib/uci-20070111.tar.gz}
#' @format A \code{data.frame} with 17 columns and 435 rows.
#' @docType data
#' @name voting
NULL

#' Car Evaluation Data Set.
#' 
#' @source \url{http://sourceforge.net/projects/weka/files/datasets/UCI and StatLib/uci-20070111.tar.gz}
#' @format A \code{data.frame} with 7 columns and 1728 rows.
#' @docType data
#' @name car
NULL

#' Learns Bayesian network classifiers in a wrapper fashion.
#' 
#' bsej is the backward \emph{sequential elimination and joining} algorithm
#' whereas fssj is the \emph{forward sequential selection and joining}
#' algorithms for learning a semi-naive Bayes classifier (Pazzani, 1996). tanhc
#' Learns a tree augmented naive Bayes with a greedy hill-climbing search. tanhc
#' is the super-parent variant of tanhc.
#' 
#' @name wrapper
#'   
#' @references Pazzani M (1996). Constructive induction of Cartesian product 
#'   attributes. In \emph{Proceedings of the Information, Statistics and 
#'   Induction in Science Conference (ISIS-1996)}, pp. 66-77
#'   
#'   Koegh E and Pazzani M (2002).Learning the structure of augmented Bayesian 
#'   classifiers. In \emph{International Journal on Artificial Intelligence 
#'   Tools}, \bold{11}(4), pp. 587-601.
#' @param class A character. Name of the class variable.
#' @param epsilon A numeric. Minimum absolute improvement required to keep 
#'   searching.
NULL

#' Learns a tree augmented naive Bayes classifier (TAN).
#' 
#' Learns a one-dependence Bayesian classifier using Chow-Liu's algorithm. The 
#' structure is learned so that either likelihood, the BIC or AIC scores are 
#' maximized; the first option corresponds to the well-known tree augmented 
#' naive Bayes (Friedman et al., 1997).
#' 
#' Edges with negative BIC or AIC weights are blacklisted from the final 
#' structure. The structure thus might be a forest rather than a tree.
#' 
#' @name tan_chowliu
#'   
#' @param blacklist A character matrix. Edges that may be blacklisted from the
#'   resulting structure.
#' @param root A character. The feature to be used as root of the augmenting 
#'   tree. Only one feature can be supplied, even in case of an augmenting 
#'   forest. This argument is optional.
#' @param score character The score to be maximized. \code{'loglik'},
#'   \code{'bic'}, and \code{'aic'} return the maximum likelihood, maximum BIC
#'   and maximum AIC tree/forest, respectively.
#'   
#' @references Friedman N, Geiger D and Goldszmidt M (1997). Bayesian network 
#'   classifiers. \emph{Machine Learning}, \bold{29}, pp. 131--163.
NULL