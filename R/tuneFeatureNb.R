#' Selecting the optimal number of features
#'
#' \code{tuneFeatureNb} iterates through increasing feature numbers to calculate kappa values which represents the performance of the model computed with the given features.  We recommand to save the object containing the optimal number of features for the following steps.
#'
#' @param data data.frame containing the training set
#' @param cl integer indicating the column number corresponding to the response vector that classify positive and negative regions (default = 1)
#' @param feature.ranking List of ordered features.
#' @param kernel SVM kernel, a character string: "linear" or "radial". (default = "radial")
#' @param cost The SVM cost parameter for both linear and radial kernels. If NULL (default), the function \code{mcTune} is run.
#' @param gamma The SVM gamma parameter for radial kernel. If radial kernel and NULL (default), the function \code{mcTune} is run.
#' @param step.nb Number of features to add at each step (default = 10)
#' @param numcores Number of cores to use for parallel computing (default: the number of available cores in the machine - 1)
#' @param file.prefix A character string that will be used as a prefix followed by "_kappa_measures.png" for the result plot file. If it is NULL (default), no plot is returned
#' @param valid.times Integer indicating how many times the training set will be split for the cross validation step (default = 10). This number must be smaller than positive and negative sets sizes.
#' @return A list with two objects.
#' \item{performance}{2-columns data frame. first column correspond to the number of tested features, second column contains the corresponding kappa value}
#' \item{best.feature.nb}{Integer corresponding to the number of features producing the model with the highest kappa value}
#' @examples
#' data(crm.features)
#' data(feature.ranking)
#' cost <- 1
#' gamma <- 1
#'  #feature.nb.obj <- tuneFeatureNb(data.granges=crm.features,
#'  #    feature.ranking=feature.ranking, kernel='linear', cost=cost,gamma=gamma,
#'  #    file.prefix = "test")
#'  #names(feature.nb.obj)

tuneFeatureNb = function(data, cl = 1, feature.ranking, step.nb = 10, valid.times =
                           10, cost = NULL, gamma = NULL, kernel = "linear", numcores=ifelse(.Platform$OS.type == "windows", 1,  parallel::detectCores() - 1), file.prefix = NULL) {

x=data[,-cl]
y=data[,cl]

feature.nb.vector = seq(from = step.nb, to = (ncol(x) - 1), by = step.nb)

feature.nb.tuner = FeatureNbTuner$new(
  x = x, y = y, feature.ranking = feature.ranking, feature.nb.vector = feature.nb.vector, valid.times =
                           valid.times, cost = gamma, gamma = gamma, kernel = kernel, numcores = numcores,file.prefix = file.prefix)
    return(feature.nb.tuner)
}
