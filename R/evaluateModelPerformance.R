#' Evaluate model performances
#'
#' \code{evaluateModelPerformance} function computes the precision and recall measures to evaluate the model through cross validation steps using \code{ROCR} package.
#'
#' @param data data.frame containing the training set
#' @param cl integer indicating the column number corresponding to the response vector that classify positive and negative regions (default = 1)
#' @param kernel SVM kernel, a character string: "linear" or "radial". (default = "radial")
#' @param cost The SVM cost parameter for both linear and radial kernels. If NULL (default), the function \code{mcTune} is run.
#' @param gamma The SVM gamma parameter for radial kernel. If radial kernel and NULL (default), the function \code{mcTune} is run.
#' @param valid.times Integer indicating how many times the training set will be split for the cross validation step (default = 10). This number must be smaller than positive and negative sets sizes.
#' @param feature.ranking List of ordered features.
#' @param feature.nb the optimal number of feature to use from the list of ordered features.
#' @param file.prefix A character string that will be used as a prefix followed by "_ROCR_perf.png" for the result plot file, if it is NULL (default), no plot is returned
#' @param numcores Number of cores to use for parallel computing (default: the number of available cores in the machine - 1)
#' @return A list with two objects.
#' \item{probs}{The predictions computed by the model for each subset during the cross-validation}
#' \item{labels}{The actual class for each subset}
#' @examples
#'data(crm.features)
#' data(feature.ranking)
#'#probs.labels.list <- evaluateModelPerformance(data.granges=crm.features,
#'#    feature.ranking=feature.ranking, feature.nb=50,
#'#    file.prefix = "test")
#'#names(probs.labels.list[[1]])

evaluateModelPerformance = function(data, cl = 1, valid.times = 10, feature.ranking = NULL, feature.nb =NULL, numcores =
                                      parallel::detectCores() - 1, file.prefix = NULL, kernel = "linear", cost = NULL, gamma = NULL) {

x=data[,-cl]
y=data[,cl]

cv.probs.labels = ModelPerformance$new(x, y, valid.times = valid.times, feature.ranking = feature.ranking, feature.nb =feature.nb, numcores =
                                      numcores, file.prefix = file.prefix, kernel = kernel, cost = cost, gamma = gamma)$cv.probs.labels

return(cv.probs.labels)

}

