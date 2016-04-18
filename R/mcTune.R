#' Tuning the SVM parameters
#'
#' The \code{mcTune} function is a modified version of the function \code{tune}  from package e1071  [6]. It tests the different combinations of C and gamma parameters given as vectors in a list and will return the prediction error computed during the cross-validation step.
#'
#' @param data data.frame containing the training set
#' @param cl integer indicating the column number corresponding to the response vector that classify positive and negative regions (default = 1)
#' @param ranges list object containing one (linear kernel) or two (radial kernel) vectors of integers corresponding to SVM cost and SVM gamma parameters to test.
#' @param kernel SVM kernel, a character string: "linear" or "radial". (default = "radial")
#' @param valid.times Integer indicating how many times the training set will be split for the cross validation step (default = 10). This number must be smaller than positive and negative sets sizes.
#' @param file.prefix A character string that will be used as a prefix followed by "_c_g_eval.png" for result plot files, if it is NULL (default), no plot is returned
#' @param numcores Number of cores to use for parallel computing (default: the number of available cores in the machine - 1)
#' @return A list of class \code{tune}
#' \item{best.parameters}{A list of the parameters giving the lowest misclassification error}
#' \item{best.performance}{The lowest misclassification error}
#' \item{method}{The method used}
#' \item{nparcomb}{the number of tested parameter combinations}
#' \item{train.ind}{The indexes used to produce subsets during the cross validation step}
#' \item{sampling}{The cross-validation fold number}
#' \item{performances}{A matrix summarizing the cross-validation step with the error for each tested parameter at each round and the dispersion of these errors (regarding to the average error)}
#' \item{best.model}{The model produced by the best parameters}
#' @examples
#' data(crm.features)
#' cost.vector <- c(1,3,10,30)
#' gamma.vector <- c(1,3,10,30)
#' #c.g.obj <- mcTune(data.granges= crm.features, ranges = list(cost=cost.vector,
#' #    gamma=gamma.vector), kernel='linear', file.prefix = "test")
#' #names(c.g.obj)
#' # cost <- c.g.obj$best.parameters$cost
#' # gamma <- c.g.obj$best.parameters$gamma

mcTune <- function(data, cl = 1,
                   ranges = list(gamma=c(1,10), cost=c(1,10)), kernel = "linear",
                   valid.times = 10,
                   file.prefix = NULL, numcores=ifelse(.Platform$OS.type != "windows", 1,  parallel::detectCores() - 1)) {

x=data[,-cl]
y=data[,cl]

tune.parameters.obj <-ParameterTuner$new(x = x, y = y, ranges=ranges, kernel = kernel,
                   valid.times = valid.times,
                   file.prefix = file.prefix, numcores = numcores)

return(tune.parameters.obj)
}
