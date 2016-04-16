#########################################################################################
# A function to run the others

#########################################################################################
# A function to run the others

#' Creates an SVM model given a feature matrix
#'
#' The \code{LedPred} function computes the best SVM parameters, defines the optimal features for creating the SVM model by running sequentially \code{mcTune},  \code{rankFeatures},  \code{tuneFeatureNb} and  \code{createModel}. The performances of this model are then computed usong \code{evaluateModelPerformance}.
#'
#' @param data data.frame containing the training set
#' @param cl integer indicating the column number corresponding to the response vector that classify positive and negative regions (default = 1)
#' @param ranges list object containing one (linear kernel) or two (radial kernel) vectors of integers corresponding to SVM cost and SVM gamma parameters to test.
#' @param cost The SVM cost parameter for both linear and radial kernels. If NULL (default), the function \code{mcTune} is run.
#' @param gamma The SVM gamma parameter for radial kernel. If radial kernel and NULL (default), the function \code{mcTune} is run.
#' @param kernel SVM kernel, a character string: "linear" or "radial". (default = "radial")
#' @param scale Logical indicating if the data have to be scaled or not (default = FALSE)
#' @param valid.times Integer indicating how many times the training set will be split for the cross validation step (default = 10). This number must be smaller than positive and negative sets sizes.
#' @param file.prefix A character string that will be used as a prefix for the result files. If it is NULL (default), no plot is returned
#' @param step.nb Number of features to add at each step (default = 10)
#' @param numcores Number of cores to use for parallel computing (default: the number of available cores in the machine - 1)
#' @param halve.above During RFE, all the features are ranked at the first round and the half lowest ranked features (that contribute the least in the model) are removed for the next round. When the number of feauture is lower or equal to halve.above, the features are removed one by one. (default=100)
#' @return A list of the object produced at each step
#' \item{best.params}{A list of the parameters giving the lowest misclassification error}
#' \item{feature.ranking}{List of ordered features from \code{rankFeatures}}
#' \item{feature.nb}{he optimal number of feature to use from the list of ordered features from \code{tuneFeatureNb}}
#' \item{model.svm}{The best SVM model \code{createModel}}
#' \item{probs.label.list}{The cross-validation results from \code{evaluateModelPerformance}}
#' @examples
#'  data(crm.features)
#'  #cost_vector <- c(1,3,10)
#'  #gamma_vector <- c(1,3,10)
#'  #ledpred.list=LedPred(data.granges=crm.features, cl=1, ranges = list(cost=cost_vector,
#'  #                          gamma=gamma_vector), kernel="linear", halve.above=50)
#'  #names(ledpred.list)


LedPred = function(data = NULL, cl = 1, ranges = list(gamma=c(1,10), cost=c(1,10)), cost=NULL, gamma=NULL, kernel = "linear", valid.times = 10, file.prefix = NULL, numcores = parallel::detectCores() - 1, step.nb =10, halve.above = 100) {

x=data[,-cl]
y=data[,cl]
feature.nb.vector = seq(from = step.nb, to = (ncol(x) - 1), by = step.nb)

  obj <- LedPredClass$new(x = x, y = y, ranges = ranges, cost=cost, gamma=gamma, kernel = kernel, valid.times =
             valid.times, file.prefix = file.prefix, numcores = numcores, halve.above = halve.above, feature.nb.vector=feature.nb.vector)
  ledpred.summary <-
    list(
      feature.ranking = obj$feature.ranking, feature.nb =
        obj$best.feature.nb, model.obj = obj$model.obj, test.folds = obj$test.folds, probs.label.list = obj$probs.label.list
    )
  return(ledpred.summary)
}


