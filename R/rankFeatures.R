#' Ranking the features according to their importance
#'
#' The \code{rankFeatures} function  performs a Recursive Feature Elimination (RFE) on subsets of the feature matrix. For each subset the features are ranked according to the weight attributed by SVM at each round of elimination and the average rank of each feature over the subsets is returned. We recommand to save the object containing the ranked features for the following steps.
#'
#' @param data data.frame containing the training set
#' @param cl integer indicating the column number corresponding to the response vector that classify positive and negative regions (default = 1)
#' @param halve.above During RFE, all the features are ranked at the first round and the half lowest ranked features (that contribute the least in the model) are removed for the next round. When the number of feauture is lower or equal to halve.above, the features are removed one by one. (default=100)
#' @param valid.times Integer indicating how many times the training set will be split (default = 10). This number must be smaller than positive and negative sets sizes.
#' @param kernel SVM kernel, a character string: "linear" or "radial". (default = "radial")
#' @param cost The SVM cost parameter for both linear and radial kernels. If NULL (default), the function \code{mcTune} is run.
#' @param gamma The SVM gamma parameter for radial kernel. If radial kernel and NULL (default), the function \code{mcTune} is run.
#' @param numcores Number of cores to use for parallel computing (default: the number of available cores in the machine - 1)
#' @param file.prefix A character string that will be used as a prefix for output file, if it is NULL (default), no file is writen.
#' @return A 3-columns data frame with ranked features. First column contains the feature names, the second the original position of the feature in the feature.matrix and the third the average rank over the subsets.
#' @examples
#' data(crm.features)
#' cost <- 1
#' gamma <- 1
#'  #feature.ranking <- rankFeatures(data.granges=crm.features, cost=cost,gamma=gamma,
#'  #    kernel='linear', file.prefix = "test", halve.above=10)


rankFeatures = function(data, cl = 1, halve.above = 100, valid.times =
                          10, kernel = "linear", cost = 1, gamma = 1, numcores =
                          parallel::detectCores() - 1, file.prefix = NULL) {


x=data[,-cl]
y=data[,cl]

	feature.ranking.obj <- FeatureRanking$new(x = x, y = y, halve.above = halve.above, valid.times =
                          valid.times, kernel = kernel, cost = cost, gamma = gamma, numcores =
                          numcores, file.prefix = file.prefix)
	feature.ranking = feature.ranking.obj$feature.ranking
	return(feature.ranking)
}

