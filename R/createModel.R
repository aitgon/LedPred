#' Create the model with the optimal features
#'
#' \code{createModel} function creates a SVM model from the training data set with the selected features.
#'
#' @param data data.frame containing the training set
#' @param cl integer indicating the column number corresponding to the response vector that classify positive and negative regions (default = 1)
#' @param kernel SVM kernel, a character string: "linear" or "radial". (default = "radial")
#' @param cost The SVM cost parameter for both linear and radial kernels. If NULL (default), the function \code{mcTune} is run.
#' @param gamma The SVM gamma parameter for radial kernel. If radial kernel and NULL (default), the function \code{mcTune} is run.
#' @param valid.times Integer indicating how many times the training set will be split for the cross validation step (default = 10). This number must be smaller than positive and negative sets sizes.
#' @param feature.ranking List of ordered features.
#' @param feature.nb the optimal number of feature to use from the list of ordered features.
#' @param file.prefix A character string that will be used as a prefix followed by "_model.RData" for the resulting model file, if it is NULL (default), no model is saved
#' @return the best SVM model
#' @examples
#'     data(crm.features)
#'     cost <- 1
#'     gamma <- 1
#'     data(feature.ranking)
#'     feature.nb <- 70
#' #svm.model <- createModel(data.granges=crm.features, cost=cost, gamma=gamma,
#' #    feature.ranking=feature.ranking, feature.nb=feature.nb)
#' #feature.weights <- as.data.frame(t(t(svm.model$coefs) %*% svm.model$SV))

createModel = function(data, cl = 1,kernel = "radial",cost = 1,gamma =
             1, valid.times = 10, feature.ranking = NULL,feature.nb = NULL,file.prefix = NULL) {

x=data[,-cl]
y=data[,cl]

#selected.features = as.character(feature.ranking$FeatureName[1:feature.nb])
#x = x[,selected.features]
model.obj <- Model$new(x = x, y = y, file.prefix=file.prefix, feature.ranking = feature.ranking, feature.nb = feature.nb, cost=cost)
#save(model.obj, file = paste(file.prefix,"_model.rda",sep = "")) # can save model for later use

return(model.obj)
}
