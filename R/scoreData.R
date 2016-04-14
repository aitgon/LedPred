#' Predicting new regulatory regions
#'
#' \code{scoreData} function predict new regulatory regions using SVM model from a test data set
#'
#' @param data data.frame containing the test set. This test set must have the same descriptive features as the one that were used to build the model.
#' @param model Returned object of the createModel function
#' @param ledpred Returned object from the LedPred function
#' @param score.file A character string that will be used as the file name for the output bed file, if it is NULL (default), no bed file is writen
#' @param score.file A character string that will be used as the file name for the output file, if it is NULL (default), no file is writen. The output file takes the form of two columns with object names and scores.
#' @return A 2-columns dataframe. First column containg the SVM model prediction probabilities and the second containing the corresponding regions
#' @examples
#' data(crm.features)
#' data(svm.model)
#' #pred.test <- scoreData(data.granges=crm.features, model=svm.model,
#' # score.file="test_prediction.tab")

scoreData <- function(data, ledpred=NULL, model=NULL, score.file = NULL) {

scores=NULL

if(!is.null(ledpred))  {
	scores = ledpred$model.obj$ScoreData(x=data)$probs

} else if (!is.null(model))  {
	scores = model$ScoreData(x=data)$probs
}

scores=sort(scores, decreasing=T)

if(!is.null(score.file)) {
	write.table(scores, file=score.file, quote=FALSE, col.names=FALSE)
}
return(scores)

}

