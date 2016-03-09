scoreData <- function(x, ledpred=NULL, model=NULL, score.file = NULL) {

scores=NULL

if(!is.null(ledpred))  {
	scores = ledpred$model.obj$ScoreData(x=x)$probs

} else if (!is.null(model))  {
	scores = model$ScoreData(x=x)$probs
}

if(!is.null(score.file)) {
	write.table(scores, file=score.file, quote=FALSE, col.names=FALSE)
}
return(scores)

}

