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

