evaluateModelPerformance = function(x, y, ...) {

cv.probs.labels = ModelPerformance$new(x, y, ...)$cv.probs.labels

return(cv.probs.labels)

}

