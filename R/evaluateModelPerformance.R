evaluateModelPerformance = function(data, cl=1, ...) {

x=data[,-cl]
y=data[,cl]

cv.probs.labels = ModelPerformance$new(x, y, ...)$cv.probs.labels

return(cv.probs.labels)

}

