mcTune <- function(data, cl=1,
                   ranges = list(gamma=c(1,10), cost=c(1,10)), ...) {

x=data[,-cl]
y=data[,cl]

tune.parameters.obj <-ParameterTuner$new(x = x, y = y, ranges=ranges, ...)

return(tune.parameters.obj)
}
