mcTune <- function(x, y,
                   ranges = list(gamma=c(1,10), cost=c(1,10)), ...) {

tune.parameters.obj <-ParameterTuner$new(x = x, y = y, ranges=ranges, ...)

return(tune.parameters.obj)
}
