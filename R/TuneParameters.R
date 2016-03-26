TuneParameters <- R6::R6Class(
  "TuneParameters",
  inherit = Data,
  public = list(
    initialize = function(x, y, valid.times=self$valid.times, numcores=self$numcores, file.prefix=self$file.prefix) {
      if (!missing(valid.times)) {
        self$valid.times = valid.times
        data.obj = Data$new(x = x, y = y, valid.times = valid.times)
      } else {
        data.obj = Data$new(x = x, y = y)
      }

# mctune function ------------------------------------

  message("mcTune is running ...")
  call <- match.call()
  
#  if (!is.null(data.granges)) {
#    data = .crmFeaturesToDf(data.granges)
#  }
  
  
  ## Set the method to be tuned
  method <- 'svm'
  ## Set control method to tune.control() from e1071 package
  
  tunecontrol <-
    e1071::tune.control(sampling = "cross", cross = valid.times)




#  cl <- as.factor(as.vector(data[,cl]))
  cl <- y
  
#  train.x <- cl ~ .
  train.x <- x
  useFormula = TRUE
  

  # Set the method to predict
  predict.func <- predict
  
  
  ## prepare training indices  
#  train.ind <- .makeCrossValidSets(data, valid.times)



# mctune function ------------------------------------
  }
  ),
  private = list()
)

