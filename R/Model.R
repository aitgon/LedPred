Model <- R6::R6Class(
  "Model",
  inherit = Data,
  public = list(
    x = NULL,
    y = NULL,
    valid.times = 5,
    test.folds = NULL,
    model = NULL,
    weights = NULL,
#    scale.factors = NULL,
    scale.center = NULL,
    scale.scale = NULL,
    feature.ranking = NULL,
    feature.nb = NULL,
    file.prefix=NULL,
    initialize = function(x, y, valid.times=self$valid.times, feature.ranking=self$feature.ranking, feature.nb=self$feature.nb, file.prefix=self$file.prefix) {
#      self$x = x
#      self$y = y
      if (!is.null(feature.ranking) && !is.null(feature.nb)) {
      self$feature.ranking=feature.ranking
      self$feature.nb=feature.nb
		selected.features = as.character(self$feature.ranking$FeatureName[1:self$feature.nb])
		x = x[,selected.features]
      }
      if (!missing(valid.times)) {
        self$valid.times = valid.times
        data.obj = Data$new(x = x, y = y, valid.times = valid.times)
      } else {
        data.obj = Data$new(x = x, y = y)
      }
      self$x = data.obj$x
      self$y = data.obj$y
      self$test.folds = data.obj$test.folds
#      self$scale.factors = data.obj$scale.factors
      self$scale.center = data.obj$scale.center # store scale center
      self$scale.scale = data.obj$scale.scale # store scale scale
      self$file.prefix = file.prefix
      private$CreateModel()
      self$weights = (t(self$model$coefs) %*% self$model$SV)
#      browser()
if (!is.null(self$file.prefix)) 
	save(self, file = paste(file.prefix,"_model.rda",sep = "")) # can save model for later use
    },
    ScoreData = function(x) {
      if (!is.null(self$feature.ranking) && !is.null(self$feature.nb)) {
		selected.features = as.character(self$feature.ranking$FeatureName[1:self$feature.nb])
		x = x[,selected.features]
      }
#    x=x/self$scale.factors
    x = scale(x, center=self$scale.center, scale=self$scale.scale)
      library(e1071)
      classpred = predict(self$model, x, decision.values = private$decision.values, probability = private$probability)
      probs = attr(classpred,"probabilities")[,1]
      scores = attr(classpred,"decision.values")[,1]
      return(list(probs = probs, scores = scores, prediction=as.numeric(as.character(classpred))))
    },
    CalcPredictionKappa = function(x, y) {
      prediction = self$ScoreData(x = x)$prediction
      return(irr::kappa2(cbind(y, prediction))$value)
    }
  ),
  private = list(
    probability = TRUE,
    decision.values = TRUE,
    CreateModel = function() {
set.seed(123)
      self$model = e1071::svm(
        x = self$x, y = self$y, type = private$type, kernel = private$kernel, cost = private$cost, scale = private$scale, probability =
          private$probability, decision.values = private$decision.values,
        cachesize = 500
      )
    }
  )
)
