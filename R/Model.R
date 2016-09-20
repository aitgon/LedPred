Model <- R6::R6Class(
  "Model",
  inherit = ParameterTuner,
  public = list(
    model = NULL,
    weights = NULL,
    feature.ranking = NULL,
    feature.nb = NULL,
    initialize = function(x, y, kernel = self$kernel, cost = self$cost, gamma =
                            self$gamma, valid.times = self$valid.times, feature.ranking = self$feature.ranking, feature.nb =
                            self$feature.nb, file.prefix = self$file.prefix) {
      #      self$x = x
      #      self$y = y
      if (!is.null(feature.ranking) && !is.null(feature.nb)) {
        self$feature.ranking = feature.ranking
        self$feature.nb = feature.nb
        selected.features = as.character(self$feature.ranking$FeatureName[1:self$feature.nb])
        x = x[,selected.features]
      }
      if (!missing(kernel))
        self$kernel = kernel
      if (!missing(cost))
        self$cost = cost
      if (!missing(gamma))
        self$gamma = gamma
      if (!missing(valid.times)) {
        self$valid.times = valid.times
        parent.obj = ParameterTuner$new(x = x, y = y, kernel = self$kernel, cost = self$cost, gamma = self$gamma, valid.times = self$valid.times)
      } else {
        parent.obj = ParameterTuner$new(x = x, y = y, kernel = self$kernel, cost = self$cost, gamma = self$gamma)
      }
      self$x = parent.obj$x
      self$y = parent.obj$y
      self$test.folds = parent.obj$test.folds
      self$cost = parent.obj$cost
      self$gamma = parent.obj$gamma
      self$scale.factors = parent.obj$scale.factors
#      self$scale.center = data.obj$scale.center #store scale center
#      self$scale.scale = data.obj$scale.scale #store scale scale
      self$file.prefix = file.prefix
      private$CreateModel()
      self$weights = (t(self$model$coefs) %*% self$model$SV)
      if (!is.null(self$file.prefix))
        save(self, file = paste(file.prefix,"_model.rda",sep = "")) # can save model for later use
    },
    ScoreData = function(x, scale=TRUE) {
      if (!is.null(self$feature.ranking) && !is.null(self$feature.nb)) {
        selected.features = as.character(self$feature.ranking$FeatureName[1:self$feature.nb])
        x = x[,selected.features]
      }
      if (scale) {
      x=t(t(x)/self$scale.factors)
      }
#      x = scale(x, center = self$scale.center, scale = self$scale.scale)
      library(e1071)
      classpred = predict(
        self$model, x[,colnames(self$model$SV)], decision.values = private$decision.values, probability = private$probability
      )
      probs = attr(classpred,"probabilities")[,c("1")]
      scores = attr(classpred,"decision.values")[,1]
      if (colnames(attr(classpred,"decision.values"))=="-1/1") # predicts inverse: needs negative
        scores = -scores
      return(list(
        probs = probs, scores = scores, prediction = classpred
      ))
    },
    CalcPredictionKappa = function(x, y, scale=FALSE) {
      prediction = self$ScoreData(x = x, scale=scale)$prediction
      return(irr::kappa2(cbind(y, prediction))$value)
    }
  ),
  private = list(
    probability = TRUE,
    decision.values = TRUE,
    CreateModel = function() {
      set.seed(123)
      if (self$kernel=="linear")
      self$model = e1071::svm(
        x = self$x, y = self$y, type = private$type, kernel = self$kernel, cost = self$cost, scale = private$scale, probability =
          private$probability, decision.values = private$decision.values,
        cachesize = 500
      ) 
      else if (self$kernel=="radial")
      self$model = e1071::svm(
        x = self$x, y = self$y, type = private$type, kernel = self$kernel, cost = self$cost, gamma = self$gamma, scale = private$scale, probability =
          private$probability, decision.values = private$decision.values,
        cachesize = 500
      ) 
    }
  )
)
