#' This is data to be included in my package
#'
#' @name crm.features
#' @docType data
NULL

#' This is data to be included in my package
#'
#' @name feature.ranking
#' @docType data
NULL

.removeZeroFeatures <- function (x) {
# This function will removed features that are zero everywhere
      if (any(apply(x==0,2,all))) {
        warning("Some data features are zero everywhere and must be removed")
        x = x[, !apply(x==0,2,all)]
      }
      return(x)
}

Data <- R6::R6Class(
  "Data",
  public = list(
    x = NULL,
    y = NULL,
    kernel = "linear",
    cost = NULL,
    gamma = NULL,
    valid.times = 5,
    test.folds = NULL,
    scale.factors = NULL,
#    scale.center = NULL,
#    scale.scale = NULL,
    numcores = ifelse(.Platform$OS.type == "windows", 1,  parallel::detectCores() - 1),
    file.prefix = NULL,
    initialize = function(x, y, kernel = self$kernel, cost = self$cost, gamma =
                            self$gamma, valid.times = self$valid.times, numcores = self$numcores, file.prefix = self$file.prefix) {
      self$x = x #assign to self
      self$x[is.na(self$x)] <- 0 #replace NAs with zeros
      self$x = .removeZeroFeatures(self$x) # remove features that are zero everywhere
      self$scale.factors = apply(self$x, 2, function(x) sqrt(sum(x^2))) # store scale factors
#      self$scale.center = colMeans(x) #store scale center
#      self$scale.scale = matrixStats::colSds(as.matrix(x)) #store scale scale
#      self$x = scale(x, center = self$scale.center, scale = self$scale.scale)
      self$x = t(t(self$x)/self$scale.factors) #scale x
      self$y = as.factor(y) #store y
      if (!missing(kernel))
        self$kernel = kernel
      if (!missing(cost))
        self$cost = cost
      if (!missing(gamma))
        self$gamma = gamma
      if (!missing(valid.times))
        self$valid.times <- valid.times
      if (self$valid.times > 1)
        self$test.folds = self$MakeCrossValidSets(self$valid.times)
    },
    MakeCrossValidSets = function(valid.times) {
      nrows = nrow(self$y)
      perm.ind.pos = which(self$y == "1")
      nrows.pos = length(perm.ind.pos)
      perm.ind.neg = which(self$y == "-1")
      nrows.neg = length(perm.ind.neg)
      if (valid.times > nrows.pos)
        stop(sQuote("cross"), " must not exceed positive or negative set sizes!")
      if (valid.times == 1)
        stop(sQuote("cross"), " must be greater than 1!")
      test.folds.pos = tapply(1:nrows.pos, cut(1:nrows.pos, breaks =  valid.times), function(x)
        perm.ind.pos[x])
      test.folds.neg = tapply(1:nrows.neg, cut(1:nrows.neg, breaks =  valid.times), function(x)
        perm.ind.neg[x])
      test.folds = lapply(1:length(test.folds.pos), function (x)
        c(test.folds.pos[[x]], test.folds.neg[[x]]))
      return(test.folds)
    }
  ),
  private = list(type = 'C-classification',
                 scale = FALSE)
)
