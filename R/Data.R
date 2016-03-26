Data <- R6::R6Class(
  "Data",
  public = list(
    x = NULL,
    y = NULL,
    valid.times = 5,
    test.folds = NULL,
    scale.center = NULL,
    scale.scale = NULL,
    numcores = parallel::detectCores() - 1,
    file.prefix = NULL,
    initialize = function(x, y, valid.times=self$valid.times) {
#self$scale.factors = apply(x, 2, function(x) sqrt(sum(x^2))) # store scale factors
self$scale.center=colMeans(x) # store scale center
self$scale.scale=matrixStats::colSds(as.matrix(x)) # store scale scale
self$x = scale(x, center=self$scale.center, scale=self$scale.scale)
#self$x = x/self$scale.factors # scale x
		self$y = y # store y
      if (!missing(valid.times)) self$valid.times <- valid.times
if (self$valid.times > 1) self$test.folds = self$MakeCrossValidSets(self$valid.times)
    },
    MakeCrossValidSets = function(valid.times) {
      nrows = nrow(self$y)
      perm.ind.pos = which(self$y == 1)
      nrows.pos = length(perm.ind.pos)
      perm.ind.neg = which(self$y < 1)
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
    },
    MakeCrossValidSets_bak = function(valid.times) {
      nrows = nrow(self$data)
      perm.ind.pos = which(self$data$cl == 1)
      nrows.pos = length(perm.ind.pos)
      perm.ind.neg = which(self$data$cl < 1)
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
  private = list(
    type = 'C-classification',
    cost = 1,
    kernel = "linear",
    scale = FALSE
  )
)

