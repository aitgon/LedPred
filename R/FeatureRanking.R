FeatureRanking <- R6::R6Class(
  "FeatureRanking",
  inherit = ParameterTuner,
  public = list(
    kfold.nb = 1,
    halve.above = 100,
    feature.ranking = NULL,
    initialize = function(x, y, kernel = self$kernel, cost = self$cost, gamma =
                            self$gamma, valid.times = self$valid.times, kfold.nb =
                            self$kfold.nb, halve.above = self$halve.above, numcores = self$numcores, file.prefix =
                            self$file.prefix) {
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
      if (!missing(file.prefix))
        self$file.prefix = file.prefix
      if (!missing(kfold.nb))
        self$kfold.nb = kfold.nb
      if (!missing(numcores))
        self$numcores = numcores
      if (.Platform$OS.type == "windows") self$numcores = 1
      if (!missing(halve.above))
        self$halve.above = halve.above
      self$feature.ranking = private$RankFeatures(self$x, self$y, self$test.folds, self$halve.above)
      if (!is.null(self$file.prefix))
        write.table(
          self$feature.ranking, file = paste(file.prefix, '_feature_ranking.txt', sep =
                                               ""), quote = FALSE, row.names = FALSE
        )
    }
  ),
  private = list(
    probability = TRUE,
    decision.values = TRUE,
    RankFeatures = function (x, y, test.folds, halve.above) {
    message("rankFeatures is running ...")
      feature.ranking_results = parallel::mclapply(
        test.folds, private$RunSVMRFEOneFold, x = x, y = y, mc.cores = self$numcores, mc.set.seed = FALSE
      )
      
      featureID = sort(apply(sapply(feature.ranking_results, function(x)
        sort(x$feature, index.return = TRUE)$ix), 1, mean), index = TRUE)$ix
      avg.rank  = sort(apply(sapply(feature.ranking_results, function(x)
        sort(x$feature, index.return = TRUE)$ix), 1, function (x)
          round(mean(x),5)), index = TRUE)$x
      feature.name = colnames(x)[featureID]
      feature.ranking = data.frame(FeatureName = feature.name, FeatureID = featureID, AvgRank =
                                     avg.rank)
      feature.ranking = feature.ranking[with(feature.ranking, order(AvgRank, FeatureName)),]
      return(feature.ranking)
    },
    RunSVMRFEOneFold = function (test.fold.i, x, y, halve.above = 100) {
      train.fold.i = (1:nrow(x))[!(1:nrow(x) %in% test.fold.i)]
      x = x[train.fold.i,]
      y = y[train.fold.i]
      set.seed(123)
      k.folds = rep(1:self$kfold.nb, len = nrow(x))[sample(nrow(x))]
      k.folds = lapply(1:self$kfold.nb, function(x)
        which(k.folds == x))
      

      feature.ids = private$RunSVMRFE(x, y, k.folds, halve.above)
      return(
        list(
          feature.ids = feature.ids, train.data.ids = train.fold.i, test.data.ids = test.fold.i
        )
      )
    },
    RunSVMRFE = function(x, y, k.folds, halve.above = 100) {
      n = ncol(x)
      i.surviving = 1:n
      i.ranked    = n
      ranked.list = vector(length = n)
      
      x.rfe = x
      
      # ---------------------
      while (length(i.surviving) > 1) {

        k.fold.weights = lapply(k.folds, private$GetKFoldWeights, x = x.rfe, y =
                                  y)
        
        c = private$CombineKFoldWeigths(k.fold.weights)
        ranking = sort(c, index.return = TRUE)$ix
        
        if (length(i.surviving) > halve.above) {
          # Cut features in half until less than halve.above
          nfeat = length(i.surviving)
          ncut  = round(nfeat / 2)
          n     = nfeat - ncut
        } else
          ncut = 1
        
        ranked.list[i.ranked:(i.ranked - ncut + 1)] = i.surviving[ranking[1:ncut]]
        i.ranked    = i.ranked - ncut
        i.surviving = i.surviving[-ranking[1:ncut]]
        
        x.rfe = x[,i.surviving]
      }
      
      ranked.list[1] = i.surviving # add latest i.surviving
      # ---------------------
      return(ranked.list)
    },
    CombineKFoldWeigths = function (w) {
      if (length(w) > 1) {
        w = do.call(rbind, w)
        
        # Normalize each weights vector
        w = t(apply(w, 1, function(x)
          x / sqrt(sum(x ^ 2))))
        
        # Compute ranking criteria
        v    = w ^ 2
        vbar = apply(v, 2, mean)
        vsd  = apply(v, 2, sd)
        c    = vbar / vsd
      } else {
        c = unlist(w) ^ 2
      }
      return(c)
    },
    GetKFoldWeights = function(k.fold.i, x, y) {
      x = x[k.fold.i,]
      y = y[k.fold.i]
      obj <-
        Model$new(
          x = x, y = y, kernel = self$kernel, cost = self$cost, gamma = self$gamma, valid.times = 1
        )
      
      return(obj$weights)
    }
  )
)
