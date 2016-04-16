.classAgreement2 <- function (tab) {
  n <- sum(tab)
  # correct classification rate
  if (!is.null(dimnames(tab))) {
    lev <- intersect(colnames(tab), rownames(tab))
    d <- diag(tab[lev, lev])
    p0 <- sum(d) / n
  } else {
    m <- min(dim(tab))
    d <- diag(tab[1:m, 1:m])
    p0 <- sum(d) / n
  }
  p0 <- list(p0 = p0)
  if (is.null(dimnames(tab))) {
    stop('tables without dimension names are not allowed when generating confusionmatrizes.')
  }
  return(p0)
}


.plotCostGamma <- function(e1071.tune.obj) {
  c.g.perf <- e1071.tune.obj$performances
  if (length(e1071.tune.obj$best.parameter) == 1) {
    c.g.perf[,"gamma"] <- 1:nrow(c.g.perf)
  }
  
  xyz = akima::interp(c.g.perf$cost, c.g.perf$gamma, c.g.perf$error)
  p = plot3D::image2D(
    xyz$z, x = xyz$x, y = xyz$y, facets = FALSE, log = "xy",
    xlim = c(min(xyz$x[which(xyz$x > 0)]), max(x = xyz$x)),
    ylim = c(min(xyz$y[which(xyz$y > 0)]), max(x = xyz$y)),
    xlab = "cost", ylab = "gamma"
  )
  cost = e1071.tune.obj$best.parameters$cost
  gamma = e1071.tune.obj$best.parameters$gamma
  title(paste("best cost:", cost,' and gamma:', gamma))
  points (c.g.perf$cost,c.g.perf$gamma, pch = "x", cex = 2)
}

ParameterTuner <- R6::R6Class(
  "ParameterTuner",
  inherit = Data,
  public = list(
    ranges = list(gamma=c(1,10), cost=c(1,10)),
    e1071.tune.obj = NULL,
    initialize = function(x, y, kernel = self$kernel, cost = self$cost, gamma =
                            self$gamma, valid.times=self$valid.times, ranges = self$ranges, numcores=self$numcores, file.prefix=self$file.prefix) {
      if (!missing(kernel))
        self$kernel = kernel
      if (!missing(cost))
        self$cost = cost
      if (!missing(gamma))
        self$gamma = gamma
      if (!missing(ranges)) self$ranges = ranges
      if (!missing(numcores)) self$numcores = numcores
    if (.Platform$OS.type == "windows") self$numcores = 1
      if (!missing(file.prefix)) self$file.prefix = file.prefix
      if (!missing(valid.times)) {
        self$valid.times = valid.times
        parent.obj = Data$new(x = x, y = y, kernel = self$kernel, cost = self$cost, gamma = self$gamma, valid.times = self$valid.times)
      } else {
        parent.obj = Data$new(x = x, y = y, kernel = self$kernel, cost = self$cost, gamma = self$gamma)
      }
      self$x = parent.obj$x
      self$y = parent.obj$y
      self$test.folds = parent.obj$test.folds
      self$cost = parent.obj$cost
      self$gamma = parent.obj$gamma
      self$scale.factors = parent.obj$scale.factors
if (is.null(self$cost) || (self$kernel=='radial' && is.null(self$gamma))) {
	self$e1071.tune.obj = private$mcTune()
	self$cost = self$e1071.tune.obj$best.parameters$cost
	self$gamma = self$e1071.tune.obj$best.parameters$gamma
	}
  }
  ),
  private = list(
      mcTune = function(y=self$y, x=self$x, valid.times=self$valid.times, numcores=self$numcores, ranges=self$ranges, file.prefix=self$file.prefix) {

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
#  browser()
#  train.ind <- .makeCrossValidSets(data, valid.times)
train.folds<-lapply(1:length(self$test.folds), function(xi) (1:nrow(x))[-self$test.folds[[xi]]])

  parameters <- (if (is.null(self$ranges))
    data.frame(dummyparameter = 0)
    else
      expand.grid(self$ranges))
  p <- nrow(parameters)

  if (!is.logical(tunecontrol$random)) {
    if (tunecontrol$random < 1)
      stop("random must be a strictly positive integer")
    if (tunecontrol$random > p)
      tunecontrol$random <- p
    parameters <- parameters[sample(1:p, tunecontrol$random),]
  }
  if (.Platform$OS.type == "windows") {
    numcores = 1
  }

# mctune function ------------------------------------
  train_results <-
    do.call(
      what = get("mclapply", asNamespace("parallel")), args = c(mc.cores = self$numcores, list(
        X = 1:p, FUN = function(para.set) {
          sampling.errors <- c()
          sampling.confusions <- c()
          ## - loop over all training samples
          for (sample in 1:length(train.folds)) {
            repeat.errors <- c()
            repeat.confusions <- c()
            
            ## - repeat training `nrepeat' times
            for (reps in 1:tunecontrol$nrepeat) {
              #        print(ranges)
              

              ## train one model
              pars <- if (is.null(ranges))
                NULL
              else
                lapply(parameters[para.set,,drop = FALSE], unlist)
                train.fold.i <- train.folds[[sample]]
	            model.obj <- Model$new(x = self$x[train.fold.i,], y = self$y[train.fold.i], kernel=self$kernel, cost=pars$cost, gamma=pars$gamma)
                model <- model.obj$model
              ## predict validation set
              
              
              pred <-
                predict.func(model,x[-train.folds[[sample]],])
              ## compute performance measure
#              true.y <- .resp(train.x, data[-train.folds[[sample]],])
              true.y <- self$y[-train.folds[[sample]]]
              
              if (is.null(true.y))
                true.y <- rep(TRUE, length(pred))
              
              if (!is.null(tunecontrol$error.fun))
                repeat.errors[reps] <-
                tunecontrol$error.fun(true.y, pred)
              else if ((is.logical(true.y) ||
                       (is.logical(pred) ||
                        is.factor(true.y)) &&
                        is.factor(pred) || is.character(pred))) {
                ## classification error
                l <- .classAgreement2(table(pred, true.y))
                repeat.errors[reps] <-
                  (1 - l$p0) # wrong classification rate
              } else if (is.numeric(true.y) &&
                         is.numeric(pred))
                ## mean squared error
                repeat.errors[reps] <-
                crossprod(pred - true.y) / length(pred)
              else
                stop("Dependent variable has wrong type!")
            }
            
            sampling.errors[sample] <-
              tunecontrol$repeat.aggregate(repeat.errors)
            
          }
          list(
            model.error = tunecontrol$sampling.aggregate(sampling.errors),
            model.variance = tunecontrol$sampling.dispersion(sampling.errors)
          )
        }
      ))
    )
    
  model.errors <-
    unlist(lapply(train_results,function(x)
      x$model.error))
  model.variances <-
    unlist(lapply(train_results,function(x)
      x$model.variance))
  
  ## return results
  best <- which.min(model.errors)
  pars <- if (is.null(ranges))
    NULL
  else
    lapply(parameters[best,,drop = FALSE], unlist)
  e1071.tune.obj <-
    structure(
      list(
        best.parameters  = parameters[best,,drop = FALSE],
        best.performance = model.errors[best],
        method           = "svm",
        nparcomb         = nrow(parameters),
        train.ind        = train.folds,
        sampling         = switch(
          tunecontrol$sampling,
          fix = "fixed training/validation set",
          bootstrap = "bootstrapping",
          cross = if (tunecontrol$cross == nrow(self$x))
            "leave-one-out"
          else
            paste(tunecontrol$cross,"-fold cross validation", sep ="")
        ),
        performances     = if (tunecontrol$performances)
          cbind(parameters, error = model.errors, dispersion = model.variances),
        best.model       = if (tunecontrol$best.model) {
                modeltmp <- e1071::svm(y=self$y, x=self$x, scale = self$scale, kernel = self$kernel, type="C-classification", cost=parameters[best,,drop = FALSE]$cost, gamma=parameters[best,,drop = FALSE]$gamma)
          modeltmp
        }
      ),
      class = "tune"
    )
  if (!is.null(file.prefix)) {
    png(paste(file.prefix,"_c_g_eval.png",sep = ""))
    .plotCostGamma(e1071.tune.obj)
    garb = dev.off()
  }
return(e1071.tune.obj)
    }
      )
)

