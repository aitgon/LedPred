#Copyright: Denis Seyres, Aitor Gonzalez, Elodie Darbo (TAGC 2015)

#' This is data to be included in my package
#'
#' @name feature.matrix
#' @docType data
NULL

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

#' This is data to be included in my package
#'
#' @name svm.model
#' @docType data
NULL

.crmFeatureDfToGranges <- function(crm.feature.tab, genome = TRUE) {
  mat <-
    matrix(unlist(strsplit(rownames(crm.feature.tab), "_")), nrow = dim(crm.feature.tab)[1], byrow =
             TRUE)
  if (genome) {
    colnames(mat) <- c("genome", "seqnames", "start", "end", "strand")
  } else {
    colnames(mat) <- c("seqnames", "start", "end", "strand")
  }
  df <- as.data.frame(mat)
  df[,"start"] <- as.numeric(as.character(df[,"start"]))
  df[,"end"] <- as.numeric(as.character(df[,"end"]))
  crm.feature.gr <- GenomicRanges::makeGRangesFromDataFrame(df)
  GenomicRanges::mcols(crm.feature.gr) <- crm.feature.tab
  #genome(crm.feature.gr)=mat[1]
  return(crm.feature.gr)
}

.crmFeaturesToDf <- function(crm.features) {
  seq.feature.df = as.data.frame(crm.features, row.names = gsub(" ", "", apply(
    data.frame(
      seqnames = seqnames(crm.features), start = start(crm.features), end = end(crm.features), strand =
        strand(crm.features)
    ), 1, paste, collapse = "_"
  )))[,-1:-5]
  
  return(seq.feature.df)
}

#' R interface to bed_to_matrix REST in server
#'
#' The \code{mapFeaturesToCRMs} function allows the user to create a training set matrix to build a predictive model. The training set is composed of positive regions (known to be involved in the pathway of interest) and negative regions (randomly picked or known to not be involved in the pathway of interest) that will be described (scored) by features. Three types of features file format are accepted: Position specific scoring matrices modeling motifs recognised by transcription factors, bed files containing region coordinates for any discrete feature (NGS peaks, conservation blocks) and wig/bigWig files containing signal data. This script has been tested with version 0.99 of the online server. Go here to see current version of the server http://ifbprod.aitorgonzalezlab.org/map_features_to_crms.php
#'
#' @param URL URL of the server REST target
#' @param positive.bed Positive bed file path. Compulsory
#' @param genome Genome code, eg. dm3 for Drosophila Melanogaster. Compulsory
#' @param negative.bed Negative bed file path.
#' @param shuffling Integer with number of time shuffle background sequences (background.seqs). If negative.bed is NULL and shuffling is set at 0, the feature matrix does not contain negative sequences. It is useful to produce a test set matrix.
#' @param background.seqs Background sequences used for shuffling. If shuffling = 0, set this parameter at 0.
#' @param genome.info File require for shuffling bed. If shuffling = 0, set this parameter at 0.
#' @param pssm Position specific scoring matrices
#' @param background.freqs Background frequencies of nucleotides in genome
#' @param ngs NGS (bed and wig) files
#' @param bed.overlap Minimal overlap as a fraction of query sequence with NGS bed peak. Equivalent with intersectBed -f argument. Default 1bp.
#' @param my.values Bed file where fourth column are values to append to the SVM matrix
#' @param feature.ranking File with ranked features (Output of rankFeatures). It is used for scoring a query bed file
#' @param feature.nb Integer with feature.nb
#' @param crm.feature.file  Path to feature matrix file
#' @param stdout.log.file  Path to standard output log
#' @param stderr.log.file  Path to error log
#' @return A list
#' \item{feature.matrix}{a data frame where each row is a region and each column a feature, each cell carry a score, the first column is the response vector}
#' \item{stdout.log}{Standard output log of mapFeaturesToCRMs script in server}
#' \item{stderr.log}{Standard error log of mapFeaturesToCRMs script in server}
#' @examples
#' \dontrun{
#'  dirPath <- system.file("extdata", package="LedPred")
#'  file.list <-   list.files(dirPath, full.names=TRUE)
#'  background.freqs <- file.list[grep("freq", file.list)]
#'  positive.regions <-  file.list[grep("positive", file.list)]
#'  negative.regions <-  file.list[grep("negative", file.list)]
#'  TF.matrices <-  file.list[grep("tf", file.list)]
#'  ngs.path <- system.file("extdata/ngs", package="LedPred")
#'  ngs.files=list.files(ngs.path, full.names=TRUE)
#'  crm.features.list <- mapFeaturesToCRMs(positive.bed=positive.regions,
#'      negative.bed=negative.regions,  background.freqs=background.freqs,
#'      pssm=TF.matrices, genome="dm3", ngs=ngs.files,
#'      crm.feature.file = "crm.features.tab",
#'      stderr.log.file = "stderr.log", stdout.log.file = "stdout.log")
#'  names(crm.features.list)
#'  class(crm.features.list$crm.features)
#'  crm.features.list$stdout.log
#'  crm.features.list$stderr.log
#'}

mapFeaturesToCRMs <-
  function(URL = 'http://ifbprod.aitorgonzalezlab.org/map_features_to_crms.php', positive.bed =
             NULL, genome = NULL, negative.bed = NULL, shuffling = NULL, background.seqs =
             NULL, genome.info = NULL, pssm = NULL, background.freqs = NULL, ngs = NULL, bed.overlap =
             NULL, my.values =
             NULL, feature.ranking = NULL, feature.nb = NULL, crm.feature.file = NULL, stderr.log.file =
             NULL, stdout.log.file = NULL) {
    message("mapFeaturesToCRMs is running ...")
    postForm.cmd = "RCurl::postForm("
    if (!is.null(URL)) {
      postForm.cmd = paste(postForm.cmd,"uri='",URL,"'", sep = "")
    } else {
      stop("Server URL is missing...")
    }
    
    if (!RCurl::url.exists(URL)) {
      stop('This URL does not exist.')
    }
    
    if (!is.null(positive.bed)) {
      postForm.cmd = paste(
        postForm.cmd,", positive_bed=RCurl::fileUpload(filename = '",positive.bed,"')", sep =
          ""
      )
    } else {
      stop("positive.bed is missing...")
    }
    
    if (!is.null(genome)) {
      postForm.cmd = paste(postForm.cmd,", genome='",genome,"'", sep = "")
    } else {
      #Genome (dm3,mm9...)
      stop("Genome is missing...")
    }
    
    if (!is.null(negative.bed)) {
      postForm.cmd = paste(
        postForm.cmd,", negative_bed=RCurl::fileUpload(filename = '",negative.bed,"')", sep =
          ""
      )
    } else if (!is.null(shuffling)) {
      if (!is.null(shuffling) &&
          !is.null(background.seqs) && !is.null(genome.info)) {
        postForm.cmd = paste(postForm.cmd,", shuffling=",shuffling, sep = "")
        postForm.cmd = paste(
          postForm.cmd,", background_seqs=RCurl::fileUpload(filename = '",background.seqs,"')", sep =
            ""
        )
        postForm.cmd = paste(
          postForm.cmd,", genome_info=RCurl::fileUpload(filename = '",genome.info,"')", sep =
            ""
        )
      } else {
        stop("negative.bed or shuffling/background.seqs/genome_info are missing...")
      }
    } else {
      # no negative -> prediction
      empty.file = tempfile(pattern = "file", tmpdir = tempdir())
      empty.file = gsub("\\\\", "/", empty.file)
      file.create(empty.file)
      postForm.cmd = paste(
        postForm.cmd,", negative_bed=RCurl::fileUpload(filename = '",empty.file,"')", sep =
          ""
      )
    }
    
    if ((!is.null(pssm) &&
         is.null(background.freqs)) &&
        (is.null(pssm) && !is.null(background.freqs))) {
      stop("Both pssm and background.freqs are needed...")
    } else if (!is.null(pssm) && !is.null(background.freqs)) {
      postForm.cmd = paste(postForm.cmd,", pssm=RCurl::fileUpload(filename = '",pssm,"')", sep =
                             "")
      postForm.cmd = paste(
        postForm.cmd,", background_freqs=RCurl::fileUpload(filename = '",background.freqs,"')", sep =
          ""
      )
    }
    
    if (!is.null(ngs)) {
      postForm.cmd.ngs = lapply(ngs, function(x) {
        sprintf("'ngs[]'=RCurl::fileUpload(filename = '%s')", x)
      })
      postForm.cmd.ngs = paste0(unlist(postForm.cmd.ngs), collapse = ",")
      postForm.cmd = paste(postForm.cmd, postForm.cmd.ngs, sep = ",")
      if (!is.null(bed.overlap)) {
        postForm.cmd = paste(postForm.cmd,", f='",bed.overlap,"'", sep = "")
      }
    }
    
    if (!is.null(my.values)) {
      postForm.cmd.my.values = lapply(my.values, function(x) {
        sprintf("'my_values[]'=RCurl::fileUpload(filename = '%s')", x)
      })
      postForm.cmd.my.values = paste0(unlist(postForm.cmd.my.values), collapse =
                                        ",")
      postForm.cmd = paste(postForm.cmd, postForm.cmd.my.values, sep = ",")
    }
    
    if (!is.null(feature.ranking) && !is.null(feature.nb)) {
      feature.ranking.file = tempfile(pattern = "file", tmpdir = tempdir())
      feature.ranking.file = gsub("\\\\", "/", feature.ranking.file)
      write.table(
        feature.ranking, file = feature.ranking.file, quote = FALSE, row.names = FALSE
      )
      postForm.cmd = paste(
        postForm.cmd,", feature_rank=RCurl::fileUpload(filename = '",feature.ranking.file,"')", sep =
          ""
      )
      postForm.cmd = paste(postForm.cmd,", feature_nb=",feature.nb, sep =
                             "")
    }
    
    postForm.cmd = paste(postForm.cmd,", .encoding='utf-8')", sep = "")
    json = eval(parse(text = postForm.cmd))
    json.decoded <- jsonlite::fromJSON(json)
	while (!RCurl::url.exists(json.decoded[['stdoutLog']])) { Sys.sleep(5) }
	match=FALSE
	while (!match){ Sys.sleep(5); data=readLines(json.decoded[['stdoutLog']]); match=any(grepl('Finished', data)) };
    crm.feature.tab <-
      read.table(json.decoded$crmFeatures, fill = TRUE)
    crm.feature.gr <- .crmFeatureDfToGranges(crm.feature.tab)
    stdout.log = RCurl::getURL(json.decoded$stdoutLog)
    stderr.log = RCurl::getURL(json.decoded$stderrLog)
    if (!is.null(crm.feature.file)) {
      write.table(crm.feature.tab, file = crm.feature.file, quote = FALSE, col.names=NA, sep="\t")
    }
    if (!is.null(stdout.log.file)) {
      download.file(json.decoded$stdoutLog, destfile = stdout.log.file)
    }
    if (!is.null(stderr.log.file)) {
      download.file(json.decoded$stderrLog, destfile = stderr.log.file)
    }
    return(
      list(
        crm.features = crm.feature.gr, stdout.log = stdout.log, stderr.log = stderr.log
      )
    )
  }

.makeCrossValidSets <- function(data, valid.times) {
  nrows = nrow(data)
  perm.ind.pos = which(data$cl == 1)
  nrows.pos = length(perm.ind.pos)
  perm.ind.neg = which(data$cl == -1)
  nrows.neg = length(perm.ind.neg)
  if (valid.times > nrows.pos)
    stop(sQuote("cross"), " must not exceed positive or negative set sizes!")
  if (valid.times == 1)
    stop(sQuote("cross"), " must be greater than 1!")
  test.ind.pos = tapply(1:nrows.pos, cut(1:nrows.pos, breaks =  valid.times), function(x)
    perm.ind.pos[x])
  test.ind.neg = tapply(1:nrows.neg, cut(1:nrows.neg, breaks =  valid.times), function(x)
    perm.ind.neg[x])
  test.ind = lapply(1:length(test.ind.pos), function (x)
    c(test.ind.pos[[x]], test.ind.neg[[x]]))
  return(test.ind)
}


.plotCostGamma <- function(c.g.obj) {
  c.g.perf <- c.g.obj$performances
  if (length(c.g.obj$best.parameter) == 1) {
    c.g.perf[,"gamma"] <- 1:nrow(c.g.perf)
  }
  
  xyz = akima::interp(c.g.perf$cost, c.g.perf$gamma, c.g.perf$error)
  p = plot3D::image2D(
    xyz$z, x = xyz$x, y = xyz$y, facets = FALSE, log = "xy",
    xlim = c(min(xyz$x[which(xyz$x > 0)]), max(x = xyz$x)),
    ylim = c(min(xyz$y[which(xyz$y > 0)]), max(x = xyz$y)),
    xlab = "cost", ylab = "gamma"
  )
  cost = c.g.obj$best.parameters$cost
  gamma = c.g.obj$best.parameters$gamma
  title(paste("best cost:", cost,' and gamma:', gamma))
  points (c.g.perf$cost,c.g.perf$gamma, pch = "x", cex = 2)
}

.resp <- function(formula, data) {
  m.resp <- model.response(model.frame(formula, data))
  return(m.resp)
}

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

.checkData <- function(data,cl) {
  if (is.data.frame(data)) {
    data <- as.data.frame(data)
    
  }
  else{
    stop(
      sQuote("data"), " must be a data.frame object containing the predictors and the response at the first column (by default) or at the column specified by the argument Y"
    )
  }
  
  # set the formula
  colnames(data)[cl] <- "cl"
  return(data)
}

#' Tuning the SVM parameters
#'
#' The \code{mcTune} function is a modified version of the function \code{tune}  from package e1071  [6]. It tests the different combinations of C and gamma parameters given as vectors in a list and will return the prediction error computed during the cross-validation step.
#'
#' @param data data.frame containing the training set
#' @param data.granges Bioconductor GenomicRanges object containing the training set
#' @param cl integer indicating the column number corresponding to the response vector that classify positive and negative regions (default = 1)
#' @param ranges list object containing one (linear kernel) or two (radial kernel) vectors of integers corresponding to SVM cost and SVM gamma parameters to test.
#' @param kernel SVM kernel, a character string: "linear" or "radial". (default = "radial")
#' @param scale Logical indicating if the data have to be scaled or not (default = FALSE)
#' @param valid.times Integer indicating how many times the training set will be split for the cross validation step (default = 10). This number must be smaller than positive and negative sets sizes.
#' @param file.prefix A character string that will be used as a prefix followed by "_c_g_eval.png" for result plot files, if it is NULL (default), no plot is returned
#' @param numcores Number of cores to use for parallel computing (default: the number of available cores in the machine - 1)
#' @return A list of class \code{tune}
#' \item{best.parameters}{A list of the parameters giving the lowest misclassification error}
#' \item{best.performance}{The lowest misclassification error}
#' \item{method}{The method used}
#' \item{nparcomb}{the number of tested parameter combinations}
#' \item{train.ind}{The indexes used to produce subsets during the cross validation step}
#' \item{sampling}{The cross-validation fold number}
#' \item{performances}{A matrix summarizing the cross-validation step with the error for each tested parameter at each round and the dispersion of these errors (regarding to the average error)}
#' \item{best.model}{The model produced by the best parameters}
#' @examples
#' data(crm.features)
#' cost.vector <- c(1,3,10,30)
#' gamma.vector <- c(1,3,10,30)
#' c.g.obj <- mcTune(data.granges= crm.features, ranges = list(cost=cost.vector,
#'     gamma=gamma.vector), kernel='linear', file.prefix = "test")
#' names(c.g.obj)
#'  cost <- c.g.obj$best.parameters$cost
#'  gamma <- c.g.obj$best.parameters$gamma

mcTune <- function(data = NULL, data.granges = NULL, cl = 1,
                   ranges = list(gamma=c(1,10), cost=c(1,10)), kernel = "linear",scale = FALSE,
                   valid.times = 10,
                   file.prefix = NULL, numcores = parallel::detectCores() -
                     1) {
  message("mcTune is running ...")
  call <- match.call()
  
  if (!is.null(data.granges)) {
    data = .crmFeaturesToDf(data.granges)
  }
  
  
  ## Set the method to be tuned
  method <- 'svm'
  ## Set control method to tune.control() from e1071 package
  if (!is.numeric(valid.times)) {
    stop(sQuote("valid.times"), " must be a numeric value")
  }
  tunecontrol <-
    e1071::tune.control(sampling = "cross", cross = valid.times)
  
  
  
  ## parameter handling
  data[,cl] <- as.numeric(as.vector(data[,cl]))
  data <- .checkData(data,cl)
  
  # set the formula
  
  cl <- as.factor(as.vector(data[,cl]))
  
  train.x <- cl ~ .
  useFormula = TRUE
  
  # Set the method to predict
  predict.func <- predict
  
  
  ## prepare training indices
  
  
  train.ind <- .makeCrossValidSets(data, valid.times)
  ## find best model
  parameters <- (if (is.null(ranges))
    data.frame(dummyparameter = 0)
    else
      expand.grid(ranges))
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
  train_results <-
    do.call(
      what = get("mclapply", asNamespace("parallel")), args = c(mc.cores = numcores, list(
        X = 1:p, FUN = function(para.set) {
          sampling.errors <- c()
          sampling.confusions <- c()
          ## - loop over all training samples
          for (sample in 1:length(train.ind)) {
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
              
              if (kernel == "linear") {
                model <-
                  e1071::svm(
                    cl ~ .,data,subset = train.ind[[sample]], scale = scale, kernel = kernel,cost =
                      pars$cost
                  )
              }
              else{
                model <-
                  e1071::svm(
                    cl ~ .,data,subset = train.ind[[sample]], scale = scale, kernel = kernel,cost =
                      pars$cost,gamma = pars$gamma
                  )
              }
              
              
              ## predict validation set
              
              
              pred <-
                predict.func(model,data[-train.ind[[sample]],])
              
              ## compute performance measure
              true.y <- .resp(train.x, data[-train.ind[[sample]],])
              
              if (is.null(true.y))
                true.y <- rep(TRUE, length(pred))
              
              if (!is.null(tunecontrol$error.fun))
                repeat.errors[reps] <-
                tunecontrol$error.fun(true.y, pred)
              else if ((is.logical(true.y) ||
                        is.factor(true.y)) &&
                       (is.logical(pred) ||
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
  c.g.obj <-
    structure(
      list(
        best.parameters  = parameters[best,,drop = FALSE],
        best.performance = model.errors[best],
        method           = "svm",
        nparcomb         = nrow(parameters),
        train.ind        = train.ind,
        sampling         = switch(
          tunecontrol$sampling,
          fix = "fixed training/validation set",
          bootstrap = "bootstrapping",
          cross = if (tunecontrol$cross == nrow(data))
            "leave-one-out"
          else
            paste(tunecontrol$cross,"-fold cross validation", sep =
                    "")
        ),
        performances     = if (tunecontrol$performances)
          cbind(parameters, error = model.errors, dispersion = model.variances),
        best.model       = if (tunecontrol$best.model) {
          modeltmp <-
            do.call(what = get(method, asNamespace("e1071")), c(
              list(train.x, data = data), scale = scale, kernel = kernel,
              pars
            ))
          
          call[[1]] <- as.symbol("best.tune")
          modeltmp$call <- call
          modeltmp
        }
      ),
      class = "tune"
      
    )
  if (!is.null(file.prefix)) {
    png(paste(file.prefix,"_c_g_eval.png",sep = ""))
    .plotCostGamma(c.g.obj)
    garb = dev.off()
  }
  return(c.g.obj)
}

#' Ranking the features according to their importance
#'
#' The \code{rankFeatures} function  performs a Recursive Feature Elimination (RFE) on subsets of the feature matrix. For each subset the features are ranked according to the weight attributed by SVM at each round of elimination and the average rank of each feature over the subsets is returned. We recommand to save the object containing the ranked features for the following steps.
#'
#' @param data data.frame containing the training set
#' @param data.granges Bioconductor GenomicRanges object containing the training set
#' @param cl integer indicating the column number corresponding to the response vector that classify positive and negative regions (default = 1)
#' @param halve.above During RFE, all the features are ranked at the first round and the half lowest ranked features (that contribute the least in the model) are removed for the next round. When the number of feauture is lower or equal to halve.above, the features are removed one by one. (default=100)
#' @param valid.times Integer indicating how many times the training set will be split (default = 10). This number must be smaller than positive and negative sets sizes.
#' @param kernel SVM kernel, a character string: "linear" or "radial". (default = "radial")
#' @param scale Logical indicating if the data have to be scaled or not (default = FALSE)
#' @param cost The SVM cost parameter for both linear and radial kernels. If NULL (default), the function \code{mcTune} is run.
#' @param gamma The SVM gamma parameter for radial kernel. If radial kernel and NULL (default), the function \code{mcTune} is run.
#' @param numcores Number of cores to use for parallel computing (default: the number of available cores in the machine - 1)
#' @param file.prefix A character string that will be used as a prefix for output file, if it is NULL (default), no file is writen.
#' @return A 3-columns data frame with ranked features. First column contains the feature names, the second the original position of the feature in the feature.matrix and the third the average rank over the subsets.
#' @examples
#' data(crm.features)
#' cost <- 1
#' gamma <- 1
#'  feature.ranking <- rankFeatures(data.granges=crm.features, cost=cost,gamma=gamma,
#'      kernel='linear', file.prefix = "test", halve.above=10)

rankFeatures = function(data = NULL, data.granges = NULL, cl = 1, halve.above = 100, valid.times =
                          10, kernel = "linear", cost = 1, gamma = 1, scale = FALSE, numcores =
                          parallel::detectCores() - 1, file.prefix = NULL) {
  message("rankFeatures is running ...")
  
  if (!is.null(data.granges)) {
    data = .crmFeaturesToDf(data.granges)
  }
  
  data[,cl] <- as.numeric(as.vector(data[,cl]))
  data <- .checkData(data,cl)
  col.cl <- cl
  cl <- as.numeric(as.vector(data[,cl]))
  form <- cl ~ .
  svm.params <-
    .checkSVMParam(cost,gamma,data,valid.times,kernel,scale)
  cost <- svm.params$cost
  gamma <- svm.params$gamma
  
  test.ind <- .makeCrossValidSets(data, valid.times)
  # Perform feature ranking on all training sets
  
  if (.Platform$OS.type == "windows") {
    numcores = 1
  }
  
  feature.ranking_results = parallel::mclapply(
    test.ind, .svmRFEWrap, data,col.cl, k = valid.times, halve.above = halve.above, kernel =
      kernel, cost = cost, gamma = gamma, scale = scale, mc.cores = numcores, mc.set.seed =
      FALSE
  )
  #if (kernel =="linear"){
  #model <- e1071::svm(form, data=data,  kernel=kernel, cost=cost, scale=scale, probability=TRUE)
  #}
  #else{
  #model <- e1071::svm(form, data=data,  kernel=kernel, cost=cost, gamma=gamma, scale=scale, probability=TRUE)
  #}
  #feature_coefs=as.data.frame(t(model$coefs) %*% model$SV)
  #browser()
  featureID = sort(apply(sapply(feature.ranking_results, function(x)
    sort(x$feature, index.return = TRUE)$ix), 1, mean), index = TRUE)$ix
  avg.rank  = sort(apply(sapply(feature.ranking_results, function(x)
    sort(x$feature, index.return = TRUE)$ix), 1, function (x)
      round(mean(x),5)), index = TRUE)$x
  feature.name = colnames(data[,-col.cl])[featureID]
  #feature_coefs = feature_coefs[feature.name]
  #feature.ranking = data.frame(FeatureName=feature.name, FeatureID=featureID, AvgRank=avg.rank, FeatureCoeffs=as.numeric(feature_coefs[1,]))
  feature.ranking = data.frame(FeatureName = feature.name, FeatureID = featureID, AvgRank =
                                 avg.rank)
  feature.ranking = feature.ranking[with(feature.ranking, order(AvgRank, FeatureName)),]
  if (!is.null(file.prefix)) {
    write.table(
      feature.ranking, file = paste(file.prefix,"_feature_ranking.txt",sep = ""), quote =
        FALSE, row.names = FALSE
    )
  }
  return(feature.ranking)
}

# Copyright (C) 2011  John Colby
# http://github.com/johncolby/SVM-RFE

.svmRFEWrap <-
  function(test.ind, X,Y, kernel, cost, gamma, scale, ...) {
    # Wrapper to run svmRFE function while omitting a given test fold
    train.data = X[-test.ind,]
    test.data  = X[test.ind,]
    
    # Rank the features
    features.ranked = .svmRFE(
      train.data,Y, kernel = kernel, cost = cost, gamma = gamma, scale = scale, ...
    )
    return(
      list(
        feature.ids = features.ranked, train.data.ids = row.names(train.data), test.data.ids =
          row.names(test.data)
      )
    )
  }

.getWeights <-
  function(test.ind, X,Y, kernel = kernel, cost = cost, gamma = gamma, scale =
             scale) {
    # Fit a SVM model and obtain feature weights
    train.data = X
    if (!is.null(test.ind))
      train.data = X[-test.ind,]
    set.seed(123)
    if (kernel == "linear") {
      svmModel = e1071::svm(
        train.data[,-Y], train.data[, Y], cost = cost, cachesize = 500, kernel =
          kernel, scale = scale, seed = 123
      )
    }
    else{
      svmModel = e1071::svm(
        train.data[,-Y], train.data[, Y], cost = cost, gamma = gamma, cachesize =
          500, kernel = kernel, scale = scale, seed = 123
      )
    }
    t(svmModel$coefs) %*% svmModel$SV
    
  }

.svmRFE <-
  function(X,Y, k, halve.above, kernel, cost, gamma, scale) {
    # Feature selection with Multiple SVM Recursive Feature Elimination (RFE) algorithm
    n = ncol(X) - 1
    
    # Scale data up front so it doesn't have to be redone each pass
    cat('Scaling data...')
    #X[, -1] = scale(X[, -1])
    cat('Done!\n')
    flush.console()
    
    pb = txtProgressBar(1, n, 1, style = 3)
    
    i.surviving = 1:n
    i.ranked    = n
    ranked.list = vector(length = n)
    
    # Recurse through all the features
    while (length(i.surviving) > 0) {
      if (k > 1) {
        # Subsample to obtain multiple weights vectors (i.e. mSVM-RFE)
        set.seed(123)
        folds = rep(1:k, len = nrow(X))[sample(nrow(X))]
        folds = lapply(1:k, function(x)
          which(folds == x))
        
        # Obtain weights for each training set
        w = lapply(
          folds, .getWeights, X[, c(1, 1 + i.surviving)], Y = Y, kernel = kernel, cost =
            cost, gamma = gamma, scale = scale
        )
        w = do.call(rbind, w)
        
        # Normalize each weights vector
        w = t(apply(w, 1, function(x)
          x / sqrt(sum(x ^ 2))))
        
        # Compute ranking criteria
        v    = w * w
        vbar = apply(v, 2, mean)
        vsd  = apply(v, 2, sd)
        c    = vbar / vsd
      } else {
        # Only do 1 pass (i.e. regular SVM-RFE)
        w = .getWeights(
          NULL, X[, c(1, 1 + i.surviving)], Y = Y,kernel = kernel, cost = cost, gamma =
            gamma, scale = scale
        )
        c = w * w
      }
      
      # Rank the features
      ranking = sort(c, index.return = TRUE)$ix
      if (length(i.surviving) == 1) {
        ranking = 1
      }
      
      if (length(i.surviving) > halve.above) {
        # Cut features in half until less than halve.above
        nfeat = length(i.surviving)
        ncut  = round(nfeat / 2)
        n     = nfeat - ncut
        
        cat('Features halved from', nfeat, 'to', n, '\n')
        flush.console()
        
        pb = txtProgressBar(1, n, 1, style = 3)
        
      } else
        ncut = 1
      
      # Update feature list
      ranked.list[i.ranked:(i.ranked - ncut + 1)] = i.surviving[ranking[1:ncut]]
      i.ranked    = i.ranked - ncut
      i.surviving = i.surviving[-ranking[1:ncut]]
      
      setTxtProgressBar(pb, n - length(i.surviving))
      flush.console()
    }
    
    close(pb)
    
    return (ranked.list)
  }

.checkSVMParam <-
  function(cost,gamma,data,valid.times,kernel,scale) {
    cl <- which(colnames(data) == "cl")
    if (is.null(cost) | is.null(gamma)) {
      message(
        "You have not defined cost and/or gamma parameters: mcTune function will be used to compute them ..."
      )
      if (kernel == "radial") {
        if (!is.null(gamma)) {
          gamma.vector = gamma
          cost.vector = c(1, 3, 10, 30, 100) # cost spaces
        }
        else if (!is.null(cost)) {
          cost.vector <- cost
          gamma.vector = c(1, 3, 10, 30, 100) # gamma space
        }
        else{
          gamma.vector = c(1, 3, 10, 30, 100) # gamma space
          cost.vector = c(1, 3, 10, 30, 100) # cost spaces
        }
        c.g.obj <-
          mcTune(
            data, cl, ranges = list(cost = cost.vector,gamma = gamma.vector), kernel =
              kernel,scale = scale, valid.times = valid.times, numcores = parallel::detectCores() -
              1
          )
        cost <- c.g.obj$best.parameters$cost
        gamma <- c.g.obj$best.parameters$gamma
      }
      else if (kernel == "linear") {
        if (is.null(cost)) {
          cost.vector <- c(1, 3, 10, 30, 100)
          
          c.g.obj <-
            mcTune(
              data, cl, ranges = list(cost = cost.vector), kernel = kernel,scale = scale, valid.times = valid.times, numcores =
                parallel::detectCores() - 1
            )
          cost <- c.g.obj$best.parameters$cost
          gamma <- NULL
        }
      }
      
    }
    svm.params <- list(cost = cost,gamma = gamma)
    return(svm.params)
  }

#' Selecting the optimal number of features
#'
#' \code{tuneFeatureNb} iterates through increasing feature numbers to calculate kappa values which represents the performance of the model computed with the given features.  We recommand to save the object containing the optimal number of features for the following steps.
#'
#' @param data data.frame containing the training set
#' @param data.granges Bioconductor GenomicRanges object containing the training set
#' @param cl integer indicating the column number corresponding to the response vector that classify positive and negative regions (default = 1)
#' @param feature.ranking List of ordered features.
#' @param kernel SVM kernel, a character string: "linear" or "radial". (default = "radial")
#' @param scale Logical indicating if the data have to be scaled or not (default = FALSE)
#' @param cost The SVM cost parameter for both linear and radial kernels. If NULL (default), the function \code{mcTune} is run.
#' @param gamma The SVM gamma parameter for radial kernel. If radial kernel and NULL (default), the function \code{mcTune} is run.
#' @param step.nb Number of features to add at each step (default = 10)
#' @param numcores Number of cores to use for parallel computing (default: the number of available cores in the machine - 1)
#' @param file.prefix A character string that will be used as a prefix followed by "_kappa_measures.png" for the result plot file. If it is NULL (default), no plot is returned
#' @param valid.times Integer indicating how many times the training set will be split for the cross validation step (default = 10). This number must be smaller than positive and negative sets sizes.
#' @return A list with two objects.
#' \item{performance}{2-columns data frame. first column correspond to the number of tested features, second column contains the corresponding kappa value}
#' \item{best.feature.nb}{Integer corresponding to the number of features producing the model with the highest kappa value}
#' @examples
#' data(crm.features)
#' data(feature.ranking)
#' cost <- 1
#' gamma <- 1
#'  feature.nb.obj <- tuneFeatureNb(data.granges=crm.features,
#'      feature.ranking=feature.ranking, kernel='linear', cost=cost,gamma=gamma,
#'      file.prefix = "test")
#'  names(feature.nb.obj)

tuneFeatureNb = function(data = NULL, data.granges = NULL, feature.ranking = NULL, cl = 1, valid.times =
                           10, cost = 1, gamma = 1, kernel = "linear",scale = FALSE, step.nb = 10, numcores = parallel::detectCores() - 1,file.prefix = NULL) {
  message("tuneFeatureNb is running ...")
  
  if (!is.null(data.granges)) {
    data = .crmFeaturesToDf(data.granges)
  }
  
  data[,cl] <- as.numeric(as.vector(data[,cl]))
  data <- .checkData(data,cl)
  col.cl <- cl
  cl <- as.numeric(as.vector(data[,cl]))
  form <- cl ~ .
  
  svm.params <-
    .checkSVMParam(cost,gamma,data,valid.times,kernel,scale)
  cost <- svm.params$cost
  gamma <- svm.params$gamma
  test.ind <- .makeCrossValidSets(data, valid.times)
  
  feature_nb_vector = seq(from = 10, to = (ncol(data) - 1), by = step.nb)
  
  set.seed(123)
  
  if (.Platform$OS.type == "windows") {
    numcores = 1
  }
  feature_kappa = parallel::mclapply(
    feature_nb_vector, .calculateFeatureKappa, data = data, feature.ranking =
      feature.ranking, test.ind = test.ind, cost = cost, kernel = kernel, gamma =
      gamma, scale = scale, mc.cores = numcores, mc.set.seed = FALSE, mc.preschedule = TRUE
  )
  # Plot feature/kappa
  #  browser()
  kappa = sapply(feature_kappa, function(x)
    ifelse(is.null(x), NA, x$kappa_val))
  feature_nb = sapply(feature_kappa, function(x)
    ifelse(is.null(x), NA, x$feature_n))
  best.feature.nb = feature_nb[min(which(kappa == max(kappa)))]
  feature_nb_obj <-
    list(
      performance = data.frame(feature_nb = feature_nb, kappa = kappa), best.feature.nb =
        best.feature.nb
    )
  if (!is.null(file.prefix)) {
    png(paste(file.prefix,"_kappa_measures.png",sep = ""))
    .plotKappa(feature_nb_obj)
    garb = dev.off()
  }
  return(feature_nb_obj)
  
}


.calculateFeatureKappa = function(feature_n, data = data, feature.ranking =
                                    feature.ranking, test.ind = test.ind, cost = cost, kernel = kernel, gamma =
                                    gamma, scale = scale) {
  feature.ranking_this = feature.ranking[1:feature_n,1]
  data.feature = data[, c("cl", as.character(feature.ranking_this))]
  fold.kappa.list = lapply(
    test.ind, .calculateKappa, data = data.feature, kernel = kernel, cost =
      cost, gamma = gamma, scale = scale
  )
  fold_average_kappa_val = mean(sapply(fold.kappa.list, function(x)
    x$kappa_val))
  return (list(feature_n = feature_n, kappa_val = fold_average_kappa_val))
}

.calculateKappa = function(testi = testi, data = data, kernel = kernel, cost =
                             cost, gamma = gamma, scale = scale) {
  data$cl <- factor(data$cl)
  traini = (1:nrow(data))[-testi]
  trainset = data[traini,]
  testset = data[testi,]
  set.seed(123)
  if (kernel == "linear") {
    classfit <-
      e1071::svm(
        cl ~ .,data = trainset,probability = TRUE,cost = cost,kernel = kernel,scale =
          FALSE
      )
  }
  else{
    classfit <-
      e1071::svm(
        cl ~ .,data = trainset,probability = TRUE,cost = cost,kernel = kernel,gamma =
          gamma,scale = FALSE
      )
  }
  library(e1071)
  classpred <- predict(classfit,testset[,-1],probability = TRUE)
  #detach("package:e1071", unload=TRUE)
  kappa.val = irr::kappa2(cbind(testset$cl,classpred))$value # kappa=0.83, 260-1 features
  
  return(list(kappa_val = kappa.val))
}


.plotKappa <- function(feature_nb_obj) {
  feature_performance = feature_nb_obj$performance
  feature_nb_best = feature_nb_obj$best.feature.nb
  plot(feature_performance[,"feature_nb"], feature_performance[,"kappa"], xlab =
         "Feature nb", ylab = "kappa")
  lines(feature_performance[,"feature_nb"], feature_performance[,"kappa"])
  title(paste(
    'Feature nb performance. Selected feature nb:', feature_nb_best, sep = ""
  ))
}

.calculatePredictionProbability = function(testi, data = data, cl=1, kernel = "radial", scale = FALSE, cost = NULL, gamma = NULL, feature.ranking=NULL, feature.nb=NULL) {
  trainset = data[-testi,]
  testset = data[testi,]
classfit <- createModel(data = trainset, cl = cl,kernel = kernel, scale = scale,cost = cost,gamma =
             gamma, feature.ranking = feature.ranking,feature.nb = feature.nb)
  library(e1071)
  #classpred = predict(svm.model, testset[,-1], probability = TRUE)
  classpred = predict(classfit, testset[,-1], probability = TRUE)
  #detach("package:e1071", unload=TRUE)
  probs = attr(classpred,"probabilities")[,1]
  labels = testset$cl
  return (list(probs = probs, labels = labels))
}

#' Create the model with the optimal features
#'
#' \code{createModel} function creates a SVM model from the training data set with the selected features.
#'
#' @param data data.frame containing the training set
#' @param data.granges Bioconductor GenomicRanges object containing the training set
#' @param cl integer indicating the column number corresponding to the response vector that classify positive and negative regions (default = 1)
#' @param kernel SVM kernel, a character string: "linear" or "radial". (default = "radial")
#' @param scale Logical indicating if the data have to be scaled or not (default = FALSE)
#' @param cost The SVM cost parameter for both linear and radial kernels. If NULL (default), the function \code{mcTune} is run.
#' @param gamma The SVM gamma parameter for radial kernel. If radial kernel and NULL (default), the function \code{mcTune} is run.
#' @param valid.times Integer indicating how many times the training set will be split for the cross validation step (default = 10). This number must be smaller than positive and negative sets sizes.
#' @param feature.ranking List of ordered features.
#' @param feature.nb the optimal number of feature to use from the list of ordered features.
#' @param file.prefix A character string that will be used as a prefix followed by "_model.RData" for the resulting model file, if it is NULL (default), no model is saved
#' @return the best SVM model
#' @examples
#'     data(crm.features)
#'     cost <- 1
#'     gamma <- 1
#'     data(feature.ranking)
#'     feature.nb <- 70
#' svm.model <- createModel(data.granges=crm.features, cost=cost, gamma=gamma,
#'     feature.ranking=feature.ranking, feature.nb=feature.nb)
#' feature.weights <- as.data.frame(t(t(svm.model$coefs) %*% svm.model$SV))

createModel <-
  function(data = NULL, data.granges = NULL, cl = 1,kernel = "radial",scale = FALSE,cost = 1,gamma =
             1, valid.times = 10, feature.ranking = NULL,feature.nb = NULL,file.prefix = NULL) {
    message("createModel is running ...")
    
    
    if (!is.null(data.granges)) {
      data = .crmFeaturesToDf(data.granges)
    }
    
    data <- .checkData(data,cl)
    col.cl <- cl
    cl <- as.factor(as.vector(data[,cl]))
    
    selected.features <- feature.ranking[1:feature.nb,1]
    data <- data[,names(data) %in% selected.features]
    if (is.null(valid.times)) {
      valid.times = 10
    }
    svm.params <-
      .checkSVMParam(cost,gamma,data,valid.times,kernel,scale)
    cost <- svm.params$cost
    gamma <- svm.params$gamma
    data <- data.frame(cl = cl,data)
set.seed(123)
    if (kernel == "radial") {
      classfit <-
        e1071::svm(
          cl ~ .,data = data,kernel = kernel, gamma = gamma,cost = cost, scale = scale, probability =
            TRUE
        )
    } else if (kernel == "linear") {
      classfit <-
        e1071::svm(
          cl ~ .,data = data, kernel = kernel, cost = cost, scale = scale, probability =
            TRUE
        )
    }
    if (!is.null(file.prefix)) {
      save(classfit, file = paste(file.prefix,"_model.RData",sep = "")) # can save model for later use
    }
    
    
    return(classfit)
  }


#' Evaluate model performances
#'
#' \code{evaluateModelPerformance} function computes the precision and recall measures to evaluate the model through cross validation steps using \code{ROCR} package.
#'
#' @param data data.frame containing the training set
#' @param data.granges Bioconductor GenomicRanges object containing the training set
#' @param cl integer indicating the column number corresponding to the response vector that classify positive and negative regions (default = 1)
#' @param kernel SVM kernel, a character string: "linear" or "radial". (default = "radial")
#' @param scale Logical indicating if the data have to be scaled or not (default = FALSE)
#' @param cost The SVM cost parameter for both linear and radial kernels. If NULL (default), the function \code{mcTune} is run.
#' @param gamma The SVM gamma parameter for radial kernel. If radial kernel and NULL (default), the function \code{mcTune} is run.
#' @param valid.times Integer indicating how many times the training set will be split for the cross validation step (default = 10). This number must be smaller than positive and negative sets sizes.
#' @param svm.model the model to test
#' @param feature.ranking List of ordered features.
#' @param feature.nb the optimal number of feature to use from the list of ordered features.
#' @param file.prefix A character string that will be used as a prefix followed by "_ROCR_perf.png" for the result plot file, if it is NULL (default), no plot is returned
#' @param numcores Number of cores to use for parallel computing (default: the number of available cores in the machine - 1)
#' @return A list with two objects.
#' \item{probs}{The predictions computed by the model for each subset during the cross-validation}
#' \item{labels}{The actual class for each subset}
#' @examples
#'data(crm.features)
#' data(feature.ranking)
#' data(svm.model)
#'probs.labels.list <- evaluateModelPerformance(data.granges=crm.features,
#'    feature.ranking=feature.ranking, feature.nb=50,
#'    file.prefix = "test")
#'names(probs.labels.list[[1]])

#evaluateModelPerformance = function(data = NULL, data.granges = NULL, cl = 1, valid.times = 10, svm.model, feature.ranking, feature.nb, numcores =
#                                      parallel::detectCores() - 1, file.prefix = NULL) {
evaluateModelPerformance = function(data = NULL, data.granges = NULL, cl = 1, valid.times = 10, feature.ranking = NULL, feature.nb =NULL, numcores =
                                      parallel::detectCores() - 1, file.prefix = NULL, kernel = "linear", scale = FALSE, cost = 1, gamma = 1) {
  message("evaluateModelPerformance is running ...")
  
  if (!is.null(data.granges)) {
    data = .crmFeaturesToDf(data.granges)
  }
  
  cl <- data$cl
  
  selected.features <- feature.ranking[1:feature.nb,1]
  data <- data[,names(data) %in% selected.features]
  #browser()
  data <- data.frame(cl = cl,data)
  
  #  data <- .checkData(data,cl)
  
  col.cl <- cl
  
  valid.times.here = valid.times
  if (valid.times > floor(sum(cl == 1) / 2)) {
    valid.times.here = floor(sum(cl == 1) / 2)
  }
  test.ind <- .makeCrossValidSets(data, valid.times.here)
  #browser()
  
  if (.Platform$OS.type == "windows") {
    numcores = 1
  }
  
  cv.probs.labels = parallel::mclapply(
    test.ind, .calculatePredictionProbability, data = data, kernel = kernel, scale = scale, cost = cost, gamma = gamma, feature.ranking = feature.ranking, feature.nb = feature.nb, mc.cores =
      numcores, mc.set.seed = FALSE, mc.preschedule = TRUE
  )
  if (!is.null(file.prefix)) {
    labels = sapply(cv.probs.labels, function(x)
      x$labels)
    probs = sapply(cv.probs.labels, function(x)
      x$probs)
    png(paste(file.prefix,"_ROCR_perf.png",sep = ""))
    .plotRocr(labels, probs)
    garb = dev.off()
  }
  return(cv.probs.labels)
}

.plotRocr = function(labels, probs) {
  rocr.pred = ROCR::prediction(probs,labels,label.ordering = c(-1, 1))
  # These are the 4 performance measures in the plot
  prec_rec = ROCR::performance(rocr.pred, "prec", 'rec')
  prec = ROCR::performance(rocr.pred, measure = "prec")
  rec = ROCR::performance(rocr.pred, measure = "rec")
  tpr_fpr = ROCR::performance(rocr.pred, measure = "tpr", x.measure = "fpr")
  auc.tmp = ROCR::performance(rocr.pred, "auc");
  auc = round(mean(as.numeric(auc.tmp@y.values)), digits = 2)
  par(
    mfrow = c(2,2), oma = c(1,1,1,1), mar = c(5, 4.5, 0, 2) + 0.1
  )
  par(cex.lab = 1.5)
  ROCR::plot(
    prec, avg = "vertical", spread.estimate = "stderror", col = c("#FF0000FF"), ylim =
      c(0,1),xlim = c(0,1)
  )
  ROCR::plot(
    rec, avg = "vertical",spread.estimate = "stderror",col = c("#FF0000FF"),ylim =
      c(0,1),xlim = c(0,1)
  )
  ROCR::plot(
    prec_rec, avg = "vertical", spread.estimate = "stderror", col = c("#FF0000FF"), ylim =
      c(0,1), xlim = c(0,1)
  )
  ROCR::plot(
    tpr_fpr, avg = "vertical",spread.estimate = "stderror",col = c("#FF0000FF"),ylim =
      c(0,1),xlim = c(0,1)
  )
  text(0.7, 0.2, paste("AUC= ", auc, sep = ""),cex = 1.2)
}

#' Predicting new regulatory regions
#'
#' \code{scoreData} function predict new regulatory regions using SVM model from a test data set
#'
#' @param data data.frame containing the test set. This test set must have the same descriptive features as the one that were used to build the model.
#' @param data.granges Bioconductor GenomicRanges object containing the test set
#' @param model the SVM model
#' @param score.bed.file A character string that will be used as the file name for the output bed file, if it is NULL (default), no bed file is writen
#' @param score.file A character string that will be used as the file name for the output file, if it is NULL (default), no file is writen. The output file takes the form of two columns with object names and scores.
#' @return A 2-columns dataframe. First column containg the SVM model prediction probabilities and the second containing the corresponding regions
#' @examples
#' data(crm.features)
#' data(svm.model)
#' pred.test <- scoreData(data.granges=crm.features, model=svm.model,
#'  score.file="test_prediction.tab")

scoreData <-
  function(data = NULL, data.granges = NULL, model, score.file = NULL, score.bed.file = NULL) {
    message("scoreData is running ...")
    
    
    if (!is.null(data.granges)) {
      data = .crmFeaturesToDf(data.granges)
    }
    library(e1071)
    pred <- stats::predict(model, data, probability = TRUE)
    fit <- as.data.frame(attr(pred,"probabilities"))
    fit = fit[order(fit[,1], decreasing = TRUE),]
    
    scores <- data.frame(fit[,1], row.names = rownames(fit))
    colnames(scores) <- c("score")
    
    if (!is.null(score.file)) {
		write.table(scores, file=score.file, col.names=FALSE, quote=FALSE)
    return(scores)
	}

    if (is.null(data.granges)) {
      gr = .crmFeatureDfToGranges(scores)
    } else {
      gr = .crmFeatureDfToGranges(scores, genome = FALSE)
    }
    
    if (!is.null(score.bed.file)) {
      df <-
        data.frame(
          seqnames = seqnames(gr), starts = start(gr), ends = end(gr), names =
            c(rep(".", length(gr))), scores = score(gr), strands = strand(gr)
        )
      write.table(
        df, file = score.bed.file, quote = FALSE, sep = "\t", row.names = FALSE, col.names =
          FALSE
      )
    }
    return(scores)
  }

#########################################################################################
# A function to run the others

#' Creates an SVM model given a feature matrix
#'
#' The \code{LedPred} function computes the best SVM parameters, defines the optimal features for creating the SVM model by running sequentially \code{mcTune},  \code{rankFeatures},  \code{tuneFeatureNb} and  \code{createModel}. The performances of this model are then computed usong \code{evaluateModelPerformance}.
#'
#' @param data data.frame containing the training set
#' @param data.granges Bioconductor GenomicRanges object containing the training set
#' @param cl integer indicating the column number corresponding to the response vector that classify positive and negative regions (default = 1)
#' @param ranges list object containing one (linear kernel) or two (radial kernel) vectors of integers corresponding to SVM cost and SVM gamma parameters to test.
#' @param kernel SVM kernel, a character string: "linear" or "radial". (default = "radial")
#' @param scale Logical indicating if the data have to be scaled or not (default = FALSE)
#' @param valid.times Integer indicating how many times the training set will be split for the cross validation step (default = 10). This number must be smaller than positive and negative sets sizes.
#' @param file.prefix A character string that will be used as a prefix for the result files. If it is NULL (default), no plot is returned
#' @param step.nb Number of features to add at each step (default = 10)
#' @param numcores Number of cores to use for parallel computing (default: the number of available cores in the machine - 1)
#' @param halve.above During RFE, all the features are ranked at the first round and the half lowest ranked features (that contribute the least in the model) are removed for the next round. When the number of feauture is lower or equal to halve.above, the features are removed one by one. (default=100)
#' @return A list of the object produced at each step
#' \item{best.params}{A list of the parameters giving the lowest misclassification error}
#' \item{feature.ranking}{List of ordered features from \code{rankFeatures}}
#' \item{feature.nb}{he optimal number of feature to use from the list of ordered features from \code{tuneFeatureNb}}
#' \item{model.svm}{The best SVM model \code{createModel}}
#' \item{probs.label.list}{The cross-validation results from \code{evaluateModelPerformance}}
#' @examples
#'  data(crm.features)
#'  cost_vector <- c(1,3,10)
#'  gamma_vector <- c(1,3,10)
#'  ledpred.list=LedPred(data.granges=crm.features, cl=1, ranges = list(cost=cost_vector,
#'                            gamma=gamma_vector), kernel="linear", halve.above=50)
#'  names(ledpred.list)

LedPred <-
  function(data = NULL, data.granges = NULL, cl = 1, ranges = list(gamma=seq(from = 1 , to = 10 , by = 9), cost=seq(from = 1 , to = 10 , by = 9)), kernel = "linear", scale = FALSE, valid.times =
             10, file.prefix = NULL, numcores = parallel::detectCores() - 1, step.nb =
             20, halve.above = 100) {
     if (!is.null(data.granges)) {
     data = .crmFeaturesToDf(data.granges)
    }
#browser()
    
    c.g.obj <-
      mcTune(
        data = data, data.granges = data.granges, cl = cl, ranges = ranges, kernel = kernel, scale = scale, valid.times =
          valid.times, file.prefix = file.prefix, numcores = numcores
      )
    cost <- c.g.obj$best.parameters$cost
    gamma <- c.g.obj$best.parameters$gamma
    feature.ranking <-
      rankFeatures(
        data = data, data.granges = data.granges, cl = cl, halve.above = halve.above, valid.times = valid.times, kernel =
          kernel, cost = cost, gamma = gamma, scale = scale, numcores = numcores, file.prefix =
          file.prefix
      )
    feature.obj <-
      tuneFeatureNb(
        data = data, data.granges = data.granges, cl = cl, feature.ranking = feature.ranking,kernel = kernel, valid.times =
          valid.times, cost = cost, gamma = gamma, scale = scale, step.nb = step.nb, numcores =
          numcores, file.prefix = file.prefix
      )
    feature.nb = feature.obj$best.feature.nb
    write(feature.nb, paste(file.prefix, "_feature_nb.txt", sep=''))
    svm.model <-
      createModel(
        data = data, data.granges = data.granges, cl = cl, kernel=kernel, scale=scale, cost=cost, gamma = gamma, feature.ranking =
          feature.ranking, feature.nb=feature.nb, file.prefix=file.prefix,
      )
    probs.label.list <-
      evaluateModelPerformance(
        data = data, data.granges = data.granges, cl = cl, kernel=kernel, scale=scale, cost=cost, gamma = gamma, valid.times = valid.times, feature.ranking =
          feature.ranking, feature.nb = feature.nb, numcores = numcores, file.prefix = file.prefix
      )
    ledpred.summary <-
      list(
        best.parameters = c.g.obj$best.parameters,feature.ranking = feature.ranking,feature.nb =
          feature.nb,model.svm = svm.model,probs.label.list = probs.label.list
      )
    return(ledpred.summary)
  }
