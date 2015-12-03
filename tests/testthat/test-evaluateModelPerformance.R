load("mat.Rda")
load("fr.Rda")
load("testi.Rda")
fn = 370

evaluateModelPerformance <-
  function(data = data, valid.times = valid.times) {
    testi.vector <- .makeCrossValidSets(data, valid.times)
    cv.probs.labels = parallel::mclapply(testi.vector, .calculatePredictionProbability, data)
    #print(cv.probs.labels[[1]])
    return(cv.probs.labels)
  }

test_that("evaluateModelPerformance", {
  cv.probs.labels <-
    evaluateModelPerformance(data = mat, valid.times = 5)
  labels = sapply(cv.probs.labels, function(x)
    x$labels)
  probs = sapply(cv.probs.labels, function(x)
    x$probs)
  #rocr.pred = ROCR::prediction(cv.probs.labels[[i]]$probs, cv.probs.labels[[i]]$labels)
  rocr.pred = ROCR::prediction(probs, labels)
  auc.vector <-
    slot(ROCR::performance(rocr.pred, "auc"), "y.values")
  auc <- mean(as.numeric(auc.vector), digits = 2)
  testthat::expect_equal(auc, 0.66125, tolerance = 1e-5)
})

test_that(".calculatePredictionProbability", {
  prob.label.obj = .calculatePredictionProbability(
    testi, data = mat, cl = 1, kernel = "linear", scale = FALSE, cost = 1, feature.ranking = fr, feature.nb = fn
  )
  probs = prob.label.obj$probs
  labels = prob.label.obj$labels

  rocr.pred = ROCR::prediction(
    predictions = probs, labels = labels, label.ordering = c(-1, 1)
  )
  testthat::expect_true(slot(ROCR::performance(rocr.pred, "auc"), "y.values") ==
                          0.6)
})
