load("mat.Rda")
load("fr.Rda")
load("testi.Rda")
fn = 370

test_that("evaluateModelPerformance features", {
  cv.probs.labels <-
    evaluateModelPerformance(data = mat, valid.times = 5, feature.ranking = fr, feature.nb = fn, file.prefix = "somefeatures")
  labels = sapply(cv.probs.labels, function(x)
    x$labels)
  probs = sapply(cv.probs.labels, function(x)
    x$probs)
  rocr.pred = ROCR::prediction(probs, labels)
  auc.vector <-
    slot(ROCR::performance(rocr.pred, "auc"), "y.values")
  auc <- mean(as.numeric(auc.vector), digits = 2)
  testthat::expect_equal(auc, 0.83625, tolerance = 1e-5) # need to check by hand if this is true
})

test_that("evaluateModelPerformance", {
  cv.probs.labels <-
    evaluateModelPerformance(data = mat, valid.times = 5, file.prefix = "allfeatures")
  labels = sapply(cv.probs.labels, function(x)
    x$labels)
  probs = sapply(cv.probs.labels, function(x)
    x$probs)
  rocr.pred = ROCR::prediction(probs, labels)
  auc.vector <-
    slot(ROCR::performance(rocr.pred, "auc"), "y.values")
  auc <- mean(as.numeric(auc.vector), digits = 2)
  testthat::expect_equal(auc, 0.66125, tolerance = 1e-5)
})

test_that(".calculatePredictionProbability", {
  prob.label.obj = .calculatePredictionProbability(
    testi, data = mat, cl = 1
  )
})
