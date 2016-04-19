crms = read.table('data_starrseq/crm_features.tab')
y = crms[,1]
x = crms[,-1]
cost=1

#numcores=1
numcores <- ifelse(.Platform$OS.type == "windows", 1,  parallel::detectCores() - 1)

feature.nb.vector = list(100, 200, 300, 400, 500, 600)
step.nb=200

# -------------------------------------

test_that("starrseq_LedPredClass", {

obj <- LedPredClass$new(x = x, y = y, numcores=numcores, feature.nb.vector=feature.nb.vector, cost=cost)
scores = obj$model.obj$ScoreData(x=x)$scores

testthat::expect_true(all(obj$feature.ranking[c(1,2), 'FeatureName']==c('peaks_ets1.bed', 'Myb')))
testthat::expect_true(obj$best.feature.nb==200)
testthat::expect_true(all(rownames(obj$model$SV)[1:3]==c("mm9_chr10_110522706_110522889_+", "mm9_chr10_117089250_117089433_+", "mm9_chr10_122088945_122089304_+")))

testthat::expect_equal(as.numeric(scores)[1], -1.07, tolerance=1e-1)
testthat::expect_true(all(obj$feature.ranking[c(11,12), 'AvgRank']==c(39.0, 41.4)))

})

test_that("starrseq_LedPred", {

obj <- LedPred(data = crms, cl = 1, numcores=numcores, step.nb=step.nb, cost=cost)

testthat::expect_true(all(obj$feature.ranking[c(1,2), 'FeatureName']==c('peaks_ets1.bed', 'Myb')))
testthat::expect_true(obj$feature.nb==200)
testthat::expect_true(all(rownames(obj$model$model$SV)[1:3]==c("mm9_chr10_110522706_110522889_+", "mm9_chr10_117089250_117089433_+", "mm9_chr10_122088945_122089304_+")))
testthat::expect_true(all(obj$feature.ranking[c(11,12), 'AvgRank']==c(33.6, 35.4)))

scores = scoreData(data=x, ledpred=obj)
testthat::expect_equal(as.numeric(scores)[1], 1, tolerance=1e-2)

})


