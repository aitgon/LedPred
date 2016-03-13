crms = read.table('data_starrseq/crm_features.tab')
y = crms[,1]
x = crms[,-1]

#numcores=1
numcores = parallel::detectCores() - 1

feature.nb.vector = list(100, 200, 300, 400, 500, 600)

# -------------------------------------

test_that("starrseq_LedPred", {

obj <- LedPred$new(x = x, y = y, numcores=numcores, feature.nb.vector=feature.nb.vector)
scores = obj$model.obj$ScoreData(x=x)$scores

testthat::expect_true(all(obj$feature.ranking[c(1,2), 'FeatureName']==c('Myb', 'peaks_ets1.bed')))
testthat::expect_true(obj$best.feature.nb==200)
testthat::expect_true(all(rownames(obj$model$SV)[1:3]==c("mm9_chr10_110522706_110522889_+", "mm9_chr10_117089250_117089433_+", "mm9_chr10_122088945_122089304_+")))

#browser()
testthat::expect_equal(as.numeric(scores)[1], -1.053714, tolerance=1e-2)
#browser()
testthat::expect_true(all(obj$feature.ranking[c(11,12), 'AvgRank']==c(43.0, 45.6)))

})

test_that("starrseq_LedPred_wrapper", {

obj <- ledpred(x = x, y = y, numcores=numcores, feature.nb.vector=feature.nb.vector)
#scores = obj$model.obj$ScoreData(x=x)$scores


testthat::expect_true(all(obj$feature.ranking[c(1,2), 'FeatureName']==c('Myb', 'peaks_ets1.bed')))
testthat::expect_true(all(obj$feature.ranking[c(11,12), 'AvgRank']==c(43.0, 45.6)))
testthat::expect_true(obj$feature.nb==200)
testthat::expect_true(all(rownames(obj$model$SV)[1:3]==c("mm9_chr10_110522706_110522889_+", "mm9_chr10_117089250_117089433_+", "mm9_chr10_122088945_122089304_+")))

scores = scoreData(x=x, ledpred=obj)
#browser()
testthat::expect_equal(as.numeric(scores)[1], 1, tolerance=1e-2)
#browser()

})


