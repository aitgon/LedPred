test_that("starrseq_FeatureRanking", {

crms = read.table('data_starrseq/crm_features.tab')
y = crms[,1]
x = crms[,-1]

# -------------------------------------
valid.times=5
kfold.nb=5
numcores=1

# -------------------------------------
feature.ranking.obj <-FeatureRanking$new(x = x, y = y, valid.times = valid.times, kfold.nb=kfold.nb)

testthat::expect_true(all(feature.ranking.obj$feature.ranking[c(1,2), 'FeatureName']==c('peaks_ets1.bed', 'GPAM.output_1')))
})

