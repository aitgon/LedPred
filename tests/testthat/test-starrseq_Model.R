crms = read.table('data_starrseq/crm_features.tab')
y = crms[,1]
x = crms[,-1]

feature.ranking = read.table('data_starrseq/_feature_ranking.txt', header=TRUE)
best.feature.nb = 200
selected.features = as.character(feature.ranking$FeatureName[1:best.feature.nb])
x = x[,selected.features]

test_that("starrseq_Model", {
model.obj = Model$new( x = x, y = y)
model = model.obj$model

scores = model.obj$ScoreData(x=x)$scores

#browser()
testthat::expect_equal(as.numeric(scores)[1], -1.072615, tolerance=1e-5)
testthat::expect_equal(as.numeric(scoreData(x, model.obj))[1], -1.072615, tolerance=1e-5)
}
)

