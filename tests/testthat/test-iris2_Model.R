x=get(load(file="data_iris2/x.rda"))
y=get(load(file="data_iris2/y.rda"))

# -------------------------------------
valid.times=5

# -------------------------------------
feature.ranking = get(load('data_iris2/feature.ranking.rda'))
feature.nb.vector = list(2,4,6,8,10,12)


test_that("iris2_Model", {
model.obj = Model$new( x = x, y = y)
model = model.obj$model
testthat::expect_true(all(rownames(model$SV)[1:3]==c("21", "25", "35")))

scores = model.obj$ScoreData(x=x)$scores
testthat::expect_equal(as.numeric(scores)[1], 1.3936184, tolerance=1e-6)
}
)
