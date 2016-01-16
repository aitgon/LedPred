x=get(load(file="data_iris2/x.rda"))
y=get(load(file="data_iris2/y.rda"))

# -------------------------------------
valid.times=5
numcores=1

test_that("iris2_Model", {

data.obj = Data$new(x=x, y=y)
testthat::expect_equal(data.obj$x[1,1], 0.07056264, tolerance=1e-7)
testthat::expect_equal(data.obj$scale.factors[[1]], 72.27621, tolerance=1e-7)
})


