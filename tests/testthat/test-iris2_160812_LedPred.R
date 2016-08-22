test_that("160812_ModelPerformance", {

data=read.table("data_iris2/train.csv", sep="\t", header=TRUE, row.names=1)
ledpred = LedPred::LedPred(data=data, cl=1, valid.times=5, step.nb=round(dim(data)[2]/5), numcores=1)
testthat::expect_equal(ledpred$cv.probs.labels$probs[[5]][[1]], 0.4092086, tolerance=1e-6)
}
)

