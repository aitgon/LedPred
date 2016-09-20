test_that("160812_ModelPerformance", {

data=read.table("data_var/learning_remap_160812.csv", sep="\t", header=TRUE, row.names=1)
ledpred = LedPred::LedPred(data=data, cl=1, valid.times=round(dim(data)[1]/5), step.nb=round(dim(data)[2]/5))
testthat::expect_equal(ledpred$cv.probs.labels$probs[[5]][[1]], 0.9801934, tolerance=1e-6)
}
)

