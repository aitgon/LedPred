test_that("var_160919", {

fin <- 'data_var/training_160919.tsv' # data
file.prefix <- 'data_var/' # file.prefix

data=read.table(file=fin, header=T, sep="\t", row.names=1)
#ledpred=LedPred::LedPred(data=data, cl=1, valid.times=round(dim(data)[1]/5), step.nb=round(dim(data)[2]/5), file.prefix=file.prefix)
ledpred=LedPred::LedPred(data=data, cl=1, valid.times=5, step.nb=10)
testthat::expect_equal(ledpred$model$SV[1, 'H3K27ac.Colon_Crypt_1.youngH3K27ac_mix.zip'], 0.3687906, tolerance=1e-6)

}
)


