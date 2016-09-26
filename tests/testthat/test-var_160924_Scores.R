test_that("var_ttt", {
#browser()
fin="data_var/data_160924/training.tsv"
file.prefix <- "data_var/data_160924/output/"
fin2="data_var/data_160924/test.tsv"
fin3=paste0(file.prefix, "_ledpred.rda")
fout=paste0(file.prefix, "score.txt")

try(file.remove(fin3))

data=read.table(file=fin, header=T, sep="\t", row.names=1)
ledpred=LedPred::LedPred(data=data, cl=1, valid.times=2, step.nb=20, file.prefix=NULL, numcores=1)

test_data=read.table(file=fin2, header=T, sep="\t", row.names=1)
#ledpred=get(load(fin3))
scores <- LedPred::scoreData(test_data, ledpred=ledpred, score.file=NULL)
testthat::expect_equal(scores[[1]], 0.9999459,tolerance=1e-6)

})

