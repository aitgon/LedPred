test_that("var_ttt", {
#browser()
fin="data_var/data_160924/training.tsv"
file.prefix <- "data_var/data_160924/"
fin2=paste0(file.prefix, "/test.tsv")
fin3=paste0(file.prefix, "/_ledpred.rda")
fout=paste0(file.prefix, "/score.txt")

try(file.remove(fin3))

data=read.table(file=fin, header=T, sep="\t", row.names=1)
ledpred=LedPred::LedPred(data=data, cl=1, valid.times=2, step.nb=20, file.prefix=file.prefix, numcores=1)

test_data=read.table(file=fin2, header=T, sep="\t", row.names=1)
ledpred=get(load(fin3))
#browser()
scores <- LedPred::scoreData(test_data, ledpred=ledpred, score.file=fout)
#browser()
#testthat::expect_equal(scores[1], 0.9399665,tolerance=1e-6)

})

