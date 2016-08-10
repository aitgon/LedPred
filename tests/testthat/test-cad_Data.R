test_that("cat_Data", {

crms = read.table('data_cad/crm_features_noscale.tab',sep="\t",header=TRUE,row.names=1)
y = crms[,1]
x = crms[,-1]
cost=1
kernel='linear'


# -------------------------------------

# -------------------------------------
data.obj = Data$new(x=x, y=y, cost=cost, kernel=kernel)
testthat::expect_equal(data.obj$x[1,'K4me1_E_F_6_8h_H3_subtracted.wig'], 0.03020928,tolerance=1e-7)
}
)

