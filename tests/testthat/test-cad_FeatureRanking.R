test_that("cad_FeatureRanking", {

crms = read.table('data_cad/crm_features_noscale.tab',sep="\t",header=TRUE,row.names=1)
y = crms[,1]
x = crms[,-1]

# -------------------------------------
valid.times=5
kfold.nb=1

# -------------------------------------
feature.ranking.obj <-FeatureRanking$new(x = x, y = y, valid.times = valid.times, kfold.nb=kfold.nb)

testthat::expect_true(all(feature.ranking.obj$feature.ranking[c(1,2), 'FeatureName']==c('K4me1_E_F_6_8h_H3_subtracted.wig', 'X_Gsc_Gsc_CG34340_Ptx1_al_Ptx1_dve_bcd_oc_Gsc_oc_CG1_100')))
})

