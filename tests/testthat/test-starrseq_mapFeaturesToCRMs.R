URL="http://ifbprod.aitorgonzalezlab.org/map_features_to_crms.php"

background.seqs='mapFeaturesToCRMs_test_data/data/dmel-all-intergenic_and_introns-promoters+-500bp_r5.57.bed'
background.freqs='mapFeaturesToCRMs_test_data/data/2nt_intergenic_Drosophila_melanogaster.freq'
genome.info='mapFeaturesToCRMs_test_data/data/dm3ChromInfo.txt'

###### ---------------------

test_that("test_mapFeaturesToCRMs_small", {

positive.bed='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small/positive_2seqs.bed'
negative.bed='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small/negative_5seqs.bed'
pssm='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small/mfile2.tf'

feature.matrix.bak='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small/outdir_bak/feature_matrix.tab'

bed.to.matrix.list <- mapFeaturesToCRMs(URL=URL, positive.bed=positive.bed, genome='dm3', negative.bed=negative.bed, pssm=pssm, background.freqs=background.freqs)

feature.matrix <- bed.to.matrix.list$crm.features
testthat::expect_equal(feature.matrix["dm3_chr2L_5456541_5460910_+","dTCF"], 0.009889549, tolerance=1e-4)

})

##### ---------------------

test_that("test_ngs_bed_small_ngs_features", {

positive.bed='mapFeaturesToCRMs_test_data/test_ngs_bed_small_features/positive_2seqs.bed'
negative.bed='mapFeaturesToCRMs_test_data/test_ngs_bed_small_features/negative_5seqs.bed'
#ngs=list.files(path = "test_ngs_bed_small_features/ngs_beds", pattern = "*.bed", full.names=T)
ngs=list("mapFeaturesToCRMs_test_data/test_ngs_bed_small_features/ngs_beds/ngs3.bed", "mapFeaturesToCRMs_test_data/test_ngs_bed_small_features/ngs_beds/ngs2.bed", "mapFeaturesToCRMs_test_data/test_ngs_bed_small_features/ngs_beds/ngs.bed")

feature.ranking.file='mapFeaturesToCRMs_test_data/test_ngs_bed_small_features/feature_rank.txt'
feature.ranking=read.table(file=feature.ranking.file, header=T)

feature.nb=2

feature.matrix.bak='mapFeaturesToCRMs_test_data/test_ngs_bed_small_features/outdir_bak/feature_matrix.tab'

bed.to.matrix.list <- mapFeaturesToCRMs(URL=URL, positive.bed=positive.bed, genome='dm3', negative.bed=negative.bed, ngs=ngs, bed.overlap=0.2, feature.ranking=feature.ranking, feature.nb=feature.nb)

feature.matrix <- bed.to.matrix.list$crm.features
testthat::expect_true(feature.matrix["dm3_chr2L_3521334_3522149_+","ngs2.bed"]==1)
})

### ###### ---------------------

 test_that("test_mapFeaturesToCRMs_small_pssm_features", {

 positive.bed='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small_features/positive_2seqs.bed'
 negative.bed='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small_features/negative_5seqs.bed'
 pssm='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small_features/mfile4.tf'

 feature.ranking.file='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small_features/feature_rank.txt'
 feature.ranking=read.table(file=feature.ranking.file, header=T)

 feature.nb=2

 feature.matrix.bak='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small_features/outdir_bak/feature_matrix.tab'

 bed.to.matrix.list <- mapFeaturesToCRMs(URL=URL, positive.bed=positive.bed, genome='dm3', negative.bed=negative.bed, pssm=pssm, background.freqs=background.freqs, feature.ranking=feature.ranking, feature.nb=feature.nb)
 
feature.matrix <- bed.to.matrix.list$crm.features
testthat::expect_equal(feature.matrix["dm3_chr2L_5456541_5460910_+","dTCF"], 0.009889549, tolerance=1e-4)
 })

 #### ---------------------

 test_that("test_my_values", {

 positive.bed='mapFeaturesToCRMs_test_data/test_my_values/CRM_13-16.bed'
 negative.bed='mapFeaturesToCRMs_test_data/test_my_values/negative_CRM_13-16.bed'
 #my.values=list.files(path = "test_my_values/my_value_beds", pattern = "*.bed", full.names=T)
 my.values=list("mapFeaturesToCRMs_test_data/test_my_values/my_value_beds/my_values2.bed", "mapFeaturesToCRMs_test_data/test_my_values/my_value_beds/my_values.bed")

 feature.matrix.bak='mapFeaturesToCRMs_test_data/test_my_values/outdir_bak/feature_matrix.tab'

 bed.to.matrix.list <- mapFeaturesToCRMs(URL=URL, positive.bed=positive.bed, genome='dm3', negative.bed=negative.bed, my.values=my.values)

feature.matrix <- bed.to.matrix.list$crm.features
testthat::expect_equal(feature.matrix["dm3_chr2L_10197628_10197916_+","my_values2.bed"], 0.111, tolerance=1e-4)
 })


#### ---------------------

test_that("test_ngs_bed_small", {

positive.bed='mapFeaturesToCRMs_test_data/test_ngs_bed_small/positive_2seqs.bed'
negative.bed='mapFeaturesToCRMs_test_data/test_ngs_bed_small/negative_5seqs.bed'
#ngs=list.files(path = "test_ngs_bed_small/ngs_beds", pattern = "*.bed", full.names=T)
ngs=list("mapFeaturesToCRMs_test_data/test_ngs_bed_small/ngs_beds/ngs2.bed", "mapFeaturesToCRMs_test_data/test_ngs_bed_small/ngs_beds/ngs.bed")

feature.matrix.bak='mapFeaturesToCRMs_test_data/test_ngs_bed_small/outdir_bak/feature_matrix.tab'

bed.to.matrix.list <- mapFeaturesToCRMs(URL=URL, positive.bed=positive.bed, genome='dm3', negative.bed=negative.bed, ngs=ngs)

feature.matrix <- bed.to.matrix.list$crm.features
testthat::expect_true(feature.matrix["dm3_chr2L_3521331_3522140_+","ngs2.bed"]==1)
})


###### ---------------------

test_that("test_small_shuffling", {

positive.bed='mapFeaturesToCRMs_test_data/test_small_shuffling/positive_2seqs.bed'
shuffling=3
pssm='mapFeaturesToCRMs_test_data/test_small_shuffling/mfile2.tf'

feature.matrix.bak='mapFeaturesToCRMs_test_data/test_small_shuffling/outdir_bak/feature_matrix.tab'

bed.to.matrix.list <- mapFeaturesToCRMs(URL=URL, positive.bed=positive.bed, genome='dm3', shuffling=shuffling, genome.info=genome.info, background.seqs=background.seqs, pssm=pssm, background.freqs=background.freqs)

feature.matrix <- bed.to.matrix.list$crm.features
testthat::expect_equal(feature.matrix["dm3_chr2L_5456541_5460910_+","dTCF"], 0.009889549, tolerance=1e-8)
})

