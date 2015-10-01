URL="http://ifbdev.aitorgonzalezlab.org/map_features_to_crms.php"

background.seqs='mapFeaturesToCRMs_test_data/data/dmel-all-intergenic_and_introns-promoters+-500bp_r5.57.bed'
background.freqs='mapFeaturesToCRMs_test_data/data/2nt_intergenic_Drosophila_melanogaster.freq'
genome.info='mapFeaturesToCRMs_test_data/data/dm3ChromInfo.txt'

##### ---------------------

test_that("test_mapFeaturesToCRMs_small", {

positive.bed='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small/positive_2seqs.bed'
negative.bed='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small/negative_5seqs.bed'
pssm='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small/mfile2.tf'

feature.matrix.bak='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small/outdir_bak/feature_matrix.tab'

bed.to.matrix.list <- mapFeaturesToCRMs(URL=URL, positive.bed=positive.bed, genome='dm3', negative.bed=negative.bed, pssm=pssm, background.freqs=background.freqs)

feature.matrix <- bed.to.matrix.list[[1]]
testthat::expect_true(bed.to.matrix.list[[1]][1]$dTCF==0.00988954900424961)
})


 ##### ---------------------

 test_that("test_ngs_bed_small_features", {

 positive.bed='mapFeaturesToCRMs_test_data/test_ngs_bed_small_features/positive_2seqs.bed'
 negative.bed='mapFeaturesToCRMs_test_data/test_ngs_bed_small_features/negative_5seqs.bed'
 #ngs=list.files(path = "test_ngs_bed_small_features/ngs_beds", pattern = "*.bed", full.names=T)
 ngs=list("mapFeaturesToCRMs_test_data/test_ngs_bed_small_features/ngs_beds/ngs3.bed", "mapFeaturesToCRMs_test_data/test_ngs_bed_small_features/ngs_beds/ngs2.bed", "mapFeaturesToCRMs_test_data/test_ngs_bed_small_features/ngs_beds/ngs.bed")

 feature.ranking.file='mapFeaturesToCRMs_test_data/test_ngs_bed_small_features/feature_rank.txt'
 feature.ranking=read.table(file=feature.ranking.file, header=T)

 feature.nb=2

 feature.matrix.bak='mapFeaturesToCRMs_test_data/test_ngs_bed_small_features/outdir_bak/feature_matrix.tab'

 bed.to.matrix.list <- mapFeaturesToCRMs(URL=URL, positive.bed=positive.bed, genome='dm3', negative.bed=negative.bed, ngs=ngs, bed.overlap=0.2, feature.ranking=feature.ranking, feature.nb=feature.nb)
 testthat::expect_true(bed.to.matrix.list[[1]][2]$ngs.bed==1)
 })

## ###### ---------------------

 test_that("test_mapFeaturesToCRMs_small_features", {

 positive.bed='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small_features/positive_2seqs.bed'
 negative.bed='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small_features/negative_5seqs.bed'
 pssm='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small_features/mfile4.tf'

 feature.ranking.file='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small_features/feature_rank.txt'
 feature.ranking=read.table(file=feature.ranking.file, header=T)

 feature.nb=2

 feature.matrix.bak='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small_features/outdir_bak/feature_matrix.tab'

 bed.to.matrix.list <- mapFeaturesToCRMs(URL=URL, positive.bed=positive.bed, genome='dm3', negative.bed=negative.bed, pssm=pssm, background.freqs=background.freqs, feature.ranking=feature.ranking, feature.nb=feature.nb)
 testthat::expect_true(bed.to.matrix.list[[1]][1]$dTCF==0.00988954900424961)
 })

 #### ---------------------

 test_that("test_my_values", {

 positive.bed='mapFeaturesToCRMs_test_data/test_my_values/CRM_13-16.bed'
 negative.bed='mapFeaturesToCRMs_test_data/test_my_values/negative_CRM_13-16.bed'
 #my.values=list.files(path = "test_my_values/my_value_beds", pattern = "*.bed", full.names=T)
 my.values=list("mapFeaturesToCRMs_test_data/test_my_values/my_value_beds/my_values2.bed", "mapFeaturesToCRMs_test_data/test_my_values/my_value_beds/my_values.bed")

 feature.matrix.bak='mapFeaturesToCRMs_test_data/test_my_values/outdir_bak/feature_matrix.tab'

 bed.to.matrix.list <- mapFeaturesToCRMs(URL=URL, positive.bed=positive.bed, genome='dm3', negative.bed=negative.bed, my.values=my.values)
 feature.matrix<-bed.to.matrix.list[[1]]
 testthat::expect_true(bed.to.matrix.list[[1]][1]$my_values2.bed==0.111)
 })

#### ---------------------

test_that("test_ngs_wig", {

positive.bed='mapFeaturesToCRMs_test_data/test_ngs_wig/CRM_13-16.bed'
negative.bed='mapFeaturesToCRMs_test_data/test_ngs_wig/negative_CRM_13-16.bed'
ngs=list.files(path = "mapFeaturesToCRMs_test_data/test_ngs_wig/ngs_wigs", pattern = "*.wig", full.names=T)

feature.matrix.bak='mapFeaturesToCRMs_test_data/test_ngs_wig/outdir_bak/feature_matrix.tab'

bed.to.matrix.list <- mapFeaturesToCRMs(URL=URL, positive.bed=positive.bed, genome='dm3', negative.bed=negative.bed, ngs=ngs)
feature.matrix<-bed.to.matrix.list[[1]]
testthat::expect_true(bed.to.matrix.list[[1]][2]$input2_50_scaled.wig==0.015203926)
})

#### ---------------------

test_that("test_ngs_bed_small", {

positive.bed='mapFeaturesToCRMs_test_data/test_ngs_bed_small/positive_2seqs.bed'
negative.bed='mapFeaturesToCRMs_test_data/test_ngs_bed_small/negative_5seqs.bed'
#ngs=list.files(path = "test_ngs_bed_small/ngs_beds", pattern = "*.bed", full.names=T)
ngs=list("mapFeaturesToCRMs_test_data/test_ngs_bed_small/ngs_beds/ngs2.bed", "mapFeaturesToCRMs_test_data/test_ngs_bed_small/ngs_beds/ngs.bed")

feature.matrix.bak='mapFeaturesToCRMs_test_data/test_ngs_bed_small/outdir_bak/feature_matrix.tab'

bed.to.matrix.list <- mapFeaturesToCRMs(URL=URL, positive.bed=positive.bed, genome='dm3', negative.bed=negative.bed, ngs=ngs)
feature.matrix<-bed.to.matrix.list[[1]]
testthat::expect_true(bed.to.matrix.list[[1]][2]$ngs.bed==1)
})


###### ---------------------

test_that("test_small_shuffling", {

positive.bed='mapFeaturesToCRMs_test_data/test_small_shuffling/positive_2seqs.bed'
shuffling=3
pssm='mapFeaturesToCRMs_test_data/test_small_shuffling/mfile2.tf'

feature.matrix.bak='mapFeaturesToCRMs_test_data/test_small_shuffling/outdir_bak/feature_matrix.tab'

bed.to.matrix.list <- mapFeaturesToCRMs(URL=URL, positive.bed=positive.bed, genome='dm3', shuffling=shuffling, genome.info=genome.info, background.seqs=background.seqs, pssm=pssm, background.freqs=background.freqs)
feature.matrix<-bed.to.matrix.list[[1]]
testthat::expect_true(bed.to.matrix.list[[1]][8]$dTCF==0.00879384104653186)
})

##### ---------------------

test_that("test_mapFeaturesToCRMs_small", {

positive.bed='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small/positive_2seqs.bed'
negative.bed='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small/negative_5seqs.bed'
pssm='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small/mfile2.tf'

feature.matrix.bak='mapFeaturesToCRMs_test_data/test_mapFeaturesToCRMs_small/outdir_bak/feature_matrix.tab'

bed.to.matrix.list <- mapFeaturesToCRMs(URL=URL, positive.bed=positive.bed, genome='dm3', negative.bed=negative.bed, pssm=pssm, background.freqs=background.freqs)
feature.matrix <- bed.to.matrix.list[[1]]
testthat::expect_true(bed.to.matrix.list[[1]][1]$dTCF==0.00988954900424961)
})

