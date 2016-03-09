### R code from vignette source 'LedPred.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: LedPred.Rnw:42-44
###################################################
library(LedPred)
data(crm.features)


###################################################
### code chunk number 2: LedPred.Rnw:128-145 (eval = FALSE)
###################################################
##  dirPath <- system.file("extdata", package="LedPred")
##  file.list <-   list.files(dirPath, full.names=TRUE)
##  background.freq <- file.list[grep("freq", file.list)]
##  positive.regions <-  file.list[grep("positive", file.list)]
##  negative.regions <-  file.list[grep("negative", file.list)]
##  TF.matrices <-  file.list[grep("tf", file.list)]
##  ngs.path <- system.file("extdata/ngs", package="LedPred")
##  ngs.files=list.files(ngs.path, full.names=TRUE)
##  crm.features.list <- mapFeaturesToCRMs(URL = 'http://ifbprod.aitorgonzalezlab.org/map_features_to_crms.php', positive.bed=positive.regions, 
##      negative.bed=negative.regions,  background.freqs=background.freq, 
##      pssm=TF.matrices, genome="dm3", ngs=ngs.files, 
##      crm.feature.file = "crm.features.tab", 
##      stderr.log.file = "stderr.log", stdout.log.file = "stdout.log")
##  names(crm.features.list)
##  class(crm.features.list$crm.features)
##  crm.features.list$stdout.log
##  crm.features.list$stderr.log


###################################################
### code chunk number 3: LedPred.Rnw:152-159
###################################################
library(GenomicRanges)
vals=mcols(crm.features)
cl=vals[,1]
vals=vals[,-1]
vals_mod=as.data.frame(apply(as.data.frame(vals), 2, function(x) x/sqrt(sum(x^2))))
vals_mod=cbind(cl, vals_mod)
mcols(crm.features)=vals_mod


###################################################
### code chunk number 4: LedPred.Rnw:167-174
###################################################
cost.vector <- c(1,3,10,30)
gamma.vector <- c(1,3,10,30)
c.g.obj <- mcTune(data.granges= crm.features, ranges = list(cost=cost.vector, 
    gamma=gamma.vector), kernel='linear', file.prefix = "test")
names(c.g.obj)
 cost <- c.g.obj$best.parameters$cost
 gamma <- c.g.obj$best.parameters$gamma


###################################################
### code chunk number 5: LedPred.Rnw:186-188 (eval = FALSE)
###################################################
##  feature.ranking <- rankFeatures(data.granges =crm.features, cost=cost,gamma=gamma, 
##      kernel='linear', file.prefix = "test")


###################################################
### code chunk number 6: LedPred.Rnw:194-198 (eval = FALSE)
###################################################
##  feature.nb.obj <- tuneFeatureNb(data.granges=crm.features, 
##      feature.ranking=feature.ranking, kernel='linear', cost=cost,gamma=gamma, 
##      file.prefix = "test")
##  names(feature.nb.obj)


###################################################
### code chunk number 7: LedPred.Rnw:210-213
###################################################
feature.nb <- 50
svm.model <- createModel(data.granges=crm.features, cost=cost, gamma=gamma, 
    feature.ranking=feature.ranking, feature.nb=feature.nb)


###################################################
### code chunk number 8: LedPred.Rnw:218-219
###################################################
feature.weights <- as.data.frame(t(t(svm.model$coefs) %*% svm.model$SV))


###################################################
### code chunk number 9: LedPred.Rnw:225-229
###################################################
probs.labels.list <- evaluateModelPerformance(data.granges=crm.features, 
    feature.ranking=feature.ranking, feature.nb=50, 
    file.prefix = "test")
names(probs.labels.list[[1]])


###################################################
### code chunk number 10: mapFeaturesToCRMs2
###################################################
dirPath <- system.file("extdata", package="LedPred")
file.list <-   list.files(dirPath, full.names=TRUE)
background.freqs <- file.list[grep("freq", file.list)]
positive.bed <-  file.list[grep("prediction_small", file.list)]
TF.matrices <-  file.list[grep("259_matrices_lightNames", file.list)]
ngs.path <- system.file("extdata/ngs", package="LedPred")
ngs.files=list.files(ngs.path, full.names=TRUE)
feature.nb=50

prediction.crm.features.list <- mapFeaturesToCRMs(URL = 'http://ifbprod.aitorgonzalezlab.org/map_features_to_crms.php', positive.bed=positive.bed, 
  pssm=TF.matrices, background.freqs=background.freqs, 
  genome='dm3', ngs=ngs.files, feature.ranking=feature.ranking, feature.nb=50, 
    crm.feature.file = "test_pred.crm.features.tab", 
    stderr.log.file = "test_pred.stderr.log", stdout.log.file = "test_pred.stdout.log")
names(prediction.crm.features.list)
prediction.crm.features <- prediction.crm.features.list$crm.features


###################################################
### code chunk number 11: LedPred.Rnw:263-265
###################################################
pred.test <- scoreData(data.granges=prediction.crm.features, model=svm.model, 
  score.file="test_prediction.tab")


###################################################
### code chunk number 12: LedPred.Rnw:272-278 (eval = FALSE)
###################################################
##  data(crm.features)
##  cost_vector <- c(1,3,10)
##  gamma_vector <- c(1,3,10)
##  ledpred.list=LedPred(data.granges=crm.features, cl=1, ranges = list(cost=cost_vector, 
##                            gamma=gamma_vector), kernel="linear", halve.above=50)
##  names(ledpred.list)


###################################################
### code chunk number 13: LedPred.Rnw:285-286
###################################################
sessionInfo()


