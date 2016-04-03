# 160403

- Rename LedPred class to LedPredClass and ledpred wrapper to LedPred to be consistent with previous versions
- scale.factor: modulo one. dat_mod=as.data.frame(apply(dat, 2, function(x) x/sqrt(sum(x^2))));
- y=as.factor(y) is not working for feature.ranking

