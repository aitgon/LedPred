createModel = function(x, y, feature.ranking = NULL, feature.nb = NULL, file.prefix = NULL, ...) {

#selected.features = as.character(feature.ranking$FeatureName[1:feature.nb])
#x = x[,selected.features]
model.obj <- ledpred2::Model$new(x = x, y = y, file.prefix=file.prefix, feature.ranking = feature.ranking, feature.nb = feature.nb)
#save(model.obj, file = paste(file.prefix,"_model.rda",sep = "")) # can save model for later use

return(model.obj)
}
