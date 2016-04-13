createModel = function(data, cl=1, feature.ranking = NULL, feature.nb = NULL, file.prefix = NULL, cost=NULL, ...) {

x=data[,-cl]
y=data[,cl]

#selected.features = as.character(feature.ranking$FeatureName[1:feature.nb])
#x = x[,selected.features]
model.obj <- Model$new(x = x, y = y, file.prefix=file.prefix, feature.ranking = feature.ranking, feature.nb = feature.nb, cost=cost)
#save(model.obj, file = paste(file.prefix,"_model.rda",sep = "")) # can save model for later use

return(model.obj)
}
