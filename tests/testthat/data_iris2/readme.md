~~~
set.seed(123)
iris2 = data.frame(lapply(1:4,function(i) {
  sample(iris[,i])
}))
set.seed(321)
iris22 = data.frame(lapply(1:4,function(i) {
  sample(iris[,i])
}))
colnames(iris2) = list('x1', 'x2', 'x3', 'x4')
colnames(iris22) = list('x5', 'x6', 'x7', 'x8')
iris2 = cbind(iris,iris2,iris22)

y=as.numeric(iris2$Species=='setosa')
y[y==0]=-1
x=iris2[, names(iris2)!='Species']


save(x, file='x.rda')
save(y, file='y.rda')
~~~
