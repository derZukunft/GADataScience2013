library(RWeka)
attach(RWeka)
attach(iris)

iris<-read.arff(system.file("arff", "iris.arff", package="RWeka"))
classifier<-IBk(class ~., data = iris,
	control = Weka_control(K=20, X=TRUE))
evaluate_Weka_classifier(classifier, numFolds = 10)
### 10 Fold Cross Validation ###

### Summary ###

summay(classifier)

### Confusion.matrix ###

Confusion

### Plot ###
plot(iris)

### ggplot ###
#library(ggplot)
#iris.plot<-ggplot(iris)





