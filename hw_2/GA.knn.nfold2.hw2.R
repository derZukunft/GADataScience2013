library(class)

##Preprocessing
data<-iris
labels <-data$Species
data$Species<-NULL

### Trying to remove column names --> getting an error message when trying to 
### run predictions because of NA/NAN values which I assume are the data column headers

#write.table(data, "noname.txt", sep='\t', quote=FALSE, row.names=FALSE, col.names=FALSE)
#data<-read.table("noname.txt", header=FALSE)

###
###


##Train/Test Split
set.seed(123)
train.pct.x<-.4
train.pct.y<-.4

#nn.train<-as.matrix(train.data.x, dimnames=NULL)
#n.train<-nrow(nn.train)
N<-nrow(data)

train.index.x<-sample(1:N, train.pct.x*N)
train.index.y<-sample(1:N, train.pct.y*N)

train.data.x<-data[train.index.x,]
train.data.y<-data[train.index.y,]

test.data.x<-data[-train.index.x,]
test.data.y<-data[-train.index.y,]

train.labels<-as.factor(as.matrix(labels)[train.index.x,])
test.labels<-as.factor(as.matrix(labels)[-train.index.x,])

nn.train<-as.matrix(train.data.x, dimnames=NULL)
n.train<-nrow(nn.train)


#nn.train<-as.matrix(train.data.x, dimnames=NULL)

#n.train<-nrow(nn.train)
#N<-nrow(data)

##Functions for knn
knn<-function(klist, train.data.x, train.data.y, test.data.x)
{

	#Number of training and test examples
	n.train<-nrow(train.data.x)
	n.test<-nrow(test.data.x)


	#Matrix to store predictions
	#p.test <- matrix(NA, n.test, length(klist))
	p.test<-data.frame()

	#Vector to store the distances of a point to the training points
	dsq<-numeric(n.train)

	#Loop on the test instances
	for (tst in 1:n.test)
	{
		#Compute distances to training instances
		for(trn in 1:nrow(train.data.x))
		{
			dsq[trn]<-sum((train.data.x[trn,]-test.data.x[tst,])^2)
		}
		
		#Sort distances from smallest to largest
		ord<-order(dsq)
		
		#Make prediction by averaging the k nearest neighbors
		for (i in 1:length(klist)) {
				p.test[tst, i]<-mean(train.data.y[ord[1:klist[i]],])
		}
	}
}

#Matrix to store predictions
p.cv<-matrix(NaN, n.train, length(klist))
#p.cv<-data.frame() # Initializing results object

knn.cv<-function(klist, train.data.x, train.data.y, nfolds)
{
	#Cross-Validation for kNN
	#
	#Perform nfolds-cross vaildiation of kNN, for the values of k in klist

	#Number of instances
	n.train<-nrow(train.data.x)

	#Matrix to store predictions
	#p.cv<-matrix(NA, n.train, length(klist))
	#p.cv<-data.frame()

	#Prepare the folds
	s<-split(sample(n.train), rep(1:nfolds, length=n.train))

	#Cross-Validation
	for (i in seq(nfolds))
	{
		p.cv[s[[i]],]<-knn(klist, train.data.x[-s[[i]],], train.data.y[-s[[i]],],train.data.x[s[[i]],])
	}

	#Return matrix of CV predictions
	invisible(p.cv)
}



##Make predictions by kNN
klist<-seq(n.train) #testing all values of k
nfolds<-5 # make 5-fold cross-validation
pred.train.y<-knn(p.cv, train.data.x, train.data.y, train.data.x)
#pred.test.y<-knn(klist, train.data.x, train.data.y, train.data.x)
#pred.cv.y<-knn.cv(klist, train.data.x, train.data.y, nfolds)

##Compute mean-square error(MSE)
#mse.train<-apply((pred.train.y-train.data.y)^2, 2, mean)
#mse.test<-apply((pred.test.y-test.data.y)^2, 2, mean)
#mse.cv<-apply((pred.cv.y-train.data.y)^2,2, mean)

##Plot MSE as a function of k
#plot(mse.train , ylim=c(0,2) , type='l' , xlab='k' , ylab='MSE', col=1 , lwd=2)
#lines(mse.test , col=2 , lwd=2)
#lines(mse.cv, col=3 , lwd=2)
#legend("bottomright",legend=c('Train','Test','CV'),text.col=seq(3) , lty=1 , col=seq(3))
