#install.packages("stat")
#install.packages("klaR", lib = "Rpackages")
#library(stat)
#install.packages("logForest", lib = "Rpackages")
install.packages("logitBoost")
library(caret)
library(mlbench)
#library(randomForest)
#library(naiveBayes)
#library(klaR)
#library(logForest) # depricated on R v3.0.1 
libray(logitBoost)

# Preprocess data
bf<-read.csv("bank-full.csv")			#dataset available here: http://archive.ics.uci.edu/ml/machine-learning-databases/00222/
bft<-read.csv("bank.csv")				#same source as above
labels<-bf$y

bf$marital<-as.factor(bf$marital)


# Train/Test Split
set.seed(321) 
inTraining <- createDataPartition(bf$y, p = 0.75, list = FALSE) # splitting data into 2 groups using CreateDataPartion()
training<-bf[inTraining, ] #caret training split
testing<-bf[-inTraining, ] #caret testing split

# Basic Parameter Tuning
fitControl<-trainControl(method = "repeatedcv", #10-Fold CV 
						number = 10, 
						repeats =10)

Fit1<-train(y ~ ., data = training,
						 method = 'logitBoost',
						 trControl = fitControl,
						 verbose = FALSE)

# Extracting Predictions and Classifications
predict(Fit1, head(testing), type = "prob")



	
