#install.packages("stat")
#library(stat)

# Preprocess data
bf<-read.csv("bank-full.csv")			#dataset available here: http://archive.ics.uci.edu/ml/machine-learning-databases/00222/
bft<-read.csv("bank.csv")				#same source as above
labels<-bf[17]


#bf[,17]<-NULL	#removing y variable, the term deposit outcome
bf.t<-bft[,17]  #removing all variables save for y, the term deposit classification
bf$marital<-as.factor(bf$marital)
#bf$balance<-as.factor(bf$balance)

# Train/Test Split
set.seed(321) 
train.pct<-.8 
N<-nrow(bf) 		#determine data length

train.index<-sample(1:N, train.pct*N) #set training set to 80%

# Initialize results object
err.rates<-data.frame()
err.rates2<-data.frame()		
err.rates3<-data.frame()
err.rates4<-data.frame()
err.rates5<-data.frame()
err.rates6<-data.frame()


# Loop for 5-fold cross validation
log.fit<-
	for (i in 1:5){
		yval<-i
	
		train.index <- sample(1:N, train.pct * N) 	#create a random sample of records from the training set
		train.datam<-bf[train.index,]			#split training data using random indexs derived from sampling	
		test.datam<-bf[-train.index,]		 	#split test data

		train.labels<-as.factor(as.matrix(labels)[train.index,]) #extract training set labels
		test.labels<-as.factor(as.matrix(labels)[-train.index,])  #extract test set labels

		glm1<-glm(train.datam[,17] ~ train.datam$job + train.datam$housing + train.datam$loan + train.datam$marital, family=binomial(logit))
		err.rates<-exp(cbind(OR = coef(glm1), confint(glm1)))
	
		glm2<-glm(train.datam[,17] ~ train.datam$job + train.datam$balance + train.datam$loan + train.datam$marital, family=binomial(logit))
		err.rates2<-exp(cbind(OR = coef(glm2), confint(glm2)))

		glm3<-glm(train.datam[,17] ~ train.datam$job + train.datam$housing:train.datam$balance + train.datam$loan + train.datam$marital, family=binomial(logit))
		err.rates3<-exp(cbind(OR = coef(glm3), confint(glm3)))

		glm4<-glm(train.datam[,17] ~ train.datam$job + train.datam$balance + train.datam$housing:train.datam$loan + train.datam$marital, family=binomial(logit))
		err.rates4<-exp(cbind(OR = coef(glm4), confint(glm4)))

		glm5<-glm(train.datam[,17] ~ train.datam$job + train.datam$balance + train.datam$housing:train.datam$loan + train.datam$housing:train.datam$marital, family=binomial(logit))
		err.rates5<-exp(cbind(OR = coef(glm5), confint(glm5)))

		glm6<-glm(train.datam[,17] ~ train.datam$job + train.datam$housing:train.datam$balance + train.datam$housing:train.datam$loan + train.datam$housing:train.datam$marital, family=binomial(logit))
		err.rates6<-exp(cbind(OR = coef(glm6), confint(glm6)))

		glm7<-glm(train.datam[,17] ~ train.datam$education + train.datam$balance + train.datam$housing:train.datam$loan + train.datam$housing:train.datam$marital, family=binomial(logit))
		err.rates7<-exp(cbind(OR = coef(glm7), confint(glm7)))

	#if(i==1&&err.rates>err.rates2) #| if (err.rates2>=err.rates3) | err.rates3>=err.rates4)
	#	model.select <- err.rates
	#else
	#	model.select<-err.rates2>=err.rates3
	#else
	#	model.select<-err.rates3>=err.rates4
	#else
	#	model.select<-err.rates4>=err.rates

}

#model.a<-anova(glm1, glm2) 
#model.a2<-anova(glm3, glm4)
#model.a3<-anova(glm5, glm6)

#model.e<-rbind(err.rates, err.rates2, err.rates3, err.rates4, err.rates5)
#summary(model.e)


#plot(model.s)
#plot(err.rates, err.rates2)

###########
# RESULTS #
###########

# The GLM models do not perform well here as it is very costly due to all of the covariates and factors therein. 
# Advised to use the Caret package to perform programmatic feature selection/engineering, running into limitations for analysis on 1 box.
# The err.rates are skewed by a handful of covariates predominately factors within the datam$Job Class (eg. student)
# Nevertheless, this model reveals that the best OR is glm 4; however, this model should be reworked in Caret for better accuracy and precision.
   
