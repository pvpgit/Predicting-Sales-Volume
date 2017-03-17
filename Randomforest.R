install.packages("randomForest")
library(randomForest)

##Read the existing product attributes file
existing_product_attributes <- read.csv(paste("https://github.com/pvpgit/Predicting-Sales-Volume/existing product attributes.csv", sep=""), header = TRUE)
str(existing_product_attributes)

existing_product_attributes$"X.Product.Type." <- NULL
existing_product_attributes$"X.Best.Sellers.Rank." <- NULL
existing_product_attributes$"X.5.Star.Reviews." <- NULL
existing_product_attributes$"X.4.Star.Reviews." <- NULL
existing_product_attributes$"X.Product.Width." <- NULL
existing_product_attributes$"X.Product.Height." <- NULL
existing_product_attributes$"X.Product.Depth." <- NULL
existing_product_attributes$"X.Would.consumer.recommend.product." <- NULL
str(existing_product_attributes)

## Split the data one for training the model and other for testing the model
trainsize<-round(nrow(existing_product_attributes)*0.7)
testsize<-nrow(existing_product_attributes) - trainsize
trainsize
testsize

## set the seed to random value so we will have the same random training set
set.seed(123)
training_indices<-sample(seq_len(nrow(existing_product_attributes)),size=trainsize)
training_indices
trainset<-existing_product_attributes[training_indices,]
testset<-existing_product_attributes[-training_indices,]
trainset
testset

##Create the random forest model
model_random<-randomForest(formula=trainset$Volume~., data=trainset,ntree=5)
summary(model_random)
plot(model_random)
importance(model_random)

##Make predictions
predictions<-predict(model_random,testset,interval="predict", level=0.95)
head(predictions)
table(testset[,5],predictions)
mean(testset[,5]==predictions)

##New data
new_product_attributes <- read.csv(paste("https://github.com/pvpgit/Predicting-Sales-Volume/new product attributes.csv", sep=""), header = TRUE)
str(new_product_attributes)
new_product_attributes$"X.5.Star.Reviews." <- NULL
new_product_attributes$"X.Product.Type." <- NULL
new_product_attributes$"X.Best.Sellers.Rank." <- NULL
new_product_attributes$"X.4.Star.Reviews." <- NULL
new_product_attributes$"X.Product.Width." <- NULL
new_product_attributes$"X.Product.Height." <- NULL
new_product_attributes$"X.Product.Depth." <- NULL
new_product_attributes$"X.Would.consumer.recommend.product." <- NULL
str(new_product_attributes)

##Make new predictions
pred= predict(model_random, newdata=new_product_attributes)
newPrediction<-cbind(new_product_attributes, pred)
newPrediction

#Create a predictions.csv file on desktop
write.csv(newPrediction, "~/Desktop/predictionsRandForest.csv", row.names = F)


##table(new_product_attributes[,"Volume"],pred)
##mean(new_product_attributes[,"Volume"]==pred)
##s_d=sample(56,24)
##?sample
##train_data=existing_product_attributes[s_d,]
##dim(train_data)
##test_data=existing_product_attributes[-s_d,]
##dim(existing_product_attributes)
##s_d=sample(56,24)
##?sample
##train_data=existing_product_attributes[s_d,]
##dim(train_data)
##test_data=existing_product_attributes[-s_d,]
##rf_model=randomForest(Volume~.,train_data,ntree=500)
##model_random<-randomForest(formula=trainset$Volume~., data=trainset)
##rf_model
##?randomForest
##p_model=predict(rf_model,test_data)
##p_model
##table(test_data[,"Volume"],p_model)
##mean(test_data[,"Volume"]==p_model)



##3/8/2017
##Questions:
## Whether to do classification or regression?

##model<-randomForest(formula=trainset$Volume~., data=trainset, type="classification")
##model
##plot(model)
##testPrediction<-predict(model,newdata=new_product_attributes)
##table(testPrediction,new_product_attributes$Volume)
##newPrediction<-predict(model,newdata=new_product_attributes)
##newPrediction
