##Read the existing product attributes file
existing_product_attributes <- read.csv(paste("https://github.com/pvpgit/Predicting-Sales-Volume/tree/master/existing_product_attributes.csv", sep=""), header = TRUE)
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

##Create the linear regression model
model<-lm(formula=trainset$Volume~., data=trainset)
summary(model)

##Make predictions
predictions<-predict(model,testset,interval="predict", level=0.95)
head(predictions)

## To see acutal and predicted values side by side
comparision<-cbind(testset$Volume,predictions[,1])
colnames(comparision)<-c("actual","predicted")
head(comparision)
summary(comparision)

##Calculate mean absolute percent error
mape<-(sum(abs(comparision[,1]-comparision[,2])/abs(comparision[,1]))/nrow(comparision)) * 100
mape

## To see in table format
mapeTable<-cbind(comparision,abs(comparision[,1]-comparision[,2])/comparision[,1]*100)
colnames(mapeTable)[3]<-"absolute percent error"
head(mapeTable)
sum(mapeTable[,3])/nrow(comparision)

##New data
new_product_attributes <- read.csv(paste("https://github.com/pvpgit/Predicting-Sales-Volume/tree/master/new_product_attributes.csv", sep=""), header = TRUE)
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
newPrediction<-cbind(new_product_attributes, pred= predict(model, newdata=new_product_attributes))
newPrediction

#Create a predictions.csv file on desktop
write.csv(newPrediction, "~/Desktop/prediction_lm.csv", row.names = F)

##Correlation
var10=existing_product_attributes$"'Profit margin'"
var10=existing_product_attributes$"X.Profit.margin."
cor(x="X.5.Star.Reviews.", y=var10, method="spearman")
str(existing_product_attributes)
cor(x="Price", y=var10, method="spearman")
cor(x=Price, y=var10, method="spearman")
cor(x="X.Product.Height.", y=var10, method="spearman")
var11=existing_product_attributes$"X.Product.Height."
cor(x=var11, y=var10, method="spearman")
var1=existing_product_attributes$X.5.Star.Reviews.
var2=existing_product_attributes$Volume
cor(x=var2, y=var1, method="spearman")




