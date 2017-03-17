existing_product_attributes <- read.csv(paste("https://github.com/pvpgit/Predicting-Sales-Volume/tree/master/existing_product_attributes.csv", sep=""), header = TRUE)
install.packages("e1071")
library("e1071")
existing_product_attributes$"X.Product.Type." <- NULL
existing_product_attributes$"X.Best.Sellers.Rank." <- NULL
existing_product_attributes$"X.5.Star.Reviews." <- NULL
str(existing_product_attributes)
trainsize<-round(nrow(existing_product_attributes)*0.7)
testsize<-nrow(existing_product_attributes) - trainsize
trainsize
testsize
set.seed((123))
training_indices<-sample(seq_len(nrow(existing_product_attributes)),size=trainsize)
trainset<-existing_product_attributes[training_indices,]
testset<-existing_product_attributes[-training_indices,]
training_indices

##1st model kernel=polynomial
modelsvmpoly<-svm(Volume ~ ., existing_product_attributes, type="C-classification",
          ranges=list(cost=10^(-1:2),gamma=c(0.5,1,2)), kernel="polynomial")
modelsvmpoly
tunepoly<-tune(svm,Volume ~ ., data=testset, 
                 ranges =list(cost=10^(-1:2),gamma=c(.5,1,2)))
tunepoly

##2nd model kernel=radial
##modelsvmradial<-svm(Volume ~ ., existing_product_attributes, type="C-classification",
##                  ranges=list(cost=10^(-1:2),gamma=c(0.5,1,2)), kernel="radial")
##modelsvmradial
##tuneradial<-tune(svm,Volume ~ ., data=testset, 
##               ranges =list(cost=10^(-1:2),gamma=c(.5,1,2)))
##tuneradial


svm_model_after_tunepoly <- svm(Volume ~ ., existing_product_attributes,
                kernel="polynomial", cost=1, gamma=0.5)
summary(svm_model_after_tunepoly)

##svm_model_after_tuneradial <- svm(Volume ~ ., existing_product_attributes,
##                                kernel="radial", cost=1, gamma=0.5)
##summary(svm_model_after_tuneradial)

##Preprocess the new data
new_product_attributes <- read.csv(paste("https://github.com/pvpgit/Predicting-Sales-Volume/tree/master/new_product_attributes.csv", 
sep=""), header = TRUE)
new_product_attributes$"X.Product.Type." <- NULL
new_product_attributes$"X.Best.Sellers.Rank." <- NULL
new_product_attributes$"X.5.Star.Reviews." <- NULL

predpoly<-predict(svm_model_after_tunepoly,new_product_attributes)
system.time(predict(svm_model_after_tunepoly,new_product_attributes))
predpoly

##predradial<-predict(svm_model_after_tuneradial,new_product_attributes)
##system.time(predict(svm_model_after_tuneradial,new_product_attributes))
##predradial


