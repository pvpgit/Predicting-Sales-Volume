##library(rJava)
##.jinit()
##.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
install.packages("RWeka")
library(RWeka)
existing_product_attributes <- read.csv(paste("https://github.com/pvpgit/Predicting-Sales-Volume/tree/master/existing_product_attributes.csv",
                                              sep=""), header = TRUE)
                             
existing_product_attributes$Volume<-as.factor(existing_product_attributes$Volume)
SMOFit <-SMO(existing_product_attributes$Volume ~ ., existing_product_attributes,
    control = Weka_control(K = list("weka.classifiers.functions.supportVector.RBFKernel", G = .5)),options=list(c=1))
SMOFit
                               
existing_product_attributes$"X.Product.Type." <- NULL
existing_product_attributes$"X.Best.Sellers.Rank." <- NULL
existing_product_attributes$"X.5.Star.Reviews." <- NULL
existing_product_attributes$"X.4.Star.Reviews." <- NULL
existing_product_attributes$"X.Product.Width." <- NULL
existing_product_attributes$"X.Product.Height." <- NULL
existing_product_attributes$"X.Product.Depth." <- NULL
existing_product_attributes$"X.Would.consumer.recommend.product." <- NULL
                               
                               ##str(existing_product_attributes
##classifier <-SMOreg(existing_product_attributes$Volume ~ ., data=existing_product_attributes, control=Weka_control(K=20, X=TRUE))
##evaluate_Weka_classifier(classifier, numFolds = 10)
##classifier
SMOFit <-SMO(existing_product_attributes$Volume ~ ., existing_product_attributes,
    control = Weka_control(K = list("weka.classifiers.functions.supportVector.RBFKernel", G = .5)),options=list(c=1))
SMOFit
                               
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
                               
##Make predictions
predictions<-predict(SMOFit,testset,interval="predict", level=0.95)
head(predictions)
                              
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
newPrediction<-cbind(new_product_attributes, pred= predict(SMOFit, newdata=new_product_attributes))
newPrediction
                               
#Create a predictions.csv file on desktop
write.csv(newPrediction, "~/Desktop/predictionsSMO.csv", row.names = F)
