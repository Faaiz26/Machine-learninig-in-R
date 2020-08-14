movie_sv = read.csv("Movie_classification.csv",header = TRUE)
#data processing
#doing only treatment of missing values
summary(movie_sv)
#removing Na's from time takken variable
movie_sv$Time_taken[is.na(movie_sv$Time_taken)] <- mean(movie_sv$Time_taken,na.rm = TRUE)
#creating train and test split
library(caTools)
set.seed(0)
split_sv <- sample.split(movie_sv,SplitRatio = 0.8)
train_sv <- subset(movie_sv,split_sv == TRUE)
test_sv  <- subset(movie_sv,split_sv == FALSE)

#for classification
train_sv$Start_Tech_Oscar <- as.factor(train_sv$Start_Tech_Oscar)
test_sv$Start_Tech_Oscar <- as.factor(test_sv$Start_Tech_Oscar)

#running svm model using linear kernal
#package needed "e1071"
install.packages("e1071")
library(e1071)
svmfit <- svm(Start_Tech_Oscar~., data = train_sv , kernel = "linear",cost = 1, scale = TRUE)
summary(svmfit)
# prediction
svm_prediction <- predict(svmfit,test_sv)
#creating confusion matrix
table(predict = svm_prediction , truth = test_sv$Start_Tech_Oscar)
#to check support vector
svmfit$index


#finding best value of cost / Hyperparameter tunnning for linear kernal
set.seed(0)
tune.out <- tune(svm, Start_Tech_Oscar~., data =  train_sv, kernel = "linear", ranges = list(cost=c(0.001,0.01,1,10,100)))
best_model <- tune.out$best.model
summary(best_model)
#predicting values
best_model_prediction <- predict(best_model,test_sv) 
#creating confusion matrix
table(best_model_prediction,test_sv$Start_Tech_Oscar)

#Running polynomial kernel in svm model
svmfitP <- svm(Start_Tech_Oscar~., data = train_sv , kernel = "polynomial",cost = 1,degree =2)
summary(svmfitP)
#prediction
svmfitp_prediction <- predict(svmfitP,test_sv)
table(svmfitp_prediction,test_sv$Start_Tech_Oscar)
#running hyperparameter tunning for polynomial kernel
tune.outP <- tune(svm,Start_Tech_Oscar~.,data = train_sv , kernel = "ploynomial" , cross = 4 , ranges = list(cost=c(0.001,0.1,1,5,10),degree =c(0.5,1,2,3,5)))
summary(tune.outP)
best_modelP <- tune.outP$best.model
#best.model predictgion
best_modelP_prediction <- predict(best_modelP,test_sv)
#creating confusion matrix
table(best_modelP_prediction,test_sv$Start_Tech_Oscar)


#RADIAL KERNEL SVM MODEL
svmfitR <- svm(Start_Tech_Oscar~., data = train_sv , kernel = "radial", gamma = 1, cost = 1)
summary(svmfitR)
#prediction on radial kernel model
svmfitR_prediction <- predict(svmfitR,test_sv)
table(svmfitR_prediction,test_sv$Start_Tech_Oscar)
#hyperparameter tunning for raddial kernel
tune.outR <- tune(svm,Start_Tech_Oscar~., data = train_sv , kernel = "radial", ranges = list(cost=c(0.001,0.01,100,1000),gamma = c(0.01,0.1,0.5,1,2,3,4,10,50),cross = 4))
summary(tune.outR)
best_modelR <- tune.outR$best.model
summary(best_modelR)#prediction
best_modelR_prediction <- predict(best_modelR,test_sv)
table(best_modelR_prediction,test_sv$Start_Tech_Oscar)
