#classification tree
movie_data <- read.csv("Movie_classification.csv")
View(movie_data)
#data preprocessing
summary(movie_data)
movie_data$Time_taken[is.na(movie_data$Time_taken)] <- mean(movie_data$Time_taken,na.rm = TRUE)

#Test Train split package needed catools
library(caTools)
set.seed(0)
split_movie_data <- sample.split(movie_data,SplitRatio = 0.8)
train_movie_data <- subset(movie_data,split_movie_data == TRUE)
test_movie_data <- subset(movie_data,split_movie_data == FALSE)

#running classification tree 
classtree <- rpart(formula = Start_Tech_Oscar~., data = train_movie_data, method = "class",control = rpart.control(maxdepth = 3) )
rpart.plot(classtree,box.palette = "RdBu",digits = -3)
test_movie_data$pred <- predict(classtree,test_movie_data,type='class')
table(test_movie_data$Start_Tech_Oscar,test_movie_data)


#bagging (package required = "randomForest")
set.seed(0)
bagging <- randomForest(Collection~., data = train_movie, mtry = 17)
test_movie$bagging <- predict(bagging , test_movie)
MSE2bagging <- mean((test_movie$bagging - test_movie$Collection)^2)

#random forest
randomfor <- randomForest(Collection~., data = train_movie , ntree = 500)

# prediction
test_movie$random <- predict(randomfor , test_movie)
MSE2random <- mean((test_movie$random - test_movie$Collection)^2)

#boosting package nedded "gbm"
install.packages("gbm")
library(gbm)
boosting <- gbm(Collection~., data =train_movie, distribution = "gaussian",n.trees = 6000,interaction.depth = 4,shrinkage = 0.2,verbose = F)
#prediction
test_movie$boost <- predict(boosting,test_movie,n.trees=6000)
MSE2bost <- mean((test_movie$boost - test_movie$Collection)^2)

#adda boosting
#package needed("adabag")
install.packages("adabag")
library(adabag)
train_movie_data$Start_Tech_Oscar <- as.factor(train_movie_data$Start_Tech_Oscar)
adaboost <- boosting(Start_Tech_Oscar~., data=train_movie_data,boos=TRUE,mfinal = 1000)
predada <- predict(adaboost,test_movie_data)
#making confusion matrix
table(predada$class,test_movie_data$Start_Tech_Oscar)
65/107
t1 <- adaboost$trees[[1]]
plot(t1)
text(t1,pretty = 100)

#XGBOOST package needed ("xgboost")
install.packages("xgboost")
library(xgboost)
trainy1 <- train_movie_data$Start_Tech_Oscar == "1"
trainx1 <- model.matrix(Start_Tech_Oscar~. -1, data = train_movie_data)
trainx1 <- trainx1[,-12] #to delete extra varialble

testy1 <- test_movie_data$Start_Tech_Oscar == "1"
testx1 <- model.matrix(Start_Tech_Oscar~. -1, data = test_movie_data)
testx1 <- testx1[,-12]

#since xgboost takes only input of as d matrix so we need to convert data in to d  matrix
Xmatrix <- xgb.DMatrix(data = trainx1 , label = trainy1)
Xmatrix_t <- xgb.DMatrix(data = testx1 , label = testy1)

#now performi9ng xgb boosting
xgb_boosting <- xgboost(data = Xmatrix, nrounds = 50, objective = "multi:softmax", eta = 0.3,num_class = 2, max_depth = 100)
# prediction
xgb_prediction <- predict(xgb_boosting,Xmatrix_t)
