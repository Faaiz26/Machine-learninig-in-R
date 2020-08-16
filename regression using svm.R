#regression using svm
#loadinig data
data_reg <- read.csv("Movie_regression.csv",header = TRUE)
#looking summary for data frequency
summary(data_reg)
#replacing missing values
data_reg$Time_taken[is.na(data_reg$Time_taken)] <- mean((data_reg$Time_taken) , na.rm=TRUE)

#splitting data into train and test set
#to split datapackage needed "caTools"
library(caTools)
set.seed(0)
split_reg <- sample.split(data_reg,SplitRatio =0.8)
train_reg <- subset(data_reg,split_reg == TRUE)
test_reg <- subset(data_reg,split_reg == FALSE)
#now for running support vector model we needed package "e1071"
library(e1071)
#running linear kernal regression
regfit <- svm(Collection~., data = train_reg , kernel = "linear", cost =0.01, scale =TRUE)
summary(regfit)
#prediction
reg_predLinear <- predict(regfit,test_reg)
reg_predLinear
#finding mean square value
MSEregLinear <- mean((reg_predLinear - test_reg$Collection)^2)
MSEregLinear

#RUNNING Polynomial kernal
regfit_poly <- svm(Collection~., data = train_reg , kernal = "Polynomial",cost = 1 , degree = 2)
summary(regfit_poly)
#prediction 
reg_predPoly <- predict(regfit_poly,test_reg)
reg_predPoly
MSEregPoly <- mean((reg_predPoly - test_reg$Collection)^2)

#running radial kernal
regfit_rad <- svm(Collection~., data = train_reg , kernal="radial",cost = 1, gamma = 1)
summary(regfit_rad)
#prediction 
reg_predRad <- predict(regfit_rad , test_reg)
reg_predRad
MSEregRad <- mean((reg_predRad - test_reg$Collection)^2)
