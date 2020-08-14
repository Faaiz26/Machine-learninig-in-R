#importing data set
movie <- read.csv("Movie_regression.csv")
View(movie)

summary(movie)
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken,na.rm=TRUE)
is.na(movie$Time_taken)


# splitting  data into test and train using library caTools
set.seed(0)
split_1 <- sample.split(movie,SplitRatio = 0.8)
train_movie <- subset(movie, split_1 == TRUE)
test_movie = subset(movie, split_1 == FALSE)

#for regression tree we need "rpart" and "rpart.plot" packagews
library(rpart)
regtree <- rpart(formula =Collection~., data = train_movie, control = rpart.control(maxdepth = 3))
#Now plotting the regression tree
rpart.plot(regtree,box.palette = "RdBu",digit=-3)

#prediction
test_movie$pred <- predict(regtree,test_movie,type = "vector")
MSME2 <- mean((test_movie$pred - test_movie$Collection)^2)
