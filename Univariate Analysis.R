df <- read.csv("House_Price.csv",header=TRUE)
str(df)
summary(df)
hist(df$crime_rate)
pairs(~price + crime_rate + n_hot_rooms + rainfall,data=df)
barplot(table(df$airport))
# observations
#n_hot_rooms and rainfall contain outliers
#n_hos_bed contain missing values
#bus terminals is useless variable
#crime rate has some functional relationship with 
# calculating outliers
quantile(df$n_hot_rooms,0.99)
# assining Upeer value
uv <- 3*quantile(df$n_hot_rooms,0.99)
df$n_hot_rooms[df$n_hot_rooms > uv] <- uv
summary(df$n_hot_rooms)
# assining lower values
lv = 0.3*quantile(df$rainfall,0.01)
df$rainfall[df$rainfall>lv] <- lv
summary(df$rainfall)
# Dealing with Missing values
mean(df$n_hos_beds)
mean(df$n_hos_beds,na.rm=TRUE)

# to know cell number of missing values
which(is.na(df$n_hos_beds))

# replacing missing values with mean

df$n_hos_beds[is.na(df$n_hos_beds)]<- mean(df$n_hos_beds,na.rm=TRUE)
summary(df$n_hos_beds)




# Bivariate Analysis

pairs(~price+crime_rate,data=df)
#or
plot(df$price,df$crime_rate)

#transforming variable
df$crime_rate = log(1+df$crime_rate)
plot(df$price,df$crime_rate)
# creating average distance value

df$avg_dist = (df$dist1+df$dist2+df$dist3+df$dist4)/4
df2 <-  df[,-7:-10]
df<- df2
df<- df[,-14]


# creating dummy variable
install.packages("dummies")
df <- dummy.data.frame(df)
df <-df[,-9]
df <-df[,-14]
df
View(df)


# correlation in r
cor(df)

round(cor(df),2)
df <- df[,-16]
View(df)


# linear regression
simple_model1 <- lm(price~room_num,data=df)
summary(simple_model1)
plot(df$room_num,df$price)
abline(simple_model1)


# Multiple linear regression
multiple_model <- lm(price~.,data=df)
summary(multiple_model)






# Test train
install.packages("caTools")
library(caTools)
set.seed(0)
split = sample.split(df,SplitRatio=0.8)
training_set=subset(df,split==TRUE)
test_set=subset(df,split==FALSE)

# running linear model on testing set
lm_a = lm(price~.,data=training_set)

# prediction with known values
train_a = predict(lm_a,training_set)
# predictioon on test set
test_a = predict(lm_a,test_set)
#mean r squarwe vale of train set
mean((training_set$price-train_a)^2)
#mean r squared value of test set
mean((test_set$price-test_a)^2)




# Subset selection method (using leaps package)
#running best subset model
lm_best = regsubsets(price~.,data=df,nvmax = 15)
summary(lm_best)
# selecting highly adjusted R^2 value
summary(lm_best)$adjr2
which.max(summary(lm_best)$adjr2)
#viewing highest r square value
coef(lm_best,8)

#running forward step wise selectioin
lm_forward = regsubsets(price~.,data=df,nvmax=15,method="forward")
summary(lm_forward)
#selecting best r^2 valuer
summary(lm_forward)$adjr2
which.max(summary(lm_forward)$adjr2)
coef(lm_forward,8)


#running backward step wise selection model
lm_backward = regsubsets(price~.,data=df,nvmax=15,method="backward")
summary(lm_backward)
summary(lm_backward)$adjr2
which.max(summary(lm_backward)$adjr2)
coef(lm_backward,8)



# Ridge regression library needed "glmnet"

x <- model.matrix(price~.,data=df)[,-1]   #independent variable
y <- df$price  #dependent variable
grid <- 10^seq(10,-2,length = 100)
lm_ridge = glmnet(x,y,alpha=0,lambda = grid)

#finding best lambda
cv_fit = cv.glmnet(x,y,alpha=0,lambda = grid)
plot(cv_fit)


#optimum value of lambda
op_lambda <- cv_fit$lambda.min
#finding tss
tss <- sum((y-mean(y))^2)

#finding predicted value of y
y_a <- predict(lm_ridge,s=op_lambda,newx=x)
#finding residual sum of square(rss)
rss = sum((y_a-y)^2)

#finding r square value
rsq <- 1-rss/tss


#running lasso model
lm_lasso <- glmnet(x,y,alpha=1,lambda =grid)
#finding best lambda
cv_fit1 <-cv.glmnet(x,y,alpha=1,lambda = grid)
plot(cv_fit1)
# finding optimum value of lambda

opt_lambda = cv_fit1$lambda.min 

#tss value wil be same as find in ridge

# predficted value of y 
y_b <- predict(lm_lasso,s=opt_lambda,newx=x)

#rss value 
rss1<- sum((y_b-y)^2)

# finding r swquare value
rsq1 <- 1-rss1/tss
