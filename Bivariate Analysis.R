# Bivariate Analysis
df =  read.csv("House_Price.csv",header=TRUE)
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
