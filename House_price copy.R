df <- read.csv("House_Price.csv", header = TRUE)

str(df)
summary(df
hist(df$crime_rate)
hist(df$parks)
hist(df$rainfall)
barplot(table(df$airport))
barplot(table(df$bus_ter))
barplot(table(df$waterbody))
pairs(~price+crime_rate+n_hot_rooms+rainfall, data = df)

uv = 3*quantile(df$n_hot_rooms, 0.99)
df$n_hot_rooms[df$n_hot_rooms> uv] <- uv
df$n_hot_rooms

summary(df$n_hot_rooms)

summary(df$rainfall)

lv =   0.3*  quantile(df$rainfall, 0.01)

df$rainfall[df$rainfall<lv] <-lv

summary(df$rainfall)

which(is.na(df$n_hos_beds))


df$n_hos_beds[is.na(df$n_hos_beds)] <- mean(df$n_hos_beds,na.rm = TRUE)
summary(df$n_hos_beds)
which(is.na(df$n_hos_beds))
pairs(~price+crime_rate,data = df)
plot(df$price,df$crime_rate)
df$crime_rate
df$crime_rate=log(1+df$crime_rate)
df$crime_rate
pairs(~price+crime_rate,data = df)
plot(df$price,df$crime_rate)


df$avg_dist = (df$dist1+df$dist2+df$dist3+df$dist4)/4
df2 <- df[,-7:-10]
df <- df2
rm(df2)

df
df <-df[,-14]
library("dummies")
df <- dummy.data.frame(df)
df <- df[,-9]
df <- df[,-14]


simple_model <- lm(price~room_num, data=df)

simple_model

summary(simple_model)

plot(df$room_num,df$price)
abline(simple_model)


#install.packages("caTools")
library("caTools")
set.seed(0)
split = sample.split(df,SplitRatio = 0.8)
training_set = subset(df,split == TRUE)
test_set = subset(df, split == FALSE)
lm_a = lm(price~.,data=training_set)

train_a = predict(lm_a,training_set)
test_a = predict(lm_a,test_set)
mean((training_set$price-train_a)^2)
mean((test_set$price-test_a)^2)
sqrt(mean((test_set$price-test_a)^2))

#install.packages("leaps")
library(leaps)

lm_best = regsubsets(price~.,data = df, nvmax = 15)

summary(lm_best)

summary(lm_best)$adjr2

which.max(summary(lm_best)$adjr2)
coef(lm_best,8)
lm_forward = regsubsets(price~.,data = df, nvmax = 15, method = "forward")
summary(lm_forward)

