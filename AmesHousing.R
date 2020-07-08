install.packages("AmesHousing") 
library("AmesHousing") 
install.packages("tree") 
library("tree")

d1 = (as.data.frame(AmesHousing::make_ames()))
str(d1)
d1$Sale_Price
dim(d1)
summary(d1)


library(caTools) 
set.seed(123) 
split =sample.split(d1,SplitRatio = 0.8) 
train = subset(d1,split == TRUE) 
test = subset(d1,split == FALSE) 

#install required packages 
install.packages('rpart') 
install.packages('rpart.plot') 

library(rpart) # performing regression trees 
library(rpart.plot) # plotting regression trees 

#Run regression tree model on train set 
m1 <- rpart(formula = Sale_Price~., data = train)
summary(m1)
m1
m1$cptable

fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis) 
fit2 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, 
              parms = list(prior = c(.65,.35), split = "information")) 
fit3 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, 
              control = rpart.control(cp = 0.05)) 
par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped 
plot(fit) 
text(fit, use.n = TRUE) 
plot(fit2) 
text(fit2, use.n = TRUE)

rpart.plot(m1)
plotcp(regtree)
plotcp(m1)

#Predict value at any point 
test$pred <- predict(m1, test, type = "vector") 

m1_MSE <- mean((test$pred - test$Sale_Price)^2)
m1_MSE

m2 <- rpart(
  formula = Sale_Price ~ .,
  data = train, method = "anova", control = list(cp =0, xval = 10))
m2
summary(m2)
rpart.plot(m2)
plotcp(m2)

m3 <- rpart( 
  formula = Sale_Price ~ ., 
  data = train, 
  method = "anova", 
  control = list(minsplit = 10, maxdepth = 12, xval = 10) 
) 

m3$cptable 
