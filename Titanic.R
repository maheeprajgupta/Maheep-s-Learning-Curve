test <- read.csv("test.csv")
train <- read.csv("train.csv")
str(train)
summary(train)
table(train$Survived)
prop.table(table(train$Survived))
summary(train$Sex)
mean(train$Fare)
actual_mode = table(train$Age)
actual_mode
names(actual_mode)[actual_mode == max(actual_mode)]
median(train$Fare)
range(train$Fare)
boxplot(train$Age ~ train$Pclass, xlab = "Class", ylab = "Age", col = c("red"))
var(train$Fare)
sqrt(var(train$Fare))
hist(train$Fare, main = "Fare Per Person", xlab = "Fare", col = "grey", breaks = 40, 
     xlim = c(0,300))
summary(table(train$Survived,train$Pclass))
plot(train$Fare, train$Age, xlab = 'Fare', ylab = 'Age')
cor.test(train$Age, train$Fare, method = 'pearson')
ss = prop.table(table(train$Sex, train$Survived),1)
test$Survived <- rep(0, 418)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
plot(ss)
summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1
train$Child
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x){sum(x)})
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

library(rpart)

rpartfit = rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
plot(rpartfit)
text(rpartfit)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(rpartfit)

Prediction <- predict(rpartfit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "titandtree.csv", row.names = FALSE)

test$Survived <- NA
test$Survived
dim(test)
dim(train)
head(train)
head(test)
str(train)
str(test)
train_1 = read.csv("train.csv")

combi <- rbind(train_1, test)
dim(train_1)
dim(test)
dim(combi)

combi$Name <- as.character(combi$Name)
combi$Name[1]

strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
train <- combi[1:891,]
test <- combi[892:1309,]
fit_1 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class")

fancyRpartPlot(fit_1)
Prediction <- predict(fit_1, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "titandtree_1.csv", row.names = FALSE)
try2 <- read.csv("titandtree_1.csv")
dim(try2)



fit <- glm(Survived ~ ., data=train, family = "binomial" (link = 'logit'))
summary(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "titaniclogit.csv", row.names = FALSE)
