library(tree)
library(rpart)
library(rpart.plot)
library(caret)
library(bst)


#Data Input

data <- read.csv("C:/Users/cptNemo/Desktop/CPS project/austin_weather.csv",header = TRUE)
data1=na.omit(data,invert=FALSE)
attach(data1)

data2=data1[,-c(1,20,22)]
tree.model =tree(Rain ~. , data2,method = "class" )
summary(tree.model)

plot(tree.model )
text(tree.model ,pretty =0)


# Train And Test Data

index <- createDataPartition(Rain, p = 0.7, list = FALSE)
train = data1[index,-c(1,20,22)]
test = data1[-index,-c(1,20,22)]
test.Y = Rain[-index]

# Tree Model

tree.model1 = rpart(Rain ~ . ,data = train, method = "class", cp =0.0202)
rpart.plot(tree.model1)
plot(tree.model1)
text(tree.model1)

tree.pred = predict(tree.model1 ,test, type = "class")
table(tree.pred,test.Y)
confusionMatrix(tree.pred,test.Y)

# Cross Validation

model <- train(
  Rain ~., data = data1[,-c(1,20,22)], method = "rpart",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 20
  ) 
model
plot(model)
k=model$bestTune
k

# Prunning

ptree<-  prune(tree.model1,cp=0.12303)
rpart.plot(ptree)
plot(ptree)
text(ptree,pretty = 0)

ptree.pred = predict(ptree ,test, type = "class")
table(ptree.pred,test.Y)
confusionMatrix(ptree.pred,test.Y)

# Using Gini Indexing

model1 <- train(
  Rain ~., data = data1[,-c(1,20,22)],parms = list(split = "gini"),
  method = "rpart",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 20
) 
plot(model1)
model1$bestTune

tree.pred.gini = predict(model1 ,test)
table(tree.pred.gini,test.Y)
confusionMatrix(tree.pred.gini,test.Y)

