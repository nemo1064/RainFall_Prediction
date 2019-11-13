library(party)
library(randomForest)
library(caret)

# Data Input

data <- read.csv("C:/Users/cptNemo/Desktop/CPS project/austin_weather.csv",header = TRUE)
data1=na.omit(data,invert=FALSE)
attach(data1)

# Data Partitioning

index <- createDataPartition(Rain, p = 0.7, list = FALSE)
train.df <- data1[index,-c(1,20,22)]
test.df <- data1[-index,-c(1,20,21,22)]
test.Y <- data1[-index,21]

# Random Forest

model.rf = randomForest(Rain ~ ., data= train.df)

pred <- predict(model.rf, test.df, type ="response")
head(pred)

confusionMatrix(pred,test.Y)

# Cross Validation

model.cv <- train(
  Rain ~., data = train.df[,-c(1,20,22)], method = "rf",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 20
) 
model.cv
plot(model.cv)
k=model.cv$bestTune
k

pred.cv = predict(model.cv,test.df)
confusionMatrix(pred.cv,test.Y)

model.rf1 = randomForest(Rain ~ ., data= train.df , mtry = 15)
pred1 <- predict(model.rf1, test.df, type ="response")
confusionMatrix(pred1,test.Y)



