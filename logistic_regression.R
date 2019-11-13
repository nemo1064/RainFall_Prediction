library(tidyverse)
library(boot)
library(forecast)
library(tseries) 
library(caret)
library(ROCR)
library(corrplot)
library(psych)

# Data Input

data <- read.csv("C:/Users/cptNemo/Desktop/CPS project/austin_weather.csv",header = TRUE)
data1=na.omit(data,invert=FALSE)
attach(data1)
summary(data1)
summary(Rain)


mat=cor(data1[,-c(1,20,21)],method = "spearman")


corrplot(mat,method = "square")
corrplot.mixed(mat, lower.col = "black",upper = "square", number.cex = .7)


# Data Partitioning

index <- createDataPartition(Rain, p = 0.7, list = FALSE)
# Training set
train.df <- data1[index,]
# Testing dataset
test.df <- data1[-index,]

summary(train.df)
summary(test.df)

# Logistic regression

colnames(data1)


model <- glm(Rain ~ TempHighF+TempAvgF+TempLowF+DewPointHighF+DewPointAvgF+DewPointLowF+HumidityHighPercent+HumidityAvgPercent+HumidityLowPercent+SeaLevelPressureHighInches+SeaLevelPressureAvgInches+VisibilityLowMiles+VisibilityHighMiles+VisibilityAvgMiles+WindGustMPH+WindHighMPH+WindAvgMPH, data = train.df, family = binomial)

summary(model)


predicted_values <- predict(model, test.df[,-c(1,20,21,22)], type = "response")
head(predicted_values)

# Validation

table(Rain)
nrows_prediction<-nrow(test.df)
prediction <- data.frame(c(1:nrows_prediction))
colnames(prediction) <- c("Rain")
str(prediction)
prediction$Rain <- as.character(prediction$Rain)
prediction$Rain <- "yes"
prediction$Rain[ predicted_values < 0.5] <- "no"
prediction$Rain <- as.factor(prediction$Rain)

#Confusion Matrix

table(prediction$Rain, test.df$Rain)

confusionMatrix(prediction$Rain,test.df$Rain)

glm.diag.plots(model)

ggplot(test.df, aes(x = test.df$HumidityLowPercent, y = predicted_values))+
  geom_point() + # add points
  geom_smooth(method = "glm", # plot a regression...
              method.args = list(family = "binomial"))




