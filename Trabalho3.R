########################################################################
# INF-0615 - Tarefa 3 - Student Performance                            #
# Alunos: Rafael Fernando Ribeiro                                      #
#         Thiago Gomes MarÃ§al Pereira                                  #
########################################################################

set.seed(42)
#setwd("/Users/thiagom/Documents/Studies/Unicamp/MDC/INF-615/Tarefas/INF0615_Tarefa3/")
setwd("C:\\Users\\rafaelr\\Documents\\INF015\\Tarefa3\\INF0615_Tarefa3")

# Reading data
train_data <- read.csv("student_performance_train.data", header = TRUE)
val_data<- read.csv("student_performance_val.data", header = TRUE)
#test_data<- read.csv("student_performance_test.data", header = TRUE)

dim(train_data)
summary(train_data)
dim(val_data)
summary(val_data)
#dim(test_data)
#summary(test_data)

# Predict data using model and evaluate
predictAndEvaluate <- function(model, data){
  prediction = predict(model, data)
  prediction = as.numeric(prediction[,2] >= 0.5)
  prediction[prediction==0] = "0"
  prediction[prediction==1] = "1"
  
  CM = as.matrix(table(Actual = data$approved, Predicted = prediction))
  
  if (dim(CM)[2] == 1) {
    CM <- cbind(CM, c(0,0))
  }
  
  TPR = CM[2,2] / (CM[2,2] + CM[2,1])
  TNR = CM[1,1] / (CM[1,1] + CM[1,2])
  ACCNorm = mean(c(TPR, TNR))
  
  return(list(CM=CM, ACCNorm=ACCNorm))
}

data.convert <- function (train_data) {
  train_data$Medu <- as.factor(train_data$Medu)
  train_data$Fedu <- as.factor(train_data$Fedu)
  train_data$traveltime <- as.factor(train_data$traveltime)
  train_data$studytime <- as.factor(train_data$studytime)
  train_data$failures <- as.factor(train_data$failures)
  train_data$famrel <- as.factor(train_data$famrel)
  train_data$freetime <- as.factor(train_data$freetime)
  train_data$goout <- as.factor(train_data$goout)
  train_data$Dalc <- as.factor(train_data$Dalc)
  train_data$Walc <- as.factor(train_data$Walc)
  train_data$health <- as.factor(train_data$health)
  train_data$approved <- as.factor(train_data$approved)
  
  train_data
}

train_data <- data.convert(train_data)
val_data <- data.convert(val_data)

#scatter plot

#baseline tree
library(rpart)
treeModel <- rpart(formula = "approved ~.", data=train_data, parms = list(split="information"), method = "class")
predictAndEvaluate(treeModel, train_data)
predictAndEvaluate(treeModel, val_data)

summary(treeModel)
printcp(treeModel)

plot(treeModel, uniform=TRUE)
text(treeModel, use.n=TRUE, all=TRUE, cex=.8)

# test random forest
library(randomForest)

###### PLOT ACCNorm for nTree
nTree <- c(5, 10,25, 50, 100, 500)
accPerNTree <- data.frame(ntree=numeric(6), accTrain=numeric(6) , accVal=numeric(6))
for (i in 1:6){
  #formula <- approved ~ failures+higher+school+freetime+schoolsup+Walc+Fedu+age+famrel+Mjob+reason+health
  formula <- approved ~ .
  rfModel <- randomForest(formula=formula, data= train_data, ntree=nTree[i])
  print(summary(rfModel))
  rfPrediction <- predict(rfModel, train_data) 
  rfCM <- as.matrix(table(Actual = train_data$approved, Predicted = rfPrediction))
  rfTPR <- rfCM[2,2] / (rfCM[2,2] + rfCM[2,1])
  rfTNR <- rfCM[1,1] / (rfCM[1,1] + rfCM[1,2])
  rfACCNormT <- mean(c(rfTPR, rfTNR))
  
  rfPrediction <- predict(rfModel, val_data) 
  rfCM <- as.matrix(table(Actual = val_data$approved, Predicted = rfPrediction))
  rfTPR <-rfCM[2,2] / (rfCM[2,2] + rfCM[2,1])
  rfTNR <- rfCM[1,1] / (rfCM[1,1] + rfCM[1,2])
  rfACCNormV <- mean(c(rfTPR, rfTNR))
  
  accPerNTree[i,] <- c(nTree[i], rfACCNormT, rfACCNormV)
}
accPerNTree

# test bagging
library(ipred)
bagModel <- bagging(formula = approved ~ ., data = train_data, coob = TRUE)
summary(bagModel)
rfPrediction <- predict(bagModel, train_data) 
rfCM <- as.matrix(table(Actual = train_data$approved, Predicted = rfPrediction))
rfTPR <- rfCM[2,2] / (rfCM[2,2] + rfCM[2,1])
rfTNR <- rfCM[1,1] / (rfCM[1,1] + rfCM[1,2])
rfACCNormT <- mean(c(rfTPR, rfTNR))

rfPrediction <- predict(bagModel, val_data) 
rfCM <- as.matrix(table(Actual = val_data$approved, Predicted = rfPrediction))
rfTPR <-rfCM[2,2] / (rfCM[2,2] + rfCM[2,1])
rfTNR <- rfCM[1,1] / (rfCM[1,1] + rfCM[1,2])
rfACCNormV <- mean(c(rfTPR, rfTNR))
