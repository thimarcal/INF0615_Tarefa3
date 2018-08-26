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
test_data<- read.csv("student_performance_test.data", header = TRUE)

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

# Predict data using model and evaluate
predictAndEvaluateRF <- function(model, data){
  prediction = predict(model, data)
  
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
test_data <- data.convert(test_data)

dim(train_data)
summary(train_data)
dim(val_data)
summary(val_data)
dim(test_data)
summary(test_data)

#baseline tree
library(rpart)
treeModel <- rpart(formula = "approved ~.", data=train_data, parms = list(split="information"), method = "class")
predictAndEvaluate(treeModel, train_data) # 0,7797283
predictAndEvaluate(treeModel, val_data)  #0,703485

summary(treeModel)
printcp(treeModel)

plot(treeModel, uniform=TRUE)
text(treeModel, use.n=TRUE, all=TRUE, cex=.8)

#Save the complete DT into file
post(treeModel, file = "tree.ps",title = "Classification Tree for Income")


#Tree using Gini
treeModelGini <- rpart(formula = "approved ~.", data=train_data, parms = list(split="gini"), method = "class")
predictAndEvaluate(treeModelGini, train_data) # 0,7797283
predictAndEvaluate(treeModelGini, val_data) # 0,7034085

#summary(treeModelGini)
printcp(treeModelGini)

plot(treeModelGini, uniform=TRUE)
text(treeModelGini, use.n=TRUE, all=TRUE, cex=.8)

#Save the complete DT into file
post(treeModelGini, file = "treeGini.ps",title = "Classification Tree for Income")


# Allow Tree to grow
treeModelGrow <- rpart(formula = "approved ~.", data=train_data, method="class",
                       control=rpart.control(minsplit=10, cp=0.0001),
                       parms= list(split="information"))
predictAndEvaluate(treeModelGrow, train_data) # 0,888123
predictAndEvaluate(treeModelGrow, val_data) # 0,6461884

#Print the table with complexity parameters
printcp(treeModelGrow)

plot(treeModelGrow$cptable[,4])

#summary(treeModelGrow)
printcp(treeModelGrow)

plot(treeModelGrow, uniform=TRUE)
text(treeModelGrow, use.n=TRUE, all=TRUE, cex=.8)

#Save the complete DT into file
post(treeModelGrow, file = "treeGrow.ps",title = "Classification Tree for Income")


######### POST PRUNE ########
#Prune the tree based on the complexity parameter that minimizes 
#the error in cross-validation (xerror)
minCP = treeModelGrow$cptable[which.min(treeModelGrow$cptable[,"xerror"]),"CP"]

ptree = prune(treeModelGrow, cp=minCP)
#summary(ptree)

#Plot the pruned tree
plot(ptree, uniform=TRUE)
text(ptree, use.n=TRUE, all=TRUE, cex=.8)

#Save the complete DT into file
post(ptree, file = "prunedTree.ps",title = "Classification Tree for Income")

predictAndEvaluate(ptree, train_data) # 0,8044526
predictAndEvaluate(ptree, val_data) # 0,680273

# test random forest
library(randomForest)

###### PLOT ACCNorm for nTree
nTree <- c(1, 5, 10,25, 50, 100, 500)
accPerNTree <- data.frame(ntree=numeric(7), accTrain=numeric(7) , accVal=numeric(7))
for (i in 1:7){
  #formula <- approved ~ failures+higher+school+Fedu+Medu+schoolsup+Dalc+reason+address
  formula <- approved ~ .
  rfModel <- randomForest(formula=formula, data= train_data, ntree=nTree[i], importance=TRUE,
                          proximity=TRUE, replace = TRUE)
  
  rfACCNormT <- predictAndEvaluateRF(rfModel, train_data) 
  rfACCNormV <- predictAndEvaluateRF(rfModel, val_data) 
  
  accPerNTree[i,] <- c(nTree[i], rfACCNormT$ACCNorm, rfACCNormV$ACCNorm)
}
accPerNTree
g <- ggplot(accPerNTree, aes(ntree)) +
  geom_line(aes(y=accTrain,colour="Train")) +
  geom_line(aes(y=accVal, colour="Val")) +
  scale_color_manual(breaks= c("Train", "Val"), values = c("blue", "green")) +
  xlab("Numero de Arvores") +
  ylab("Acuracia") +
  labs(title="Random Forest Test")
g
accPerNTree
###################################
#  ntree  accTrain    accVal
#1     1 0.8415952 0.5990013
#2     5 0.9326593 0.6214495
#3    10 0.9600797 0.6312534
#4    25 0.9744281 0.6513194
#5    50 0.9872855 0.6225948
#6   100 0.9869996 0.6448140
#7   500 0.9872141 0.6263515
###################################

# test bagging
library(ipred)
nBag <- c(10,25, 50, 100)
accPerNBag <- data.frame(ntree=numeric(4), accTrain=numeric(4) , accVal=numeric(4))
for (i in 1:4){
  bagModel <- bagging(formula = approved ~ ., data = train_data, nbagg=nBag[i], coob = TRUE, control = rpart.control(minsplit=10))
  #summary(bagModel)
  rfACCNormT <- predictAndEvaluateRF(rfModel, train_data) 
  rfACCNormV <- predictAndEvaluateRF(rfModel, val_data)
  rfACCNormT
  rfACCNormV
  
  accPerNBag[i,] <- c(nBag[i], rfACCNormT$ACCNorm, rfACCNormV$ACCNorm)
}
accPerNBag
g <- ggplot(accPerNBag, aes(ntree)) +
  geom_line(aes(y=accTrain,colour="Train")) +
  geom_line(aes(y=accVal, colour="Val")) +
  scale_color_manual(breaks= c("Train", "Val"), values = c("blue", "green")) +
  xlab("Numero de Arvores") +
  ylab("Acuracia") +
  labs(title="Bagging Test")
g


#################################################################################################
# Based on the results, the choice was to use a random Forest with nTree = 50.                  #
# The results in the validation data were the highest for models with accuracy greater than 95% #
# in train data.                                                                                #
#################################################################################################
# formula <- approved ~ . + failures^2+higher^2+school^2+freetime^2+schoolsup^2+Walc^2+Fedu^2+age^2+
#+                             famrel^2+Mjob^2+reason^2+health^2
# 0.9838031 / 0.6557632

# formula <- approved ~ . + failures^3+higher^3+school^3+freetime^3+schoolsup^3+Walc^3+Fedu^3+age^3+
# famrel^3+Mjob^3+reason^3+health^3
# 0.9822406 / 0.6410574

# formula <- approved ~ . + failures*higher*school*freetime*schoolsup*Walc*Fedu*age*
# famrel*Mjob*reason*health
# 0.9870711 / 0.654847

# As the tested results were not better, we decided to use it as is.

# pplot better RF model
rfModel <- randomForest(formula=formula, data= train_data, ntree=10, importance=TRUE,
                        proximity=TRUE, replace = TRUE)
plot(rfModel, log = "y")
varImpPlot(rfModel)

predictAndEvaluateRF(rfModel, train_data)#0.958
predictAndEvaluateRF(rfModel, val_data)  #0.713
predictAndEvaluateRF(rfModel, test_data) #0.664

formula <- "approved ~ failures+higher+Fedu+reason+schoolsup+paid+school+freetime+famrel+Walc"
treeModel <- rpart(formula = formula, data=train_data, parms = list(split="gini"), method = "class")
predictAndEvaluate(treeModel, train_data)#0.774
predictAndEvaluate(treeModel, val_data)  #0.715
predictAndEvaluate(treeModel, test_data) #0.667

plot(treeModel, uniform=TRUE)
text(treeModel, use.n=TRUE, all=TRUE, cex=.8)

