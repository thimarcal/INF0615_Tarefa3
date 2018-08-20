########################################################################
# INF-0615 - Tarefa 3 - Student Performance                            #
# Alunos: Rafael Fernando Ribeiro                                      #
#         Thiago Gomes Marçal Pereira                                  #
########################################################################

set.seed(42)
setwd("/Users/thiagom/Documents/Studies/Unicamp/MDC/INF-615/Tarefas/INF0615_Tarefa3/")
#setwd("C:\\Users\\rafaelr\\Documents\\INF015\\Tarefa3\\INF0615_Tarefa3")

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

