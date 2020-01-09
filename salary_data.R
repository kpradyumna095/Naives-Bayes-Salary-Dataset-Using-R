# Libraries
library(naivebayes)
library(ggplot2)
library(caret)
library(e1071)

#loading test and train splitted data
# Preparing model of classification for greater or less than equal to 50k salary
# Data(Train)
train_sal <- read.csv(file.choose())
str(train_sal)

View(train_sal)
train_sal$educationno <- as.factor(train_sal$educationno)
class(train_sal)

# Data(Test)
test_sal <- read.csv(file.choose())
str(test_sal)

View(test_sal)
test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)

# Naive Bayes Training Model 
Model <- naiveBayes(train_sal$Salary ~ ., data = train_sal)
Model

#Evaluating model performance
Model_pred <- predict(Model,test_sal)

confusionMatrix(Model_pred,test_sal$Salary)

Accuracy1 <- mean(Model_pred==test_sal$Salary)
Accuracy1

#From confusion matrix we are getting 811 cases where salary > 50k in actual 
# but our model predict it wrongly which is severe case so we will check with transformation it improves or not

#Applying transformation
# Naive Bayes Training Model 
Model_2 <- naiveBayes(train_sal$Salary ~ ., data = train_sal , laplace = 2)
Model_2

#Evaluating model performance
Model_pred_2 <- predict(Model_2,test_sal)

confusionMatrix(Model_pred_2,test_sal$Salary)

Accuracy2 <- mean(Model_pred_2==test_sal$Salary)
Accuracy2

Accuracy1


# As accuracy not increasing so we will go with model1 