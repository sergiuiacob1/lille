library(party)
library(randomForest)

# (1) Sur la base de données IRIS, développez le modèle de
# régression linéaire, en prédisant le Sepal.Length sur la base de Sepal.Width (8), Petal.Length (6) et Petal.Width (5).
# (2) Réaliser des tests sur d’autres exemples.

# initialize the data
# split data into train and test datasets, proportion: 70% 30%

train.data <- iris
test.data <- iris

initialiseData <- function(no_folds, current_fold){
  # if no_folds = 5 for example, test_data will represent the first 20% of the data
  # then the next 20% and so on
  test_split = 1/no_folds
  n = length(iris$Species)
  
  ind <- rep (1, n)
  # mark the test data
  ind[((current_fold - 1) * test_split * n + 1) : (current_fold * test_split * n)] <- 2
  # ind <- sample(2, nrow(iris), replace = TRUE, prob = c(1 - test_split, test_split))
  # use "<<-" to keep these as global variables
  train.data <<- iris[ind == 1, ]
  test.data <<- iris[ind == 2, ]
}

analyseData <- function(){
  # check data
  str(iris)
  # some info
  summary(iris)
  # see some correlations
  cor(train.data$Sepal.Length, train.data$Sepal.Width)
  cor(train.data$Sepal.Length, train.data$Petal.Length)
  cor(train.data$Sepal.Length, train.data$Petal.Width)
}


trainLinearModel <- function(){
  model <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=train.data)
  # plot(model)
  return (model)
}

trainDecisionTree <- function(){
  model <- ctree(formula = Species ~ Sepal.Width + Petal.Length + Petal.Width, data=train.data)
  # plot(model)
  return (model)
}

evaluateSepalLengthPredictions <- function(model){
  predictions <- predict (model, test.data)
  # calculate score
  mse <- mean((test.data$Sepal.Length - predictions)^2)
  return (mse)
}

evaluateSpeciesPredictions <- function(model){
  predictions <- predict (model, test.data)
  # calculate accuracy
  accuracy <- sum(predictions == test.data$Species) / length(test.data$Species)
  return (accuracy)
}



# analyseData()

k_folds <- 5
mse <- 1:k_folds
accs <- 1:k_folds
accs_rf <- 1:k_folds
# shuffle data before
iris <- iris[sample(nrow(iris)),]
for (i in 1:k_folds){
  initialiseData(no_folds=k_folds, current_fold=i)
  print (mean(test.data$Sepal.Length))
  # This one predicts Sepal Length
  model <- trainLinearModel()
  mse[i] <- evaluateSepalLengthPredictions(model)
  # sprintf ("Linear model's MSE score is: %f\n", score)
  
  # This one predicts Species
  tree <- trainDecisionTree()
  accs[i] <- evaluateSpeciesPredictions(tree)
  # sprintf ("Decision tree's accuracy is: %f\n", score * 100)
  
  # Random forest, predicting Species
  rf <- randomForest(Species ~ . , data=train.data, ntree=100, proximity=T)
  accs_rf[i] <- evaluateSpeciesPredictions(rf)
}

sprintf("Average MSE score for linear model predicting Sepal.Length: %f", mean(mse))
sprintf("Standard Deviation for MSE score for linear model predicting Sepal.Length: %f", sd(mse))
sprintf("Average accuracy for the decision tree: %f", mean(accs))
sprintf("Standard Deviation for the decision tree: %f", sd(accs))
sprintf("Average accuracy for the Random Forest Classifier: %f", mean(accs_rf))
sprintf("Standard Deviation for the Random Forest Classifier: %f", sd(accs_rf))

# view the tree
# plot(tree)



