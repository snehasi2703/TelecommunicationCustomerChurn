
#Evaluate Model Performance
evaluateModel <- function(data, actual, predicted) 
{
  confusionMatrix <- table(data[[actual]], data[[predicted]])
  print(confusionMatrix)
  tp <- confusionMatrix[rownames(confusionMatrix) == 1, colnames(confusionMatrix) == 1]
  fn <- confusionMatrix[rownames(confusionMatrix) == 1, colnames(confusionMatrix) == 0]
  fp <- confusionMatrix[rownames(confusionMatrix) == 0, colnames(confusionMatrix) == 1]
  tn <- confusionMatrix[rownames(confusionMatrix) == 0, colnames(confusionMatrix) == 0]
  accuracy <- (tp + tn) / (tp + fn + fp + tn)
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  fscore <- 2 * (precision * recall) / (precision + recall)
  metrics <- c("Accuracy" = accuracy,
               "Precision" = precision,
               "Recall" = recall,
               "F-Score" = fscore)
  return(metrics)
}

#ROC curve 
rxrocCurve <- function(data, actual, predicted) 
{
  data <- data[, c(actual, predicted)]
  data[[actual]] <- as.numeric(as.character(data[[actual]]))
  rxRocCurve(actualVarName = actual,
             predVarNames = predicted,
             data = data)
}

