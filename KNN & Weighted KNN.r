mykNN <- function(train, test, y_train, y_test, k = 3, weighted = TRUE){
  
  # 1. Measure distance between point in test and all points in train
  # 2. Based on most frequent factor in k nearest points, predict test point's factor
  # 3. Compare to actual factor of test point
  # 4. Repeat for every point in test
  
  # If the response variable is factor: Do classification
  if (class(y_train) == class(y_test) && class(y_train) == 'factor')
  {
    predicted = data.frame(matrix(ncol=1, nrow=0))
    colnames(predicted) = c('Predicted')
    
    if (weighted == FALSE) # Default KNN
    {
      for (index in 1:nrow(test)) # loops entire point in test
      {
        point = test[index,]
        Distance = NULL
        for (train_index in 1:nrow(train)) # Calculates distance between point and all points in train dataset
        {
          Distance[train_index] = euc.dist(point, train[train_index,])
        }
        train_distance = cbind.data.frame(Distance, y_train)
        train_distance = train_distance[order(Distance),] # Orders classification types by distance
        
        train_neighbors = train_distance[1:k,] # K nearest points
        
        predicted_classification = names(which.max(table(train_neighbors$y_train))) # Predicted classification of test point
        
        predicted = rbind.data.frame(predicted, predicted_classification) # Append predicted classification for each point in test
        colnames(predicted) = c('Predicted')
      }
    }
    
    if (weighted == TRUE) # dnKNN
    {
      for (index in 1:nrow(test))
      {
        point = test[index,]
        Distance = NULL
        for (train_index in 1:nrow(train)) # Calculates distance between point and all points in train dataset
        {
          Distance[train_index] = euc.dist(point, train[train_index,])
        }
        train_distance = cbind.data.frame(Distance, y_train)
        train_distance['Weight'] = (1/train_distance$Distance)
        train_distance = train_distance[order(-train_distance$Weight),] # Orders classification types by descending weight
        
        train_neighbors = train_distance[1:k,] # K nearest points
        train_neighbors = aggregate(train_neighbors$Weight, by=list(Category=train_neighbors$y_train), FUN=sum) # Sum of weights per factor
        
        predicted_classification = as.character(aggregate(train_neighbors$x, by = list(train_neighbors$Category), max)[1,1])
        # Returns factor with heaviest weight
        
        predicted = rbind(predicted, predicted_classification) # Append predicted classification for each point in test
        colnames(predicted) = c('Predicted')
      }
    }
    
    comparison = cbind.data.frame(y_test, as.factor(predicted))
    colnames(comparison) = c('y_test', 'predicted')
    # Prevents confusion matrix from printing out
    # invisible(capture.output(confusion = CrossTable(x = predicted$Predicted, y = y_test, prop.chisq = FALSE, prop.t = F, prop.r = F)))
    
    k = k
    yhat = as.factor(predicted$Predicted)
    
    confusion = confusionMatrix(yhat, y_test)$table
    
    # Calculating accuracy
    correct = 0
    for (index in 1:length(y_test))
    {
      if (as.character(y_test[index]) == predicted$Predicted[index])
      {
        correct = correct + 1
      }
    }
    accuracy = correct / length(y_test)
    error = 1 - accuracy
    
    
    return(list("yhat" = yhat, "accuracy" = accuracy, "error.rate" = error, "confusion.matrix" = confusion, "k" = k))
  }
  
  # If the response variable is numeric: Do regression
  if (class(y_train) == class(y_test) && class(y_train) == 'numeric')
  {
    predicted = data.frame(matrix(ncol=1, nrow=0))
    colnames(predicted) = c('Predicted')
    pred_reg <- list()
    
    # Regular KNN
    if (weighted == FALSE) # Default KNN
    {
      for (index in 1:nrow(test)) # loops entire point in test
      {
        point = test[index,]
        Distance = NULL
        
        for (train_index in 1:nrow(train)) # Calculates distance between point and all points in train dataset
        {
          Distance[train_index,] = euc.dist(point, train[train_index,])
        }
        train_distance = cbind.data.frame(Distance, y_train)
        train_distance = train_distance[order(Distance),] # Orders classification types by distance
        
        train_neighbors = train_distance[1:k,] # K nearest points
        
        predicted_regression = mean(train_neighbors$y_train) # Predicted regression values of test point
        
        predicted = rbind.data.frame(predicted, predicted_regression) # Append predicted regression value for each point in test
        colnames(predicted) = c('Predicted')
        
        pred_reg <- append(pred_reg, predicted_regression) # appending values to a list
      }
      # return(list of objects seen below)
      
    }
    
    if (weighted == TRUE) # dnKNN
    {
      for (index in 1:nrow(test))
      {
        point = test[index,]
        Distance = NULL
        for (train_index in 1:nrow(train)) # Calculates distance between point and all points in train dataset
        {
          Distance[train_index] = euc.dist(point, train[train_index,])
        }
        train_distance = cbind.data.frame(Distance, y_train) # creating y_train and distance dataframe
        train_distance['Weight'] = (1/train_distance$Distance) # calculating the weight
        train_distance = train_distance[order(-train_distance$Weight),] # Orders values types by descending weight
        
        train_neighbors = train_distance[1:k,] # K nearest points
        predicted_regression = mean(train_neighbors$y_train) # avg y train value of closeset weights
        predicted = rbind(predicted, predicted_regression) # Append predicted classification for each point in test
        colnames(predicted) = c('Predicted')
        
        pred_reg <- append(pred_reg, predicted_regression) # appending values to a list
      }
    }
    
    regression_residual <- y_test - as.numeric(pred_reg)
    SSE <- sum( (y_test - as.numeric(pred_reg))^{2})
    return(list('k' = k, 'yhat' = unlist(as.vector(pred_reg)), 'residuals' = regression_residual, 
                'SSE' = SSE))
  }
}
