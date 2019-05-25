# function for logistic regression
LogisticRegression <- function(A_train,y,A_test=data.frame(),cv=5,seed=123,metric="auc",importance=0)
{
  # defining evaluation metric
  score <- function(a,b,metric)
  {
    switch(metric,
           accuracy = sum(abs(a-b)<=0.5)/length(a),
           auc = auc(a,b),
           logloss = -(sum(log(1-b[a==0])) + sum(log(b[a==1])))/length(a),
           mae = sum(abs(a-b))/length(a),
           precision = length(a[a==b])/length(a),
           rmse = sqrt(sum((a-b)^2)/length(a)),
           rmspe = sqrt(sum(((a-b)/a)^2)/length(a)))           
  }
  
  if (metric == "auc")
  {
    library(pROC)
  }
  
  cat("Preparing Data\n")
  A_train$order <- seq(1, nrow(A_train))
  A_train$result <- as.numeric(y)
  
  set.seed(seed)
  A_train$randomCV <- floor(runif(nrow(A_train), 1, (cv+1)))
  
  # cross validation
  cat(cv, "-fold Cross Validation\n", sep = "")
  for (i in 1:cv)
  {
    X_build <- subset(A_train, randomCV != i, select = -c(order, randomCV))
    X_val <- subset(A_train, randomCV == i) 
    
    # building model
    model_lr <- glm(result ~., data=X_build, family=binomial())
    
    # predicting on validation data
    pred_lr <- predict(model_lr, X_val, type="response")
    X_val <- cbind(X_val, pred_lr)
    
    # predicting on test data
    if (nrow(A_test) > 0)
    {
      pred_lr <- predict(model_lr, A_test, type="response")
    }
    
    cat("CV Fold-", i, " ", metric, ": ", score(X_val$result, X_val$pred_lr, metric), "\n", sep = "")
    
    # initializing outputs
    if (i == 1)
    {
      output <- X_val
      if (nrow(A_test) > 0)
      {
        A_test <- cbind(A_test, pred_lr)
      }      
    }
    
    # appending to outputs
    if (i > 1)
    {
      output <- rbind(output, X_val)
      if (nrow(A_test) > 0)
      {
        A_test$pred_lr <- (A_test$pred_lr * (i-1) + pred_lr)/i
      }            
    }
    
    gc()
  } 
  
  # final score of evaluation
  output <- output[order(output$order),]
  cat("\nLogisticRegression ", cv, "-Fold CV ", metric, ": ", score(output$result, output$pred_lr, metric), "\n", sep = "")
  
  # returning CV predictions and test data with predictions
  return(list("train"=output, "test"=A_test))  
}