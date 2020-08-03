library(ggplot2)
x = swiss[,-1]
y = as.matrix(swiss[,1])

linear_regression = function(X,Y)
{
  X = cbind(1,X)
  beta = solve(t(X) %*% X) %*% t(X) %*% Y
  return(beta)
}

cv = function(X,Y,Nfolds)
{
  n = length(Y)
  p = ncol(X)
  Nfeat = 2^p-1
  set.seed(12345)
  ind=sample(n,n)
  X = as.matrix(X[ind,])
  Y = as.matrix(Y[ind,])
  Features = list()
  MSE = numeric()
  FeaturesCount = numeric()
  Features = character()
  count = 1
  for (i in 1:ncol(X))
  {
    t = combn(1:ncol(X), i)
    
    for (j in 1:ncol(t)) 
    {
      col_combinations = t[,j]
      SSE = 0
      groupSize = n/Nfolds
      startingIndex = 1
      for (k in 1:Nfolds)
      {
        endingIndex =1
        if(k == Nfolds)
        {
          endingIndex =  nrow(X)
        }
        else
        {
          endingIndex = startingIndex + groupSize
        }
        training_data = X[-c(startingIndex:endingIndex),col_combinations]
        
        testing_data = cbind(1,X[c(startingIndex:endingIndex),col_combinations])
        
        model = linear_regression(training_data,Y[-c(startingIndex:endingIndex),])
        
        fitted_data = testing_data %*% model
        
        SSE = SSE + sum((Y[c(startingIndex:endingIndex),] - fitted_data)^2)
        
        startingIndex = startingIndex + groupSize + 1
      }
      
      MSE[count] = SSE/n
      FeaturesCount[count] = length(col_combinations)
      Features[count] = paste( colnames(X[,col_combinations]),collapse = ",")
      count = count + 1
    }
  }
plot(x = FeaturesCount,y = MSE, main = "CV scores",ylab = "Features Count")
m = which.min(MSE)
return(list("CV" = MSE[m],
            "Features" = Features[m],
            "Features Count" = FeaturesCount[m]))
}
cv(x,y,5)

