library(readxl)
library(car)
library(RColorBrewer)
library(kknn)
library(caret)

# Assignment 1. Spam classification with nearest neighbors and Logistic regression

spambase <- read_excel("E:/Machine Learning/lab_01Block01/spambase.xlsx")
n=dim(spambase)[1]
suppressWarnings(RNGkind(sample.kind = "Rounding"))
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=spambase[id,]
test=spambase[-id,]
model <- glm(Spam ~.,family="binomial",data=train)
summary(model)
# IC = 2p- 2log(L). A model with low AIC is good. The information about Fisher scoring 
# iterations is just verbose output of iterative weighted least squares. 
# A high number of iterations may be a cause for concern indicating that the 
# algorithm is not converging properly.

logistic_regression = function(data, bound)
{
  fit = predict(model,newdata = data, type = 'response')
  data_predict = ifelse(fit > bound, 1L, 0L)
  conf_mat = table(data$Spam, data_predict)
  n = nrow(data)
  error_rate = (1-sum(diag(conf_mat))/n)*100
  result = list("confusion matrix of data" = conf_mat,
  "misclassification rate of data" = error_rate)
  return(result)
}
 logistic_regression(data = train, bound = 0.5)
 logistic_regression(data = test, bound = 0.5)                                           
 logistic_regression(data = train, bound = 0.8)
 logistic_regression(data = test, bound = 0.8)

#4) standard classifier kknn()
knn_model = kknn(Spam ~ .,train, test, k = 30)
summary(knn_model)
standard_classifier = function(data, K,testing)
{
  spambase_kknn = kknn(Spam ~ ., data, testing,k = K)
  fitted_data <- fitted(spambase_kknn)
  conf_mat = table(testing$Spam, fitted_data > 0.5)
  n = nrow(data)
  error_rate = (1-sum(diag(conf_mat))/n)*100
  result = list("confusion matrix of data" = conf_mat,
                "misclassification rate of data" = error_rate)
  return(result)
}
standard_classifier(data = train, K = 30,train)
standard_classifier(data = train, K = 30,test)
standard_classifier(data = train, K = 1,train)
standard_classifier(data = train, K = 1,test)
# Assignment 3. Feature selection by cross-validation in a linear model

# linear regression
linear_regression = function(X, Y)
{
  beta = solve((t(X)%*%X))%*%t(X)%*%Y
  beta_hat = as.matrix(beta)
  return(beta)
}

linear_model = function(X,Y, Xpred)
{
  Nfolds = cbind(1,Xpred)
  beta = solve((t(X)%*%X))%*%t(X)%*%Y
  Res=Xpred1%*%beta
  return(Res)
}
t = as.matrix(swiss[1:9,2:6])
x = cbind(1,t)
y = as.matrix(swiss[1:9, 1])

