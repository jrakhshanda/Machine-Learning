library(readxl)
library(ggplot2)
library(MASS)
library(glmnet)
library(ggpairs)
# Assignment 4. Linear regression and regularization
tecator = read_excel("E:/Machine Learning/lab_01Block01/tecator.xlsx")

pl = ggplot(tecator,mapping = aes(x=Moisture, y=Protein)) + geom_point( size=2, colour="#993399") + 
  xlab("Moisture") + ylab("Protein")+geom_smooth(method = lm,formula =y ~ x ) 
pl

#3)
suppressWarnings(RNGkind(sample.kind = "Rounding"))
n= nrow(tecator)
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=tecator[-id,]
test=tecator[id,]
#expected Moisture is a polynomial function of Protein including the polynomial 
#terms up to power i.
  train_MSE = 0
  test_MSE = 0
  for (i in 1:6) 
  {
    model = lm(Moisture ~ poly(Protein,i), data = train)
    fit = predict(model, test, type = 'response')
    train_MSE[i] = mean(model$residuals^2)
    test_MSE[i] =  mean((test$Moisture-fit)^2)
  }
  df1 = data.frame("Degree" = 1:6, train_MSE, test_MSE)
   print(ggplot(data = df1)+
    geom_line(mapping = aes(x = Degree , y = train_MSE, col = "Training Data"))+
    geom_point(aes(x = Degree , y = train_MSE))+
    geom_line(mapping = aes(x = Degree , y = test_MSE,col = "Testing Data"))+
    geom_point(aes(x = Degree , y = test_MSE)))+
    xlab("Degrees of polynomial") + ylab("Mean Square Errors")+theme_gray()

#4) variable selection of a linear model
 library(MASS)
   stepData = tecator[,-c(1,104,103)]
   step_model = lm(Fat ~ ., data = stepData)
   step = stepAIC (step_model,trace = FALSE,direction = "both")
   step_forward = stepAIC(step_model,trace = FALSE,direction = "forward")
   step_backward = stepAIC(step_model,trace = FALSE,direction = "backward")
   df = data.frame( Both = (length(step$coefficients)-1),
                    Backward = (length(step_backward$coefficients)-1),
                    Forward = (length(step_forward$coefficients)-1))

#5) Ridge regression
  X = as.matrix(tecator[,2:100])
  Y = as.matrix(tecator[,102])
  cv_ridge = cv.glmnet(x = X, y = Y, alpha = 0, 
               lambda = 10^seq(3, -2, by = -.1))
  plot(cv_ridge,xvar="lambda",label=TRUE)
  optimal_lambda = cv_ridge$lambda.1se
  optimal_ridge = glmnet(x = X, y = Y, alpha = 0, lambda = optimal_lambda)
  
  #6) LASSO

  X = as.matrix(tecator[,2:100])
  Y = as.matrix(tecator[,102])
  cv_lasso = cv.glmnet(x = X, y = Y, alpha = 1, 
               lambda = 10^(seq(-3,3,by=0.1)))
  plot(cv_lasso,xvar="lambda",label=TRUE)
  
optimal_lambda = cv_lasso$lambda.1se
#optimal lasso model
opt_lasso = glmnet(X,Y, lambda = optimal_lambda)
significant_vars = coefficients(opt.lasso)  
#optimal model used 17 variables to fit the model.
  
  
