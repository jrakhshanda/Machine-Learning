library(neuralnet)
set.seed(1234567890)
x = runif(50, 0, 10)
data = data.frame(x, Sin_x = sin(x))
train = data[1:25,] # Training
test = data[26:50,] # Validation

# Random initialization of the weights in the interval [-1, 1]
set.seed(1234567890)
weights = runif(31, min = -1,max = 1)
MSE = 0
  for(i in 1:10) 
  {
    model = neuralnet(formula = Sin_x ~ x, data = data, hidden = 10,
                        linear.output = TRUE,startweights = weights,
                      threshold = i/1000)
    fit = compute(model,test)
    MSE[i] = mean((test[,2]-fit$net.result)^2)
  }
optimal_threshhold = which.min(MSE)
optimal_model = neuralnet(formula = Sin_x ~ x, data = data, hidden = 10,
                          linear.output = TRUE,startweights = weights,
                          threshold = optimal_threshhold/1000)
plot(optimal_model)
fit = compute(x = optimal_model, test)
df = cbind(test, Prediction = prediction(optimal_model)$rep1)
ggplot(data = df)+
   geom_line(aes(x = x, y = Sin_x, col = "Actual"))+
   geom_line(aes(x = x, y = Prediction.Sin_x, col = "Prediction"))

plot(prediction(optimal_model)$rep1)
points(test, col = "red")
#_________Exam Question_______#
RNGversion("3.1.5")
set.seed(1234567890)
x <- runif(50, 0, 3)
train <- data.frame(x, Sin=sin(x))
x <- runif(50, 3, 9)
test <- data.frame(x, Sin=sin(x))
weights = runif(10, -1, 1)
model = neuralnet(formula = Sin ~ x, data = train, hidden = 3,
                          startweights = weights, linear.output = TRUE)
fit = compute(model, test)
df = cbind(train, test, Prediction = fit$net.result)
colnames(df)= c("x1", "Sin_x1", "x2", "Sin_x2", "Prediction")
ggplot(df)+
  geom_point(aes(x = x1, y = Sin_x1, col = "Train"))+
  geom_point(aes(x = x2, y = Sin_x2, col = "Test"))+
  geom_point(aes(x = x2, y = Prediction, col = "Prediction"))+
  xlab("x")+ylab("sin(x)")
