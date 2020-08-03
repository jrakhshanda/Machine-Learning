library(kernlab)
library(e1071)
library(caret)
#___________SUPPORT VECTOR MACHINES_________#
data = data(spam)
n = nrow(data)
suppressWarnings(RNGkind(sample.kind = "Rounding"))
set.seed(12345)
index = sample(1:n, n)
train = data[index[1:2301],]
valid = data[index[2302:3451],]
test = data[index[3452:4601],]
train_data = data[index[1:3451],]

svm_model1= ksvm(type~., data = train, kernel = rbfdot(sigma=0.05), C = 0.5)
svm_model2= ksvm(type~., data = train, kernel = rbfdot(sigma=0.05), C = 1)
svm_model3= ksvm(type~., data = train, kernel = rbfdot(sigma=0.05), C = 5)

rates = function(model, data)
{
  n = nrow(data)
  prediction = predict(model, data)
  conf_mat = confusionMatrix(prediction, data$type)$table
  error = round((1-sum(diag(conf_mat))/n)*100, 2)
  acc = round((sum(diag(conf_mat))/n)*100, 2)
  lst = list("Confucion Matrix" = conf_mat,
             "Error Rates" = error,
             "Accuracy" = acc)
  return(lst)
}
rate1 = rates(model = svm_model1, data = valid)
rate2 = rates(model = svm_model2, data = valid)
rate3 = rates(model = svm_model3, data = valid)
#optimal_model
optimal_model = ksvm(type~., data = train_data, 
                     kernel = rbfdot(sigma=0.05), C = 1)
rate = rates(model = optimal_model, data = test)
# model reported to user
model = optimal_model = ksvm(type~., data, 
                             kernel = rbfdot(sigma=0.05), C = 1)
