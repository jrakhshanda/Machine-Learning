#________Assignment 2. High-dimensional methods_______#
RNGversion('3.5.1')
library(pamr)
library(caret)
library(glmnet)
library(kernlab)

library(pamr)
data <- read.csv2("E:/Machine Learning/lab 02 block 02/data.csv", 
                   sep =";",header = TRUE)
data$Conference <- as.factor(data$Conference)
n = nrow(data)
set.seed(12345)
id=sample(1:n, floor(n*0.7))
train=data[id,]
test=data[-id,]
rownames(train) = 1:nrow(train)
x_train = t(train[,-4703])
y_train = train[[4703]]
rownames(train) = 1:nrow(train)
x_test = t(test[,-4703])
y_test = as.factor(test[[4703]])

#_____NSC classification____#
nsc_train = list(x = x_train, y = as.factor(y_train), 
                 geneid=as.character(1:nrow(x_train)),
                 genenames=rownames(x_train))
model = pamr.train( nsc_train)
# cv fit
cv_model=pamr.cv(model,nsc_train, nfold = 10)
pamr.plotcv(cv_model)
windows()
#minimum error model
min_error = cv_model$threshold[which.min(cv_model$error)]
max_loglik = cv_model$threshold[which.max(cv_model$loglik)]
model1 = pamr.train(nsc_train,threshold =  min_error)

#centroid plot
pamr.plotcen(model1, nsc_train, threshold = min_error)
dev.copy(png,"cp",width=5,height=40,units="in",res=200)
dev.off()

lctd_features = pamr.listgenes(model1, nsc_train, 
                               threshold = min_error, genenames=TRUE)
n_features = nrow(slctd_features)
top_features = slctd_features[1:10,]

fit1 = pamr.predict(model1,x_test,threshold = min_error, type = "class")
conf_mat1 = table(y_test,fit1)
error_rates = function(conf_matrix)
{
  return((1- sum(diag(conf_matrix))/sum(conf_matrix))*100)
}
error1 = error_rates(conf_mat1)

#_____elastic.net____#
set.seed(12345)
elx_train = as.matrix(train[,-4703])
ely_train = as.matrix(train[[4703]])
elx_test = as.matrix(test[,-4703])
ely_test = as.matrix(test[[4703]])
elastic_model = cv.glmnet(x=elx_train,y=ely_train,
                           family = "binomial",alpha = 0.5)
plot(elastic_model)
min_lambda = elastic_model$lambda.1se
fit2 = glmnet(x=elx_train,y=ely_train,family = "binomial",
              alpha = 0.5, lambda = min_lambda)
prediction = predict(fit2,elx_test,s = min_lambda,type = "class")
conf_mat2 = table(ely_test,prediction)
error2 = error_rates(conf_mat2)
EN_coef = coef(fit2, min_lambda)
EN_coef = EN_coef@Dimnames[[1]][EN_coef@i + 1]

#____SVM_____#
set.seed(12345)
svm_model = ksvm(Conference ~., data=train,kernel="vanilladot",
                 scaled=FALSE,type="C-svc")
fit3 <- predict(svm_model, test, type="decision")
svm_feat = length(train[SVindex(svm_model)])
svm_top_feat = colnames(train[SVindex(svm_model)])
conf_mat3 = confusionMatrix(fit3, test)$table
error3 = error_rates(conf_mat3)
#______3_____#
data <- read.csv2("E:/Machine Learning/lab 02 block 02/data.csv", 
                  sep =";",header = TRUE)
data$Conference <- as.factor(data$Conference)
p =  NULL
for (j in 1:4702)
{ 
  y = data[,j]
  p = rbind(p,data.frame(Features = colnames(data)[j],
    p.value  = t.test(y ~ Conference, data)$p.value))
}
p = p[order(p$p.value),]
m = nrow(p)
BH_value = integer(m)
critical_value = p$p.value[m]
BH_value[m] = p$p.value[m]
for (i in (m-1):1)
{
  adjustCalc = p$p.value[i]*(m/i)
  BH_value[i] = min(critical_value,adjustCalc)
  critical_value = BH_value[i]
}
p$BH_value= BH_value

df = p[which(p$BH_value<= 0.05), ]
df = df[order(df$p.value), ]

nrow(df)




