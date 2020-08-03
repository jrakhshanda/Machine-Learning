library(readxl)
library(glmnet)
library(dplyr)
data = read.csv("C:/Users/Rakshanda/Desktop/ML Exams/732A99/Influenza.csv")
#________Question(1)________#
log_liklihood = function(y, lambda)
{
  loglik = 0
  for (i in y)
  {
    loglik = loglik +(lambda - i*log(lambda) + factorial(log(i)))
  }
  loglik
}
y = data$Mortality
Lambda = seq(10, 2910, by = 100)
Logliklihood = log_liklihood(y, Lambda)
df = data.frame(Lambda, Logliklihood)
ggplot(data = df)+
geom_line(aes(x = Lambda,y = Logliklihood))
min.lambda = Lambda[which.min(Logliklihood)]
#________Question(2)________
Mortality = data$Mortality
influenza =  as.matrix(cbind(scale(data[,-3], scale = TRUE), Mortality)) 
n = nrow(influenza)
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=influenza[-id,]
test=influenza[id,]

X = train[,-9]
Y = train[,9]
lasso_cv = cv.glmnet(x = X, y = Y, family = "poisson",
                     alpha = 1, lambda = seq(0,100,by=0.01))
plot(lasso_cv,xvar="lambda",label=TRUE)
optimal_lambda = lasso_cv$lambda.1se
optimal_lasso = glmnet(x = X, y = Y, family = "poisson",
                       alpha = 1, lambda = optimal_lambda)
selected_coefficients = coefficients(optimal_lasso)
fit = predict(optimal_lasso, test[,-9], type = "response")
MSE = mean((test[,9] - fit)^2)
exp(selected_coefficients[1])
#__________(3)________#
bh_data =  data %>% filter(Year == 1995 | Year == 1996)
p_value = NULL
pvalue  = data.frame(p_value = numeric(),FeatureName = character())
for (i in 2:9)
{ 
 #y = bh_data[,i]
 t_test = t.test(bh_data[,i] ~ bh_data[,1], bh_data, paired = TRUE)$p.value
 pvalue = rbind(pvalue,data.frame(p_value = t_test, FeatureName = colnames(bh_data)[i]))
}
pvalue
bh = p.adjust(pvalue$p_value, method = "BH")
ind = pvalue[which(bh <= 0.05), ]
ind = ind[order(ind$p_value), ]
#_________(4)________#
n = nrow(data)
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train1=data[id,]
test1=data[-id,]
pca = prcomp(train1[,-3])
sm = summary(pca)
screeplot(pca, type = c("barplot", "lines"),
          main = "Scree Plot")
#two principle components are needed to capture more than 90% percent of data.
pca_data = data.frame(sm$x, train1$Mortality)
X1 = as.matrix(pca_data[,1:8])
Y1 = as.matrix(pca_data[,9])
pca_cvlasso = cv.glmnet(x = X1, y = Y1,
                        family = "poisson",alpha = 1, lambda = seq(0,50,by=0.1))
plot(pca_cvlasso,xvar="lambda",label=TRUE)
opt_lambda = pca_cvlasso$lambda.1se
opt_lasso = glmnet(x = X1, y = Y1, family = "poisson",
                       alpha = 1, lambda = opt_lambda)
slctd_coefficients = coefficients(opt_lasso)



