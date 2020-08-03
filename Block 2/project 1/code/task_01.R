#####_______LAB_1(BLOCK_2)_______________________#####
library(mboost)
sp = read.csv2(file = "E:/Machine Learning/lab_01Block02/spambase.csv", header = TRUE, sep = ";")
sp$Spam = as.factor(sp$Spam)
n=dim(sp)[1]
RNGkind(sample.kind = "Rounding")
set.seed(12345)
id=sample(1:n, floor(n*2/3))
train=sp[id,]
test=sp[-id,]
#____performance of Adaboost classification trees____#
n_tree = seq(10,100, by = 10)
adaboost = function(data)
{
  count = 1
  error_rates = 0
  for (i in n_tree) 
  {
    model = blackboost(formula = Spam ~., data = train,
                       family = Binomial(type = c("adaboost")),
                       control = boost_control(mstop = i))
    fit = predict(model, data, type = "class")
    conf_mat = table(data$Spam, fit)
    n = nrow(data)
    error_rates[count] = (1-sum(diag(conf_mat))/n)*100
    count = count + 1
  }
  return(error_rates)
}
tr_ada = adaboost(data = train)
tes_ada = adaboost(data = test)
df = data.frame("Number of Trees"= n_tree, 
                "Trainind data" = tr_ada, 
                "Testing data" = tes_ada)
ggplot(data = df)+
geom_line(aes(x = n_tree , y = tr_ada, col = "Training Data"), size = 1)+
  geom_point( aes(x = n_tree , y = tr_ada))+
  geom_line(aes(x = n_tree , y = tes_ada, col = "Testing Data"), size = 1)+
  geom_point( aes(x = n_tree , y = tes_ada))+
  xlab("Number of Tress") + ylab("Error Rates")+ ggtitle("Adaboost classification")

#_______random forests_________#
library(randomForest)
random_forest = function(data)
{
  count = 1
  error_rates = c()
  for (j in n_tree)
  {
    model2 = randomForest(Spam ~., data = train, ntree = j)
    fit2 = predict(model2, data, type = 'class')
    conf_mat = table(data$Spam, fit2)
    n = nrow(data)
    error_rates[count] = (1-sum(diag(conf_mat))/n)*100 
    count = count+1
  }
  error_rates
}
tr_rf = random_forest(train)
tes_rf = random_forest(test)
df1 = data.frame(n_tree, tr_rf, tes_rf)
# ggplot(data = df1)+
#   geom_line(aes(x = n_tree , y = tr_rf, col = "Training Data"), size = 1)+
#   geom_point( aes(x = n_tree , y = tr_rf))+
#   geom_line(aes(x = n_tree , y = tes_rf, col = "Testing Data"),size=1)+
#   geom_point( aes(x = n_tree , y = tes_rf))+
#   scale_x_continuous(breaks = seq(10, 100,20))+
#   xlab("Number of Tress") + ylab("Error Rates")+
#   ggtitle("Random Forest classification")
