# Assignment 2. Inference about lifetime of machines
library(readxl)
data = unlist(read_excel("E:/Machine Learning/lab_01Block01/machines.xlsx"))

#__________(1)__________#
log_liklihood = function(x, theta)
{
  loglik = 0
  for (i in x)
  {
    loglik = loglik + log(theta*exp(-theta*i))
  }
    return(loglik)
}
theta = seq(0,3,by = 0.01)
m = log_liklihood(x = data, theta = theta)
df1 = data.frame("Theta" = theta,
                 "Data" = m)
ggplot(data = df1)+
  geom_point(aes(x = Theta,y = Data, col = Data))
# the maximum likelihood value of theta is some way between 1 and 1.5.
#_________(2)__________#
Data_6 = log_liklihood(x = data[1:6], theta = theta)
df2 = cbind(df1,Data_6)
colnames(df2) = c("Theta", "Data", "Data[1:6]")
ggplot(df2)+
  geom_point(aes(x = Theta,y = Data, col = "Data"))+
  geom_point(aes(x = Theta,y = Data_6, col = "Data[1:6]"))+
  ylab("LogLiklihood")
# Theliklihood value given by full dataset is more reliable 
#____________(3)_________#
bayes = function(x, theta)
{
  prior = 10*exp(-10*theta)
  l_theta = prior
  for (i in x)
  {
    l_theta = l_theta + log(theta*exp(-theta*i))
  }
  return(l_theta)
}
Posterior = bayes(x = data, theta = theta)
df3 = cbind(df1,Posterior )
ggplot(df3)+
  geom_point(aes(x = Theta,y = Data, col = "LogLiklihood"))+
  geom_point(aes(x = Theta,y = Posterior, col = "Posterior"))+
  ylab("Value")
which.max(df3$Posterior)


