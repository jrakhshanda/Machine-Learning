library(fitdistrplus)
library(ggplot2)

machinesData= read_excel(path = "machines.xlsx")
n = nrow(machinesData)
prob = function(theta, x)
{
  return(theta * exp(-theta*x))
}

theta = seq(0.1,10,0.1)
p = sapply(theta, prob, x = machinesData)
llik =lapply(p, function(i){ sum(log(i))})
llik = unlist(llik)

df= data.frame(theta = theta,ll = llik)
ggplot(df,aes(x=theta,y=ll))+geom_line()



p2 = sapply(theta, prob, x = machinesData[1:6,])
llik2 =lapply(p2, function(i){ sum(log(i))})
llik2 = unlist(llik2)

df= cbind(df,ll2 = llik2)
ggplot(df)+geom_line(aes(x=theta,y=ll,col="full data"))+geom_line(aes(x=theta,y=ll2,col = "first 6 rows"))


prior = prob(10, x = theta)
ll_theta = llik + log(prior)
df= cbind(df,ll_theta = ll_theta)
ggplot(df)+geom_line(aes(x=theta,y=ll,col="full data"))+geom_line(aes(x=theta,y=ll2,col = "first 6 rows"))+
  geom_line(aes(x=theta,y=ll_theta,col = "bayes"))
