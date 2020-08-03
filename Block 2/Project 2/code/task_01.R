#______________Assignment 1.___________#
library(readxl)
library(mgcv)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggplot2)
library(plotly)
library(hrbrthemes)
library(mgcv)

data = read_excel("E:/Machine Learning/lab 02 block 02/Influenza.xlsx")
#_____(time series plot. 1)______#
# ggplot(data) + geom_line(aes(x=Time,y = Influenza,col="Influenza")) +
#   geom_line(aes(x=Time, y = Mortality,col="Mortality"))+ ylab("Series")+
#   ggtitle("Time - Series Graph")

#____normal ditribution of Mortality____#
# ggplot(data, aes(x=Mortality)) + 
#   geom_histogram(aes(y=..density..), colour="black", fill="white")+
#   geom_density(alpha=.3, fill="#FF6666") 
# gam() model
fit.gam = gam(Mortality ~ s(Week) + Year,data = data,
              family=gaussian(), method="GCV.Cp")
# plot.gam(fit.gam,residuals = TRUE,scheme = 1, pch= 16,cex=0.7)

pred.gam = predict(fit.gam,newdata = data)
data$predictMortality = pred.gam

# ggplot(data)  +
#   geom_line(aes(x=Time, y=Mortality, colour="Original")) +
#   geom_line(aes(x=Time, y=predictMortality,colour = "Predicted")) +
#   ylab("Mortality") 

# histogram of residuals residual
df = data.frame(residuals = resid(fit.gam))
# ggplot(resd.df, aes(x=resd)) + 
#   geom_histogram(aes(y=..density..), colour="black", fill="white",bins = 40)+
#   geom_density(alpha=.3, fill="#FF6666") 

# R SQUARE ERROR
rsq <- function (x, y) cor(x, y) ^ 2
rsq(data$Mortality,pred.gam) *100

penalty = c(0.0001,0.001,0.01,0.1,1,10)
df =length(unique(data$Week))
df_vec = vector()
for (ple in penalty) {
  fit.gam = gam(Mortality ~ Year + s(Week,sp =ple),data = data)
  data[paste0("ple_",format(ple,scientific = F))] = predict(fit.gam,newdata = data)
  data[paste0("dev_",format(ple,scientific = F))] = fit.gam$deviance
  df_vec =c(df_vec, sum(influence(fit.gam)))
}
# Influence of Penalty Factor On The Deviance Of GAM Model
dev_vector = vector()
for (ple in penalty) {
  dev_vector = c(dev_vector, sum(data[paste0("dev_",format(ple,scientific = F))]))
}
dataFrame = data.frame(penalty = penalty , deviance = dev_vector,degreeFreedom = df_vec  )
#ggplot(dataFrame,aes(x= penalty,y = deviance)) + geom_line() +geom_point()

# Influence of dof On The Deviance Of GAM Model

#ggplot(dataFrame,aes(x= penalty,y = degreeFreedom)) + geom_line() +geom_point()
# comparason of different values of penalty

# ggplot(data)  +
#   geom_line(aes(x=Time, y=Mortality,col="Original")) +geom_point(aes(x=Time, y=Mortality,col="Original"))+
#   geom_point(aes(x=Time, y=ple_0.1,colour = "ple 0.1"))+
#   geom_line(aes(x=Time, y=ple_0.0001,color="ple 0.0001"))+
#   geom_point(aes(x=Time, y=ple_10,colour = "ple 10"))+
#   ylab("Mortality") 

# Residuals and influenza values against time

# ggplot(data)  +
#   geom_line(aes(x=Time, y=Influenza,colour="Original")) +
#   geom_line(aes(x=Time, y=fit.gam$residuals,colour = "Residuals")) +
#   ylab("influenza") 

# Modeling Of Mortality As Spline Functions of Year, Week, and Influenza
fit.gam = gam(Mortality ~ s(Year,k = length(unique(data$Year)))  + s(Week,k = length(unique(data$Week))) + s(Influenza,k = length(unique(data$Influenza))) ,data = data)
pred = predict(fit.gam,newdata = data)
data$predictInf = pred
# ggplot(data)  +
#   geom_line(aes(x=Time, y=Mortality,colour="Original")) +
#   geom_line(aes(x=Time, y=predictInf,colour = "Predicted")) +
#   ylab("Mortality")
# 
# plot(fit.gam)
     
     
