library(geosphere)
set.seed(1234567890)
stations = read.csv("E:/Machine Learning/lab 03 block 1/stations.csv",fileEncoding = "latin1")
temps = read.csv("E:/Machine Learning/lab 03 block 1/temps50k.csv")
st = merge(stations,temps,by="station_number")

h_distance = 20000
h_date = 12
h_time = 4

# stataion name = Link?ping(	85250)
a = 15.6333
b = 58.4166

# The date to predict
date <- "2013-07-04" 
 
#______Plots______#
  my_position = c(a,b)
  positions = cbind(st$longitude, st$latitude)
  dist_kernal = vector()
  distDiff = c()
  days_kernal = vector()
  dateDiff = vector()
  for (i in 1:nrow(st)) 
  {
    distDiff[i] = abs(distHaversine(my_position,positions[i,]))
    dist_kernal[i] = exp(-(distDiff[i]/h_distance)^2)
    temp = as.numeric(strftime(date,format = "%j")) - 
      as.numeric(strftime(as.Date(st$date[i]),format = "%j"))
    dateDiff[i] = min(abs(temp),365-abs(temp))
    days_kernal[i] = exp(-(dateDiff[i]/h_date)^2)
  }
  time = "12:00:00"
  timeDiff = (as.numeric(as.POSIXct(paste(date, time))) - 
    as.numeric(as.POSIXct(paste(date, st$time))))/3600
  timeDiff = sapply(timeDiff, function(t) {
    min(abs(t), 24 - abs(t))
  })
  time_kernel = sapply(timeDiff / (h_time), function(x) exp(-x ^ 2))

  plot(x = distDiff,y = dist_kernal, xlab='Absolute Haversine Distance',
       ylab='Guassian Distance', main = 'Location')

  plot(x = dateDiff,y = days_kernal, xlab='Absolute Date Difference',
       ylab='Guassian Kernel', main = 'Date')
  
  plot(x = timeDiff,y = time_kernel, xlab='Absolute Time Difference',
       ylab='Guassian Kernel', main = 'Time')
#_______________#

kernal = function(a, b, h_distance=2000, h_date=12, h_time=4, date, times){

#_________distance comparsion_________#
    my_position = c(a,b)
    positions = cbind(st$longitude, st$latitude)
    dist_kernal = vector()
    for (i in 1:nrow(positions)) 
    {
      temp = abs(distHaversine(my_position,positions[i,]))
      dist_kernal[i] = exp(-((temp)^2)/((h_distance^2)))
    }
    #________date comparison________#
    days_kernal = vector()
    for (i in 1:nrow(st)) 
    {
      temp = as.numeric(strftime(date,format = "%j")) - as.numeric(strftime(as.Date(st$date[i]),format = "%j"))
      temp = min(abs(temp),365-abs(temp))
      days_kernal[i] = exp(-((temp)^2)/((h_date^2)))
    }
    #________Time comparison________#
    fit_time = paste(date, times)
    date_time = paste(date, st$time)
    time_kernal = matrix(ncol = length(times),nrow = length(date_time))
    for (i in 1:length(fit_time))
    {
      for (j in 1:length(date_time)) 
      {
        temp = as.numeric(as.POSIXct(fit_time[i]))- as.numeric(as.POSIXct(date_time[j]))
        temp = temp/3600
        temp = min(abs(temp),24-abs(temp))
        time_kernal[j,i] = exp(-((temp)^2)/((h_time^2)))
      }
    }
    sum_kernals = time_kernal + dist_kernal + days_kernal  
    sum_temprature = colSums(sum_kernals*st$air_temperature)/colSums(sum_kernals)
    
    prod_kernals = time_kernal * dist_kernal * days_kernal 
    prod_temprature = colSums(prod_kernals * st$air_temperature)/ colSums(prod_kernals)
    
    df = data.frame(kernal_Add = sum_temprature, 
                    kernal_Mult = prod_temprature)
    return(df)
  }
# stataion name = Link?ping(	85250)
  a = 15.6333
  b = 58.4166
  
# Time to predict
times = c("04:00:00", "06:00:00", "08:00:00", "10:00:00",
            "12:00:00", "14:00:00", "16:00:00", "18:00:00", 
            "20:00:00", "22:00:00", "24:00:00")
#_________Spring_______#
df1 = kernal(a, b, h_distance = 20000, h_date = 12,h_time = 4,date="2001-04-15" , times)
df1$times = as.POSIXct(strptime(times, format="%H:%M:%S"))
 ggplot(df1)+
    geom_line(aes(x = times, y = kernal_Add, col = "add"))+theme_bw()+
    geom_point(aes(x = times, y = kernal_Add))+
    geom_line(aes(x = times,y=kernal_Mult,col = "mult"))+
    geom_point(aes(x = times,y=kernal_Mult))+
    ylab("Predicted temperature")+ggtitle('Spring Forecast')+
  scale_x_datetime( date_labels = "%H:%M:%S",date_breaks = "2 hours")
#_______Summer_________#
  df2 = kernal(a, b, h_distance = 20000, h_date = 12,h_time = 4,date="2005-07-15" , times)
  df2$times = as.POSIXct(strptime(times, format="%H:%M:%S"))
  ggplot(df2)+theme_bw()+
    geom_line(aes(x = times, y = kernal_Add, col = "add"))+
    geom_point(aes(x = times, y = kernal_Add))+
    geom_line(aes(x = times,y=kernal_Mult,col = "mult"))+
    geom_point(aes(x = times,y=kernal_Mult))+
    ylab("Predicted temperature")+ggtitle('Summer Forecast')+
    scale_x_datetime( date_labels = "%H:%M:%S",date_breaks = "2 hours")
#_______Autmn__________#
  df3 = kernal(a, b, h_distance = 20000, h_date = 12,h_time = 4,date="1997-10-15" , times)
  df3$times = as.POSIXct(strptime(times, format="%H:%M:%S"))
  ggplot(df3)+theme_bw()+
    geom_line(aes(x = times, y = kernal_Add, col = "add"))+
    geom_point(aes(x = times, y = kernal_Add))+
    geom_line(aes(x = times,y=kernal_Mult,col = "mult"))+
    geom_point(aes(x = times,y=kernal_Mult))+
    ylab("Predicted temperature")+ggtitle('Autmn Forecast')+
  scale_x_datetime( date_labels = "%H:%M:%S",date_breaks = "2 hours")
#_______Winter________#
  df4 = kernal(a, b, h_distance = 20000, h_date = 12,h_time = 4,date="2002-01-15" , times)
  df4$times = as.POSIXct(strptime(times, format="%H:%M:%S"))
  ggplot(df4)+theme_bw()+
    geom_line(aes(x = times, y = kernal_Add, col = "add"))+
    geom_point(aes(x = times, y = kernal_Add))+
    geom_line(aes(x = times,y=kernal_Mult,col = "mult"))+
    geom_point(aes(x = times,y=kernal_Mult))+
    ylab("Predicted temperature")+ggtitle('Winter Forecast')+
    scale_x_datetime( date_labels = "%H:%M:%S",date_breaks = "2 hours")
  