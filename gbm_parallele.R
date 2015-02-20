library(foreach)
library(data.table)
library(doMC)
library(gbm)

registerDoMC(6) #change the 2 to your number of CPU cores   
print(date())
distance  <- function(trip){
  sum(sqrt(diff(trip$x,1,1)^2 + diff(trip$y,1,1)^2))
}

rotation  <-  function(path, theta){
  path2  <- path
  path2$x  <- path$x*cos(theta) + path$y*sin(theta)
  path2$y  <- - path$x*sin(theta) + path$y*cos(theta)
  return(path2)
}

rotation0  <-  function(path){
  i  <-  which.max(path$x != 0 | path$y != 0)
  path2  <-  path
  path2$x  <- path$x*path$x[i] + path$y*path$y[i]
  path2$y  <- - path$x*path$y[i+1] + path$y*path$x[i+1]
  j  <-  which.max(path2[i+1:nrow(path)]$y != 0)
  path2$y <- sign(path2$y[i+j])*path2$y
  return(path2)
}

angle  <- function(p1.x,p1.y,p2.x,p2.y,p3.x,p3.y){
  a  <-  c(p1.x - p2.x, p1.y - p2.y)
  b  <-  c(p1.x - p3.x, p1.y - p3.y)
  theta  <-  acos(a %*% b/((a%*%a) *(b%*%b)))
  return(theta) 
}


speedDistribution <- function(trip)
{
  n  <- nrow(trip)  
  vitesse = 3.6*sqrt(diff(trip$x,20,1)^2 + diff(trip$y,20,1)^2)/20
  vitesse[vitesse > 250 ]  <- NA
  vitesse = unlist(lowess(vitesse, f = 1/25,iter = 25)[2])
  acceleration  <-  diff(vitesse,1,1)
  sinuosite <- mapply(FUN = angle, trip$x[-(1:3)], trip$y[-(1:3)], trip$x[-c(2:3,n)],trip$y[-c(2:3,n)], trip$x[-c(3,n-1,n)], trip$y[-c(3,n-1,n)])
  sinuosite[is.na(sinuosite)]  <- 0
  t  <- quantile(vitesse, seq(0.1,1, by = 0.05), na.rm = T)
  t  <- c(t,mean(vitesse, na.rm = T))
  names(t)  <- paste0('v_',names(t))
  t2  <- quantile(acceleration, seq(0.1,1, by = 0.05), na.rm = T)
  t2  <- c(t2,mean(acceleration, na.rm = T))
  names(t2)  <- paste0('a_',names(t2))
  t3  <- quantile(sinuosite, seq(0.1,1, by = 0.05), na.rm = T)
  t3  <- c(t3,mean(sinuosite, na.rm = T))  
  names(t3)  <- paste0('s_',names(t3))
  
  
  return(c(t,t2,t3,n,distance(trip)))
}

drivers = list.files("/home/qboxscientist/Data/Kaggle/Driver Telematics/Data/drivers/")
randomDrivers = sample(drivers, size = 6)

refData = NULL
target = 0
names(target) = "target"
for(driver in randomDrivers)
{
  names(driver) = "drv"  
  dirPath = paste0("/home/qboxscientist/Data/Kaggle/Driver Telematics/Data/drivers/", driver, '/')
  for(i in 1:200)
  {
    trip = read.csv(paste0(dirPath, i, ".csv"))
    features = c(driver,speedDistribution(trip), target)
    refData = rbind(refData, features)
  }
  
}
refData  <-  as.data.table(refData)
for(i in colnames(refData)) refData[[i]] <-  as.numeric(as.character(refData[[i]]))



target = 1
names(target) = "target"
submission = NULL
submission  <- foreach(j =1:length(drivers), .combine = rbind)%dopar%{
  driver  <- drivers[j]
  print(driver)
  dirPath = paste0("/home/qboxscientist/Data/Kaggle/Driver Telematics/Data/drivers/", driver, '/')
  currentData = NULL
#   for(i in 1:200)
#   {
#     trip = read.csv(paste0(dirPath, i, ".csv"))
#     features = c(speedDistribution(trip), target)
#     currentData = rbind(currentData, features)
#   }
#  currentData  <-  as.data.table(currentData)
#  save(currentData, file = paste0("/home/qboxscientist/Data/Kaggle/Driver Telematics/Data/drivers/", driver, "/data.RData"))
  load(paste0("/home/qboxscientist/Data/Kaggle/Driver Telematics/Data/drivers/", driver, "/data.RData"))
  train = rbind(currentData, refData[drv != driver, -1, with = F], use.names = F)
  train = as.data.table(train)
  #g = glm(target ~ ., data=train, family = binomial("logit"))
  train$r1  <-  train$v_/ train$s_
  train$r2  <-  train$s_/ train$v_
  train  <- train[,c(seq(1,19,4),20,seq(21,39,4),40,seq(41,59,4),60,61:65) ,with=F]
  g = gbm(target ~., data=train, distribution = "bernoulli", n.trees = 600, train.fraction = 0.8, shrinkage = .01)
  currentData = as.data.table(currentData)
  currentData$r1  <-  currentData$v_/ train$s_
  currentData$r2  <-  currentData$s_/ train$v_
  p =predict(g, currentData, type = "response")
  labels = sapply(1:200, function(x) paste0(driver,'_', x))
  result = cbind(labels, p)
  result
  #submission = rbind(submission, result)
}
print(date())

colnames(submission) = c("driver_trip","prob")
write.csv(submission, "/home/qboxscientist/Data/Kaggle/Driver Telematics/submission.csv", row.names=F, quote=F)
