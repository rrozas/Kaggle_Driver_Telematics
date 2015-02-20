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


drivers = list.files("Documents/Kaggle/Driver Telematics/Data/drivers/")
randomDrivers = sample(drivers, size = 5)

refData = NULL
target = 0
names(target) = "target"
for(driver in randomDrivers)
{
  dirPath = paste0("Documents/Kaggle/Driver Telematics/Data/drivers/", driver, '/')
  for(i in 1:200)
  {
    trip = read.csv(paste0(dirPath, i, ".csv"))
    features = c(speedDistribution(trip), target)
    refData = rbind(refData, features)
  }
}

target = 1
names(target) = "target"
submission = NULL
for(driver in drivers)
{
  print(driver)
  dirPath = paste0("Documents/Kaggle/Driver Telematics/Data/drivers/", driver, '/')
  currentData = NULL
  for(i in 1:200)
  {
    trip = read.csv(paste0(dirPath, i, ".csv"))
    features = c(speedDistribution(trip), target)
    currentData = rbind(currentData, features)
  }
  train = rbind(currentData, refData)
  train = as.data.frame(train)
  #g = glm(target ~ ., data=train, family = binomial("logit"))
  g = gbm(target ~ ., data=train, distribution = "bernoulli", n.trees = 200, train.fraction = 0.8, shrinkage = .03)
  currentData = as.data.frame(currentData)
  p =predict(g, currentData, type = "response")
  labels = sapply(1:200, function(x) paste0(driver,'_', x))
  result = cbind(labels, p)
  submission = rbind(submission, result)
}

colnames(submission) = c("driver_trip","prob")
write.csv(submission, "Documents/Kaggle/Driver Telematics/submission.csv", row.names=F, quote=F)
