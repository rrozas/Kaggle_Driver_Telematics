ptime <- system.time({
   r <- foreach(icount(trials), .combine=cbind) %dopar% {
     ind <- sample(100, 100, replace=TRUE)
     result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
     coefficients(result1)
     }
   })

library(data.table)
library(stringr)
library(foreach)  
library(doMC)  
registerDoMC(2) #change the 2 to your number of CPU cores   


dir = 'Documents/Kaggle/Driver Telematics/Data/drivers/12/'
#dir.create(paste0(dir,'/png/'))
data  <- NULL
for( file in list.files(dir) ){
  print(file)
  path  <- fread(paste0(dir,file))
  pngfile  <-  str_replace(file,".csv",".png")
  #path  <-  rotation0(path)
  data <- rbind(data,speedDistribution(path))
  #qplot(data=path, x = x, y=y)
  #ggsave(paste0(dir,'/png/',pngfile))
}
data  <- as.data.table(data)
plot(data[['s_']]/data[['v_']] )


ex_1_1  <- fread('Documents/Kaggle/Driver Telematics/Data/drivers/1/1.csv')
v  <- sqrt(rowSums((ex_1_1[-1] - ex_1_1[-862])^2))
a  <- v[-1] - v[-862]
plot(v)
plot(a)

ex_1_2  <- fread('Documents/Kaggle/Driver Telematics/Data/drivers/1/2.csv')
v  <- sqrt(rowSums((ex_1_2[-1] - ex_1_2[-561])^2))
a  <- v[-1] - v[-561]
plot(v)
plot(a)

ex_1_3  <- fread('Documents/Kaggle/Driver Telematics/Data/drivers/1/3.csv')
v  <- sqrt(rowSums((ex_1_3[-1] - ex_1_3[-931])^2))
a  <- v[-1] - v[-931]
plot(v)
plot(a)
