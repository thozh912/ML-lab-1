library(XLConnect)
library(fANCOVA)
library(kernlab)
#FROM THIS FILE LOCATION, EXCEL FILES SHOULD BE FOUND IN A SUBFOLDER IN THIS FILE LOCATION CALLED DATA
wb = loadWorkbook("C:/Users/Dator/Documents/R_HW/ML-lab-1/data/mortality_rate.xls")
data = readWorksheet(wb, sheet = 1, header = TRUE)
LMR <- log(data$Rate)
data[,3] <- LMR
names(data)[3] <- "LMR"
#head(data)

epanechnikov <- function(x_0,x,lambda){
  if(abs(x_0-x)/lambda < 1){
    res <- 3/4 *  (1- 1/(lambda)^2 *(x_0 - x)^2)
  } else{
    res <- 0
  }
  return(res)
}

NW_ker_smooth <- function(X,Y,Xtest,lambda){
  kersum <- matrix(0,length(Xtest),length(X))
  for(i in 1:length(Xtest)){
    for(j in 1:length(X)){
      kercomp <- epanechnikov(Xtest[i],X[j],lambda)
      kersum[i,j] <- kercomp
    }
  }
  kersum_rowsums <- rowSums(kersum)
  #print(kersum_rowsums)
  pred <-c()
  for(i in 1:length(Xtest)){
    pred[i] <- (kersum[i,] %*% Y) /(kersum_rowsums[i])
  }
  return(pred)
}
Xtest <- seq(from=1,to=171, by=1)
plot(data$Day,data$LMR,main="Log of Fruitfly mortality rate vs day number")
#in order, wiggly (without producing NaNs),smooth and reasonable

sm1 <- NW_ker_smooth(data$Day,data$LMR,Xtest,4)
plot(data$Day,data$LMR,main=c("Log of Fruitfly mortality rate vs day number",
                              "fitted by N-W kernel smoother",
                              "with Epanechnikov kernel"))
lines(sm1)
legend("bottom",legend = expression(lambda == 4),bty="n")
sm3 <- NW_ker_smooth(data$Day,data$LMR,Xtest,50)
plot(data$Day,data$LMR,main=c("Log of Fruitfly mortality rate vs day number",
                              "fitted by N-W kernel smoother",
                              "with Epanechnikov kernel"))
lines(sm3)
legend("bottom",legend = expression(lambda == 50),bty="n")
sm4 <- NW_ker_smooth(data$Day,data$LMR,Xtest,10)
plot(data$Day,data$LMR,main=c("Log of Fruitfly mortality rate vs day number",
                              "fitted by N-W kernel smoother",
                              "with Epanechnikov kernel"))
lines(sm4)
legend("bottom",legend = expression(lambda == 10),bty="n")
sm5 <- NW_ker_smooth(data$Day,data$LMR,data$Day,10)
bestmse <- sum((data$LMR-sm5)^2)/(length(data$LMR))
paste("MSE for training data for most reasonable model:",signif(bestmse,3))

set.seed(12345)
ksvmepsregr <- ksvm(LMR ~ Day,data=data,type="eps-svr",kernel="rbfdot",epsilon=0.1)
set.seed(12345)
ksvmepsregr1 <- ksvm(LMR ~ Day,data=data,type="eps-svr",kernel="rbfdot",epsilon=0.005)
set.seed(12345)
ksvmepsregr2 <- ksvm(LMR ~ Day,data=data,type="eps-svr",kernel="rbfdot",epsilon=3)
plot(data$Day,data$LMR,main=c("Log of Fruitfly mortality rate vs day number",
                              "ksvm eps-svr fit with epsilon = 0.1"))
lines(data$Day,predict(ksvmepsregr,newdata=data))
plot(data$Day,data$LMR,main=c("Log of Fruitfly mortality rate vs day number",
                              "ksvm eps-svr fit with epsilon = 0.005"))
lines(data$Day,predict(ksvmepsregr1,newdata=data))
plot(data$Day,data$LMR,main=c("Log of Fruitfly mortality rate vs day number",
                              "ksvm eps-svr fit with epsilon = 3"))
lines(data$Day,predict(ksvmepsregr2,newdata=data))
paste("MSE for training data for most reasonable ksvm eps-svr model:",signif(ksvmepsregr1@error))

#Residual Standard Error:0.316
loessfit <- loess.as(data$Day,data$LMR,criterion="gcv",plot=TRUE,
                     main=c("Log of Fruitfly mortality rate vs day number",
                            "loess.as fit with 95% confidence bands"))
pl <- predict(loessfit,se=TRUE)
lines(data$Day,predict(loessfit,data$Day)-2*pl$se.fit,lty=2)
lines(data$Day,predict(loessfit,data$Day)+2*pl$se.fit,lty=2)

