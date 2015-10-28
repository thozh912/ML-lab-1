library(XLConnect)
wb2 = loadWorkbook("data/machines.xlsx")
data2 = readWorksheet(wb2, sheet = "machines", header = TRUE)


#Length is exp(theta) distributed. 

#From wikipedia, for i.i.d. Lengths
#likelihood is \prod_{i=1}^n \theta \exp{-\theta x_{i}}
# = \theta^{n} \exp{-\theta n \bar{x}}

# where \bar{x} is the sample mean of lengths x_{i}.

#loglikelihood is n \ln{\theta} - \theta n \bar{x}
#which has maximum when \theta = \dfrac{1}{\bar{x}}

loglik <- function(theta,x){
  xmean <- mean(x)
  n <- length(x)
  loglik <- n * log(theta) - theta * n * xmean
  return(loglik)
}

theta <- exp(seq(from = -2, to = 2, by = 0.05))

plot(theta,loglik(theta,data2[,1]),type="l",xlab="theta",ylab="Log-likelihood",
     ylim=c(-80,10),main="Log-Likelihood function",log="x")

paste0("Maximum loglikelihood: ",round(max(loglik(theta,data2[,1])),3),
       " at theta = ",round(theta[which.max(loglik(theta,data2[,1]))],3))

lines(theta,loglik(theta,data2[1:6,1]),col="red")
legend("topleft",c("all obs.","first six obs."),
       lty=c(1,1),
       lwd=c(2.5,2.5),col=c("black","red"))

paste0("Maximum loglikelihood: ",round(max(loglik(theta,data2[1:6,1])),3),
       " at theta = ",round(theta[which.max(loglik(theta,data2[1:6,1]))],3))

#\ln{\left(p(x | \theta) p(\theta)\right)} computes
#the logarithm of the relative A posteriori probabilities p(\theta | x)

#Assuming x_{i} are i.i.d

logposterior <- function(theta){
  lambda <- 0.5
  x <- data2[,1]
  n <- length(x)
  pp <- (theta)^n * lambda * exp(-theta * (sum(x) + lambda))
  logposterior <- log(pp)
  return(logposterior)
}

plot(theta,logposterior(theta),type="l",xlab="theta",ylab="Log-posterior",
     ylim=c(-80,10),main="Log posterior function",log="x")

paste0("Maximum logposterior: ",round(max(logposterior(theta)),3),
       " at theta = ",round(theta[which.max(logposterior(theta))],3))

# We choose to use theta is 1.105

thetavalue <- 1.105

set.seed(12345)
draws <-rexp(50, thetavalue)

par(mfrow=c(1,2))
hist(data2[,1],breaks=12, main="Original data",xlab="machine lifetime")
hist(draws, breaks=12, main ="Simulated data",xlab="simulated machine lifetime")
par(mfrow=c(1,1))