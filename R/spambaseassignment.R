library(XLConnect)
library(kknn)

wb = loadWorkbook("data/spambase.xlsx")
data = readWorksheet(wb, sheet = "spambase_data", header = TRUE)

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]


knearest <- function(data, k, newdata){
  
  dataspamindicator <- data$Spam
  data <- as.matrix(data)
  data <- data[,-ncol(data),drop=FALSE]


  # find out which data rows are the zero vector
  datazerorows <-rep(TRUE,nrow(data))
  for(g in 1:nrow(data)){
    if(sum(data[g,]) == 0){
      datazerorows[g] <- FALSE
    }  
  }
  
  data <- data[datazerorows,]
  dataspamindicator <- dataspamindicator[datazerorows]
  
  newdata <- as.matrix(newdata)
  newdata <- newdata[,-ncol(newdata),drop=FALSE]
  
  # find out which newdata rows are the zero vector
  newdatazerorows <-rep(TRUE,nrow(newdata))
  for(p in 1:nrow(newdata)){
    if(sum(newdata[p,]) == 0){
      newdatazerorows[p] <- FALSE
    }  
  }

  newdata <- newdata[newdatazerorows,]

  nearness <- matrix(0,k, as.numeric(nrow(newdata)))
  
  for(j in 1:nrow(newdata)){
    distancesfromj <- rep(0,nrow(data))
    for(i in 1:nrow(data)){
      normalizing <- sqrt(sum(data[i,]^2)) * sqrt(sum(newdata[j,]^2)) 
      result <- newdata[j,] %*% data[i,] / normalizing
      dist <- 1 - result
      
      distancesfromj[i] <- dist
    }
    distancesfromj <- sort(distancesfromj, index.return = TRUE)
    nearness[,j] <- distancesfromj$ix[1:k]
  }
  
  nearness <- t(nearness)
  predictedclassprobsfornewdata <- rep(0,nrow(nearness))
  for(m in 1:nrow(nearness)){
    predictedclassprobsfornewdata[m] <- sum(dataspamindicator[nearness[m,1:k]]) / k
  }
  
  return(list(predicted = predictedclassprobsfornewdata, deletedrows = newdatazerorows))
}


#summarizing results with decision line p=0.5

knn5 <- knearest(train,5,test)



knn1 <- knearest(train,1,test)

tabularize<-function(knn,decider,newdata){
  classified_by_knn <- rep(0,length(knn$predicted))
  for(u in 1:length(knn$predicted)){
    if(knn$predicted[u] > decider){
      classified_by_knn[u] <- 1
    }
  }
  testdataspam <-newdata$Spam[knn$deletedrows]
  
  tab <- table(Truth = testdataspam,classified_by_knn)
  return(tab)  
}

tab <- tabularize(knn5,.5,test)
print(tab)
paste("Misclassification rate:",round(1-sum(diag(tab))/sum(tab),4),sep=" ")

tab <- tabularize(knn1,.5,test)
print(tab)
paste("Misclassification rate:",round(1-sum(diag(tab))/sum(tab),4),sep=" ")

kknndata <- kknn( Spam ~. , train, test ,k = 5)

#str(kknndata)

tabularizekknn<-function(kknndata,decider){
  classified_by_kknn <- rep(0,length(kknndata$fitted.values))
  for(v in 1:length(kknndata$fitted.values)){
    if(kknndata$fitted.values[v] > decider){
      classified_by_kknn[v] <- 1
      }
  }
  
  tab <- table(Truth = test$Spam,classified_by_kknn)
  return(tab)
}

tab <- tabularizekknn(kknndata,0.5)
print(tab)
paste("Misclassification rate:",round(1-sum(diag(tab))/sum(tab),4),sep=" ")

#summary(kknndata)

#ROC is TPR tp/(tp +fn) against FPR fp/(fp + tn) FPR = 1 - Specificity
#Sensitivity is TPR, specificity is TNR tn/(tn + fp)

pi <- seq(0.05, 0.95, by=0.05)




wb2 = loadWorkbook("data/machines.xlsx")
data2 = readWorksheet(wb2, sheet = "machines", header = TRUE)

#Length is exp(theta) distributed. 

#From wikipedia, for i.i.d. Lengths
#likelihood is \prod_{i=1}^n \theta \exp{-\theta x_{i}}
# = \theta^{n} \exp{-\theta n \bar{x}}

# where \bar{x} is the sample mean of lengths x_{i}.

#loglikelihood is n \ln{\theta} - \theta n \bar{x}
#which has maximum when \theta = \dfrac{1}{\bar{x}}



#\ln{\left(p(x | \theta) p(\theta)\right)} computes
#the logarithm of the relative A posteriori probabilities p(\theta | x)

