library(XLConnect)
library(kknn)

#FROM THIS FILE LOCATION, EXCEL FILES SHOULD BE FOUND IN A SUBFOLDER IN THIS FILE LOCATION CALLED DATA
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
  truth <- test$Spam
  tab <- table(Truth = truth,classified_by_kknn)
  return(tab)
}


tab <- tabularizekknn(kknndata,0.5)
print(tab)
paste("Misclassification rate:",round(1-sum(diag(tab))/sum(tab),4),sep=" ")

#summary(kknndata)

#ROC is TPR tp/(tp +fn) against FPR fp/(fp + tn) FPR = 1 - Specificity
#Sensitivity is TPR, specificity is TNR tn/(tn + fp)

pi <- seq(0.05, 0.95, by=0.05)

collect<-function(pi,knn,kknndata,newdata){
    collector <-matrix(0,19,4)
    for(b in 1:length(pi)){
      target <- tabularize(knn,pi[b],newdata)
      target2 <- tabularizekknn(kknndata,pi[b])
      collector[b,1:2] <- c(target[2,2] / ( target[2,2] + target[2,1]) , target[1,1] / (target[1,1] + target[1,2]))
      collector[b,3:4] <- c(target2[2,2] / ( target2[2,2] + target2[2,1]) , target2[1,1] / (target2[1,1] + target2[1,2]))
      
    }
    return(collector)
}

plotthis <- collect(pi,knn5,kknndata,test)


kknn_ROC <- data.frame(cbind(1 - plotthis[,4], plotthis[,3]))
knearest_ROC <- data.frame(cbind(1 - plotthis[,2], plotthis[,1]))

plot(kknn_ROC, type="l", col="blue", xlim=c(0.02,0.3), ylim=c(0.6,0.97),
     xlab="1-Specificity (false positive rate)", ylab="Sensitivity (True positive rate)",
     main="ROC curve for kknn and knearest function")
lines(knearest_ROC, type="l", col="red")
legend(0.15,0.9,c("kknn","knearest"),
       lty=c(1,1),
       lwd=c(2.5,2.5),col=c("blue","red"))




