data <- read.csv2("C:/Users/Dator/Documents/R_HW/ML-lab-1/data/spambaseshort.csv")
truespam <- data[,ncol(data)]
data <- data[,-ncol(data)]
data <- scale(data)

set.seed(12345)
index <- sample(1:nrow(data),floor(0.7 * nrow(data)))
train <- data[index,]
test <- data[-index,]
train_truespam <- truespam[index]
test_truespam <- truespam[-index]

simple_perceptron_classif <- function(data,truespam,seed){

  data <- cbind(data,rep(1,nrow(data)))
  set.seed(seed)
  weights <- rnorm(ncol(data))
  output <- rep(0,nrow(data))

  
  for(i in 1:nrow(data)){
    for(j in 1:(ncol(data))){
      output[i] <- output[i] + weights[j] * data[i,j]
    }
    output[i] <- sign(output[i])
  }
  
  for(j in 1:(ncol(data))){
    for(i in 1:nrow(data)){
      weights[j] <- (truespam[i]- output[i]) * 0.1 * data[i,j] + weights[j]
    }  
  }
  misclass <- 1 - length(which(output == truespam)) / nrow(data)
  prevmisclass <- 1
  counter <- 1
  
  

  while(prevmisclass - misclass > 0.01){
    prevmisclass <- misclass
    for(i in 1:nrow(data)){
      for(j in 1:(ncol(data))){
        output[i] <- output[i] + weights[j] * data[i,j]
      }
      output[i] <- sign(output[i])
    }
    
    for(j in 1:(ncol(data))){
      for(i in 1:nrow(data)){
        weights[j] <- (truespam[i]- output[i]) * 0.1 * data[i,j] + weights[j]
      }  
    }
    misclass <- 1 - length(which(output == truespam)) / nrow(data)
    counter <- counter + 1
  }
  
  return(list(weights=weights,
              misclassification_rate=misclass,counter = counter))
}


simple_percept_exec <- function(testdata,truespam,weights){
  testdata <- cbind(testdata,rep(1,nrow(testdata)))
  output <- rep(0,nrow(testdata))
  for(i in 1:nrow(testdata)){
    for(j in 1:ncol(testdata)){
      output[i] <- output[i] + weights[j] * testdata[i,j]
    }
    output[i] <- sign(output[i])
  }
  misclass <- 1 - length(which(output == truespam)) / nrow(testdata)
  print(table(truespam,output))
  return(paste("Misclassification rate for test data:",signif(misclass,3)))
}

firstlist <- simple_perceptron_classif(train,train_truespam,7235)
simple_percept_exec(test,test_truespam,firstlist$weights)

secondlist <- simple_perceptron_classif(train,train_truespam,846)
simple_percept_exec(test,test_truespam,secondlist$weights)
data <- read.csv2("C:/Users/Dator/Documents/R_HW/ML-lab-1/data/spambaseshort.csv")
data$Spam[data$Spam < 0] <- 0
set.seed(12345)
index <- sample(1:nrow(data),floor(0.7 * nrow(data)))
train <- data[index,]
test <- data[-index,]
testspam <- test[,ncol(test)]
testspam[testspam == 0] <- -1
test <- test[,-ncol(test)]


logisticreg <- glm(Spam ~., family = binomial(link="logit"), data = train)
testresult <- predict(logisticreg,test)
testresult <- sign(testresult)
table(testspam,testresult)
paste("Logistic regression misclassification rate:",
      signif(1 - length(which(testresult == testspam)) / length(testspam),3))
library(neuralnet)
mort <- read.csv2("C:/Users/Dator/Documents/R_HW/ML-lab-1/data/mortality.csv")
stdday <- scale(mort[,1])
mort[,1] <- stdday
plot(LMR ~ Day, data = mort, type="l",col="red",lwd=2,
     main="Log mortality rate of fruit flies vs standardized # days",
     xlab="standardized # Days")
set.seed(7235)
model1 <- neuralnet(LMR ~ Day, data = mort, hidden = 1,
                    act.fct = "tanh", stepmax = 1e6, threshold = 0.1)

set.seed(7235)
model2 <- neuralnet(LMR ~ Day, data = mort, hidden = 2,
                    act.fct = "tanh", stepmax = 1e6, threshold = 0.1)

set.seed(7235)
model3 <- neuralnet(LMR ~ Day, data = mort, hidden = 4,
                    act.fct = "tanh", stepmax = 1e6, threshold = 0.1)

set.seed(7235)
model4 <- neuralnet(LMR ~ Day, data = mort, hidden = 5,
                    act.fct = "tanh", stepmax = 1e6, threshold = 0.1)

plot(LMR ~ Day, data = mort, type="l",col="red",lwd=2,
     main="One hidden node neuralnet fit",
     xlab="standardized # Days") 
points(x=mort$Day, unlist(model1$net.result), col="darkorange", type="l", 
       lwd=2) 
legend("bottomright",legend = c("training data","neuralnet fit"),
       lty = c(1,1),col=c("red","darkorange"))
plot(LMR ~ Day, data = mort, type="l",col="red",lwd=2,
     main="Two hidden nodes neuralnet fit",
     xlab="standardized # Days") 
points(x=mort$Day, unlist(model2$net.result), col="darkorange", type="l", 
       lwd=2) 
legend("bottomright",legend = c("training data","neuralnet fit"),
       lty = c(1,1),col=c("red","darkorange"))
plot(LMR ~ Day, data = mort, type="l",col="red",lwd=2,
     main="Four hidden nodes neuralnet fit",
     xlab="standardized # Days") 
points(x=mort$Day, unlist(model3$net.result), col="darkorange", type="l", 
       lwd=2) 
legend("bottomright",legend = c("training data","neuralnet fit"),
       lty = c(1,1),col=c("red","darkorange"))
plot(LMR ~ Day, data = mort, type="l",col="red",lwd=2,
     main="Five hidden nodes neuralnet fit",
     xlab="standardized # Days") 
points(x=mort$Day, unlist(model4$net.result), col="darkorange", type="l", 
       lwd=2) 
legend("bottomright",legend = c("training data","neuralnet fit"),
       lty = c(1,1),col=c("red","darkorange"))
## NA
