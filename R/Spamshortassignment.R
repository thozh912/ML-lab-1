data <- read.csv2("data/spambaseshort.csv")
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
  coefs <- rep(0,ncol(data))
  for(j in 1:ncol(data)){
    temp <- lm(truespam ~data[,j])
    coefs[j] <- temp$coefficients[1]
  }
  
  set.seed(seed)
  weights <- rnorm(ncol(data))
  output <- rep(0,nrow(data))

  
  for(i in 1:nrow(data)){
    for(j in 1:ncol(data)){
      output[i] <- output[i] + weights[j] * data[i,j] + coefs[j]
    }
    output[i] <- sign(output[i])
  }
  
  for(j in 1:ncol(data)){
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
      for(j in 1:ncol(data)){
        output[i] <- output[i] + weights[j] * data[i,j] + coefs[j]
      }
      output[i] <- sign(output[i])
    }
    
    for(j in 1:ncol(data)){
      for(i in 1:nrow(data)){
        weights[j] <- (truespam[i]- output[i]) * 0.1 * data[i,j] + weights[j]
      }  
    }
    misclass <- 1 - length(which(output == truespam)) / nrow(data)
    counter <- counter + 1
  }
  
  return(list(weights=weights,coefs= coefs,
              misclassification_rate=misclass,counter = counter))
}


simple_percept_exec <- function(testdata,truespam,weights,coefs){
  output <- rep(0,nrow(testdata))
  for(i in 1:nrow(testdata)){
    for(j in 1:ncol(testdata)){
      output[i] <- output[i] + weights[j] * testdata[i,j] + coefs[j]
    }
    output[i] <- sign(output[i])
  }
  misclass <- 1 - length(which(output == truespam)) / nrow(testdata)
  return(paste("Misclassification rate for test data:",signif(misclass,3)))
}

firstlist <- simple_perceptron_classif(train,train_truespam,7235)
simple_percept_exec(test,test_truespam,firstlist$weights,firstlist$coefs)

secondlist <- simple_perceptron_classif(train,train_truespam,846)
simple_percept_exec(test,test_truespam,secondlist$weights,secondlist$coefs)


data <- read.csv2("data/spambaseshort.csv")
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