library(XLConnect)
library(kknn)

wb = loadWorkbook("data/spambase.xlsx")
data = readWorksheet(wb, sheet = "spambase_data", header = TRUE)

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

#This function should train a knn class probabilities on data and return predicted class
#probabilities on newdata

distance <- function(vector1,vector2){
  normalizing <- length(vector1)-1
  
  vec1 <- as.vector(scale(vector1))
  vec2 <- as.vector(scale(vector2))
  
  result <- vec1 %*% vec2 / normalizing
    
  dist <- 1 - result
  
  return(dist)
}

knearest <- function(data, k, newdata){
  
  
  for(i in 1:nrow(data)){
    for(j in 1:nrow(newdata)){
      distance
    }
  }
  
  return(predictedclassprobsfornewdata)
}

kknndata <- kknn( Spam ~. , train, test ,k = 5)

str(kknndata)

fitkknndata <- round(fitted(kknndata))
table(test$Spam, fitkknndata)

#summary(kknndata)

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

