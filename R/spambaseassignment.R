library(XLConnect)
wb = loadWorkbook("data/spambase.xlsx")
data = readWorksheet(wb, sheet = "spambase_data", header = TRUE)

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

#This function should train a knn class probabilities on data and return predicted class
#probabilities on newdata
knearest <- function(data, k, newdata){
  
  
  
  return(predictedclassprobsfornewdata)
}

kknndata <- kknn( Spam ~. , train, test ,k = 5)

str(kknndata)

fitkknndata <- round(fitted(kknndata))
table(test$Spam, fitkknndata)

#summary(kknndata)


