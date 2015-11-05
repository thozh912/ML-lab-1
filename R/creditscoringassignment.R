library(XLConnect)
library(tree)
library(e1071)
wb = loadWorkbook("data/creditscoring.xls")
data = readWorksheet(wb, sheet = "credit", header = TRUE)
data$good_bad <- as.factor(data$good_bad)
head(data)
n <- dim(data)[1]

rows <- 1:n
test_index <-list()
set.seed(12345)
for(i in 1:4){
  test_index[[i]] <- sample(rows,250)
  
  rows <- setdiff(rows,test_index[[i]])
}

train=data[c(test_index[[1]],test_index[[2]]),]
valid=data[test_index[[3]],]
test = data[test_index[[4]],]
ntrain = dim(train)[1]
nvalid = dim(valid)[1]
ntest = dim(test)[1]

fittree1 <- tree(good_bad ~., data = train, split = "deviance")
fittree2 <- tree(good_bad ~., data = train, split = "gini")

plot(fittree1)
text(fittree1,pretty=5)

plot(fittree2, type="uniform")
text(fittree2,pretty=5)

trainfit1= predict(fittree1, newdata=train, type="class")
testfit1 = predict(fittree1, newdata=test, type="class")

trainfit2= predict(fittree2, newdata=train, type="class")
testfit2 = predict(fittree2, newdata=test, type="class")

count_true <- function(a,b){
  counttrue <- 0
  for( i in 1:length(a)){
    if(a[i] == b[i]){
      counttrue <- counttrue + 1
    }
  }
  return(counttrue / length(a))
}

trainproptrue1 <- count_true(trainfit1,train$good_bad)
testproptrue1 <- count_true(testfit1,test$good_bad)

trainproptrue2 <- count_true(trainfit2,train$good_bad)
testproptrue2 <- count_true(testfit2,test$good_bad)

#fittree1 performs better than fittree2 on both training and test data.
#We choose to work with the split = "deviance"

paste("Misclassification rate of fittree1 for training data: ",1-trainproptrue1)
paste("Misclassification rate of fittree1 for test data: ",1-testproptrue1)
paste("Misclassification rate of fittree2 for training data: ",1-trainproptrue2)
paste("Misclassification rate of fittree2 for test data: ",1-testproptrue2)


# table(train$good_bad,trainfit1)
# table(test$good_bad,testfit1)
# table(train$good_bad,trainfit2)
# table(test$good_bad,testfit2)

#pruning fittree1

trainScore=rep(0,16)
testScore=rep(0,16)
for(i in 2:16) {
  prunedTree=prune.tree(fittree1,best=i)
  pred=predict(prunedTree, newdata=valid, type="tree")
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
}
plot(2:16, trainScore[2:16], type="b", col="red",
     main="Deviance of best pruned tree against no. of terminal nodes",
     xlab = "no. of terminal nodes",ylab ="Deviance",ylim=c(250,600))
points(2:16, testScore[2:16], type = "b" , col="blue")
legend("topright",c("training data","validation data"),
       lty=c(1,1),
       lwd=c(2.5,2.5),col=c("red","blue"))

#best tree appears to be size 8
# besttree <- prune.tree(fittree1,best=8)
# plot(besttree)
# text(besttree,pretty=5)
#this tree could just as well be size 5...deviance measure makes it misleading
besttree <- prune.tree(fittree1,best=5)
plot(besttree)
text(besttree,pretty=5)

#It seems having long duration loans, low levels of savings and being young 
#makes one a bad loan taker, according to this tree (which it must be said is 
#quite simplistic a view, since many "bad" are misclassified as "good")

predtestbest <- predict(besttree, newdata=test, type="class")
testbestproptrue <- count_true(predtestbest,test$good_bad)
paste("Misclassification rate of besttree for test data: ",1-testbestproptrue)
#table(test$good_bad,predtestbest)

nbayesmodel <- naiveBayes(good_bad~.,data=train)

nbayespredtrain<- predict(nbayesmodel, train)
nbayespredtest<- predict(nbayesmodel, test)

table(train$good_bad,nbayespredtrain)
table(test$good_bad,nbayespredtest)

nbayestrainproptrue <- count_true(nbayespredtrain,train$good_bad)
nbayestestproptrue <- count_true(nbayespredtest,test$good_bad)

paste("Misclassification rate of nbayesmodel for training data: ",1-nbayestrainproptrue)
paste("Misclassification rate of nbayesmodel for test data: ",1-nbayestestproptrue)
#Naive bayes classifies worse than the best decision tree, and unpruned tree.

raws <- data.frame(predict(nbayesmodel, newdata=train, type="raw"))

preds <- c()
for (i in 1:ntrain){
  if((raws[i,1]/raws[i,2]) > 0.1){
    preds[i] = "bad"
  }else{
    preds[i] ="good"
  }  
}

table(train$good_bad, preds)
lossmatrnbayestrainproptrue <- count_true(preds,train$good_bad)
paste("Misclassification rate of nbayesmodel with loss matrix for training data: ",
      1-lossmatrnbayestrainproptrue)

rawTest <- predict(nbayesmodel, newdata=test, type="raw")
predtests <- c()
for (i in 1:ntest){
  if((rawTest[i,1]/rawTest[i,2]) > 0.1){
    predtests[i] = "bad"
  }else{
    predtests[i] ="good"
  }  
}

table(test$good_bad, predtests)
lossmatrnbayestestproptrue <- count_true(predtests,train$good_bad)
paste("Misclassification rate of nbayesmodel with loss matrix for test data: ",
      1-lossmatrnbayestestproptrue)