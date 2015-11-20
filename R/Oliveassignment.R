library(kernlab)
olivedata <- read.csv(paste0(getwd(),"/data/olive.csv"))
R2 <- as.factor(olivedata$Region == 2)
olivedata[,12] <- R2
names(olivedata)[12] <- "R2"
n <- dim(olivedata)[1]
head(olivedata)

plot(olivedata$linoleic, olivedata$oleic,pch="+",
     col=ifelse(olivedata$R2==TRUE, "red", "black"),
     main="Oleic vs Linoleic acid content of olive oils")
legend("topright",legend = c("Region 2","Not Region 2"),
       pch = "+",col=c("red","black"))

nSupVec <- function(ksvmobj){
  return(ksvmobj@nSV)
}

misclassrate <- function(truevec,retvec){
  counter <- 0
  for(i in 1:length(truevec)){
    if(truevec[i] != retvec[i]){
      counter <- counter + 1
    }
  }
  return(signif(counter/length(truevec),3))
}

set.seed(12345)
linksvm <-ksvm(R2~ oleic + linoleic,olivedata,kernel="vanilladot",kpar=list())
plot(olivedata$linoleic, olivedata$oleic,pch="+",
     col=ifelse(linksvm@fitted==TRUE, "red", "black"),
     main=c("Oleic vs Linoleic acid content of olive oils",
            "Region classification fit by linear kernel SVM"))
legend("topright",legend = c("Region 2","Not Region 2"),
       pch = "+",col=c("red","black"))
paste("Number of support vectors for linear kernel SVM C-svc:",nSupVec(linksvm))
paste("Misclassification rate for linear kernel SVM:",misclassrate(R2,linksvm@fitted))

set.seed(12345)
sigma <- signif(sigest(R2~ oleic + linoleic,olivedata)[2],3)

set.seed(12345)
rbfksvm <-ksvm(R2~ oleic + linoleic,olivedata,kernel="rbfdot")
plot(olivedata$linoleic, olivedata$oleic,pch="+",
     col=ifelse(rbfksvm@fitted==TRUE, "red", "black"),
     main=c("Oleic vs Linoleic acid content of olive oils",
            "Region classification fit by Radial Basis kernel SVM",
            paste("with sigma =",sigma,
                  "and C = 1")))
legend("topright",legend = c("Region 2","Not Region 2"),
       pch = "+",col=c("red","black"))
paste("Number of support vectors for Radial Basis kernel SVM C-svc:",
      "with sigma =",sigma,
      "and C = 1:",nSupVec(rbfksvm))
paste("Misclassification rate for Radial Basis kernel SVM:",
      "with sigma =",sigma,
      "and C = 1:",misclassrate(R2,rbfksvm@fitted))

set.seed(12345)
rbfksvm1 <-ksvm(R2~ oleic + linoleic,olivedata,kernel="rbfdot", C=100)
plot(olivedata$linoleic, olivedata$oleic,pch="+",
     col=ifelse(rbfksvm1@fitted==TRUE, "red", "black"),
     main=c("Oleic vs Linoleic acid content of olive oils",
            "Region classification fit by Radial Basis kernel SVM",
            paste("with sigma =",sigma,
                  "and C = 100")))
legend("topright",legend = c("Region 2","Not Region 2"),
       pch = "+",col=c("red","black"))
paste("Number of support vectors for Radial Basis kernel SVM C-svc",
      "with sigma =",sigma,
      "and C = 100:",nSupVec(rbfksvm1))
paste("Misclassification rate for Radial Basis kernel SVM",
      "with sigma =",sigma,
      "and C = 100:",misclassrate(R2,rbfksvm1@fitted))

set.seed(12345)
rbfksvm2 <-ksvm(R2~ oleic + linoleic,olivedata,kernel="rbfdot",kpar=list(sigma=10))
plot(olivedata$linoleic, olivedata$oleic,pch="+",
     col=ifelse(rbfksvm2@fitted==TRUE, "red", "black"),
     main=c("Oleic vs Linoleic acid content of olive oils",
            "Region classification fit by Radial Basis kernel SVM",
            paste("with sigma = 10 and C = 1")))
legend("topright",legend = c("Region 2","Not Region 2"),
       pch = "+",col=c("red","black"))
paste("Number of support vectors for Radial Basis kernel SVM C-svc with sigma = 10 and C = 1:",
      nSupVec(rbfksvm2))
paste("Misclassification rate for Radial Basis kernel SVM with sigma = 10 and C = 1:",
      misclassrate(R2,rbfksvm2@fitted))

olivedata2 <- cbind(olivedata[,2],olivedata[,4:11])
names(olivedata2)[1] <-"Region"
set.seed(12345)
allacidsksvm <- ksvm(Region~.,data=olivedata2,type="spoc-svc",kernel="vanilladot",cross=10)

paste("Number of support vectors for 10-fold cross-validated",
      "multi-class ksvm() spoc-svc with linear kernel:",
      nSupVec(allacidsksvm))
paste("Misclassification rate for 10-fold cross-validated",
      "multi-class ksvm() spoc-svc with linear kernel:",
      misclassrate(olivedata2$Region,allacidsksvm@fitted))
paste("Cross validation score:",signif(allacidsksvm@cross,3))