crabdata <- read.csv("data/australian-crabs.csv")

n <- dim(crabdata)[1]
plot(crabdata$RW,crabdata$CL,pch="+",
     col=ifelse(as.numeric(crabdata$sex) == 1, "red","blue"),
     main="crab Carapace Length against Rear Width",ylab ="Carapace Length",
     xlab="Rear Width")
legend("topleft",legend = levels(crabdata$sex),
                              pch = "+",col=c("red","blue"))


#Yes it will be easy because the two sexes form different clusters.



Male <- rep(FALSE,n)

for(i in 1:n){
  if(as.numeric(crabdata$sex[i]) == 2){
    Male[i] <- TRUE 
  }
}

Female <- !Male

Male <- as.numeric(Male)
Female <- as.numeric(Female)

sexindicator <- cbind(Male,Female)

logisticreg <- glm(sexindicator ~ CL + RW, family = binomial(), data = crabdata)
