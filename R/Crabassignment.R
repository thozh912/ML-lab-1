crabdata <- read.csv("data/australian-crabs.csv")

n <- dim(crabdata)[1]
plot(crabdata$RW,crabdata$CL,pch="+",
     col=ifelse(as.numeric(crabdata$sex) == 1, "red","blue"),
     main="crab Carapace Length against Rear Width",ylab ="Carapace Length",
     xlab="Rear Width")
legend("topleft",legend = levels(crabdata$sex),
                              pch = "+",col=c("red","blue"))


#Yes it will be easy because the two sexes form different clusters.
#Except at smaller crabs where it will not be easy


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

#coef RW / CL = -2.71
#slope = 2.713, intercept = -2.940

logisticreg <- glm(sex ~ CL + RW, family = binomial(), data = crabdata)

logregclassifier <- rep(FALSE,n)
for(l in 1:n){
  if(logisticreg$fitted.values[l] > 0.5){
    logregclassifier[l] <- TRUE
  }
}

plot(crabdata$RW,crabdata$CL,pch="+",
     col=ifelse(!logregclassifier, "red","blue"),
     main=c("Logistic regression classified sex based on features",
            "Carapace Length and Rear Width"),ylab ="Carapace Length",
     xlab="Rear Width")

y <- c()
for( x in seq(from=5,to=22, by= 0.1)){
  y <- c(y,2.713 * x - 2.940)
}
lines(seq(from=5,to=22, by= 0.1),y)
legend("topleft",legend = levels(crabdata$sex),
       pch = "+",col=c("red","blue"))

LDA <- function(df){
  Male <- rep(FALSE,n)
  
  for(i in 1:n){
    if(as.numeric(df$sex[i]) == 2){
      Male[i] <- TRUE 
    }
  }
  Female <- !Male
  Maleclass <- df[Male,5:6]
  nMale <- nrow(Maleclass)
  Maleproportion <- nMale / n
  Femaleclass <- df[Female,5:6]
  nFemale <- n - nMale
  Femaleproportion <- 1 - Maleproportion
  
  Malecenter <- 1/ nMale * colSums(Maleclass)
  Femalecenter <- 1/ nFemale * colSums(Femaleclass)
  
  male_sum_matr <- matrix(c(0,0,0,0),2)
  female_sum_matr <- matrix(c(0,0,0,0),2)
  
  # Male Female are actually 50/50, so nFemale = nMale
  
  for(j in 1:nMale){
    male_sum_matr <- male_sum_matr + 
      crossprod(as.matrix(Maleclass[j,]) - Malecenter,
                as.matrix(Maleclass[j,]) - Malecenter)
    female_sum_matr <- female_sum_matr + 
      crossprod(as.matrix(Femaleclass[j,]) - Femalecenter,
                as.matrix(Femaleclass[j,]) - Femalecenter) 
  }
  Malecov <- 1/ nMale * male_sum_matr
  Femalecov <- 1/ nFemale * female_sum_matr
  
  totalcov <- 1/2 * (Malecov + Femalecov)
  
  totalobs <- df[,5:6]
  
  maleclassifier <- rep(FALSE,n)
  discriminantvalue <- rep(-100,n)
  Maleconst <- -(1/2) * Malecenter %*% solve(totalcov) %*% Malecenter + log(Maleproportion)
  Femaleconst <- -(1/2) * Femalecenter %*% solve(totalcov) %*% Femalecenter + log(Femaleproportion)
  
  for(k in 1:n){
    malediscriminant <- as.numeric(totalobs[k,]) %*% solve(totalcov) %*% Malecenter 
    femalediscriminant <- as.numeric(totalobs[k,]) %*% solve(totalcov) %*% Femalecenter
    discriminantvalue[k] <- malediscriminant + Maleconst - femalediscriminant - Femaleconst
    if(discriminantvalue[k] > 0){
      maleclassifier[k] <- TRUE
    }
  }
  
  
  
  print(solve(totalcov) %*% (Malecenter - Femalecenter))
  print(paste("Male constant:",Maleconst))
  print(paste("Female constant:",Femaleconst))
  return(maleclassifier)
}

#coef RW / CL = -2.9
maleclassifier <- LDA(crabdata)

plot(crabdata$RW,crabdata$CL,pch="+",
     col=ifelse(!maleclassifier, "red","blue"),
     main=c("LDA classified sex based on features",
            "Carapace Length and Rear Width"),ylab ="Carapace Length",
     xlab="Rear Width")
#intercept -5.066 slope 2.918

y <- c()
for( x in seq(from=5,to=22, by= 0.1)){
  y <- c(y,2.918 * x - 5.066)
}

lines(seq(from=5,to=22, by= 0.1),y)
legend("topleft",legend = levels(crabdata$sex),
       pch = "+",col=c("red","blue"))
