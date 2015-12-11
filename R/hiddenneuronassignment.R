library(neuralnet)
mort <- read.csv2("data/mortality.csv")
head(mort)
stdday <- scale(mort[,1])
mort[,1] <- stdday
plot(LMR ~ Day, data = mort, type="l",col="red",lwd=2,
     main="Log mortality rate of fruit flies vs standardized # days",
     xlab="standardized # Days")
set.seed(7235)
model1 <- neuralnet(LMR ~ Day, data = mort, hidden = 1,
                    act.fct = "tanh", stepmax = 1e6, threshold = 0.1)
plot(model1)

plot(LMR ~ Day, data = mort, type="l",col="red",lwd=2,
     main="One hidden node neuralnet fit",
     xlab="standardized # Days") 
points(x=mort$Day, unlist(model1$net.result), col="darkorange", type="l", 
       lwd=2) 
legend("bottomright",legend = c("training data","neuralnet fit"),
       lty = c(1,1),col=c("red","darkorange"))

set.seed(7235)
model2 <- neuralnet(LMR ~ Day, data = mort, hidden = 2,
                    act.fct = "tanh", stepmax = 1e6, threshold = 0.1)
plot(model2)
set.seed(7235)
model3 <- neuralnet(LMR ~ Day, data = mort, hidden = 4,
                    act.fct = "tanh", stepmax = 1e6, threshold = 0.1)
plot(model3)
set.seed(7235)
model4 <- neuralnet(LMR ~ Day, data = mort, hidden = 5,
                    act.fct = "tanh", stepmax = 1e6, threshold = 0.1)
plot(model4)