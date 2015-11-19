## Fit semiparametric ANCOVA model
set.seed(555)
n1 <- 80
x1 <- runif(n1,min=0, max=3)
sd1 <- 0.3
e1 <- rnorm(n1,sd=sd1)
y1 <- 3*cos(pi*x1/2)  + 6 + e1

n2 <- 75
x2 <- runif(n2, min=0, max=3)
sd2 <- 0.2
e2 <- rnorm(n2, sd=sd2)
y2 <- 3*cos(pi*x2/2) + 3 + e2

n3 <- 90
x3 <- runif(n3, min=0, max=3)
sd3 <- 0.3
e3 <- rnorm(n3, sd=sd3)
y3 <- 3*cos(pi*x3/2)  + e3

data.bind <- rbind(cbind(x1,y1,1), cbind(x2,y2,2),cbind(x3,y3,3))
data.bind <- data.frame(data.bind)
colnames(data.bind)=c('x','y','group') 

x <- data.bind[,1]
y <- data.bind[,2]
group <- data.bind[,3]

loess.ancova(x,y,group, plot=TRUE, data.points=TRUE,legend.position="topright")
