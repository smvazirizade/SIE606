clc <- function() cat(rep("\n", 50))
rm(list = setdiff(ls(), lsf.str()))
wants <- c("ICSNP", "mvtnorm","IQCC")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
for (pkg in wants) {library(pkg, character.only = TRUE)}
clc()





x1 <- 	c(1181,3532,2502,4510,3032,2130,1982,4675,2354,4606,3044,3340,2111,1291,1365,1175)
x2 <- 	c(14861,11367,13329,12328,12847,13979,13528,12699,13534,11609,14189,15052,12236,15482,14900,15078)
X <- t(rbind(x1,x2))
alpha <- 5/100
m <-dim(X)[1]
p <-dim(X)[2]
n=1
Phase=1
Sigma <- cov(X,X)
Mean <- c(mean(X[,1]),mean(X[,2]))
MEAN <- matrix(        rep(c(mean(X[,1]),mean(X[,2])),m), byrow = TRUE,ncol=p)
MuMatrix=X-MEAN
T2 <- matrix(0, m, 1)
for (i in 1:dim(MuMatrix)[1]) {
  T2[i,1] <- n * t(MuMatrix[i,]) %*% solve(Sigma) %*%(MuMatrix[i,])
}
#T2 <- n * (MuMatrix) %*% solve(Sigma) %*%t(MuMatrix)


if (n==1 && Phase==1){
  LCL= (m-1)^2/m* qbeta(alpha/2, p/2, (m-p-1)/2, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  MCL= (m-1)^2/m* qbeta(0.5, p/2, (m-p-1)/2, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  UCL= (m-1)^2/m* qbeta(1-alpha/2, p/2, (m-p-1)/2, ncp = 0, lower.tail = TRUE, log.p = FALSE)
}





plot(T2,xlim=c(1,m),ylim=c(LCL-0.2*abs(LCL), UCL+0.2*abs(UCL)) ,xlab="Sample", ylab="Value",pch=16,col="blue", cex  = 2 )
LineUp <-   list(x=c(0,m),y=c(UCL,UCL))
LineMean <- list(x=c(0,m),y=c(MCL,MCL))
LineDown <- list(x=c(0,m),y=c(LCL,LCL))
lines(LineUp,col="red",lwd  = 3 )
lines(LineMean,col="red" ,lwd  = 2 )
lines(LineDown,col="red" ,lwd  = 3 )
lines(T2)
par(new=TRUE)
plot(which(T2>UCL),T2[which(T2>UCL)],xlim=c(1,m),ylim=c(LCL-0.2*abs(LCL), UCL+0.2*abs(UCL)) ,xlab="Sample", ylab="Value",pch=16,col="red", cex  = 2 , ann=FALSE, axes=FALSE)
par(new=TRUE)
plot(which(T2<LCL),T2[which(T2<LCL)],xlim=c(1,m),ylim=c(LCL-0.2*abs(LCL), UCL+0.2*abs(UCL)) ,xlab="Sample", ylab="Value",pch=16,col="red", cex  = 2 , ann=FALSE, axes=FALSE)

n <-1
p <-2
m <-16
datum <- X



#mu <- c(5.682, 88.22)
mu <- Mean
#Sigma <- symMatrix(c(3.770, -5.495, 13.53), 2)

estat <- stats(X, 16, 1, 2) 
T22=T2.1(estat, 16, 1)
cchart.T2.1(T22, 16, 1, 2)

