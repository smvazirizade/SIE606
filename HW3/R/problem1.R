clc <- function() cat(rep("\n", 50))
rm(list = setdiff(ls(), lsf.str()))
wants <- c("ICSNP", "mvtnorm","IQCC")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
for (pkg in wants) {library(pkg, character.only = TRUE)}
clc()




X <- matrix(c(2,8,6,8,12,9,9,10),nrow=4,ncol=2)
muH0 <- matrix(c(7,11),nrow=2,ncol=1)
alpha <- 5/100
n <-dim(X)[1]
p <-dim(X)[2]
m <- 1
#S12 <- cov(X[,1],X[,2])
#S11 <-cov(X[,1],X[,1])
#S22 <-cov(X[,2],X[,2])
#Sigma <- matrix(c(S11,S12,S12,S22),nrow=2,ncol=2)
Sigma <- cov(X,X)
MuMatrix= matrix(mean(X[,1])-muH0[1],mean(X[,2])-muH0[2],nrow=2,ncol=1)
T2 <- n * t(MuMatrix) %*% solve(Sigma) %*%MuMatrix
(n-1)*p/(n-p)*qf(1-alpha, df1=n-p, df2=p) 
T2
pvalue=1-pf(T2/((n-1)*p/(n-p)), df1=n-p, df2=p) 
pvalue


HotellingsT2(X, mu=(muH0))

