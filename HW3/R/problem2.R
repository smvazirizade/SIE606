clc <- function() cat(rep("\n", 50))
clc()
rm(list = setdiff(ls(), lsf.str()))
wants <- c("ICSNP", "mvtnorm")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
for (pkg in wants) {library(pkg, character.only = TRUE)}


#x<- 	c(1181,3532,2502,4510,3032,2130,1982,4675,2354,4606,3044,3340,2111,1291,1365,1175)
x <- 	c(14861,11367,13329,12328,12847,13979,13528,12699,13534,11609,14189,15052,12236,15482,14900,15078)



Mean <- mean(x)
Var  <- var(x)

UCL= Mean+3*Var^.5
LCL= max(0,Mean-3*Var^.5)  
plot(x,ylim=c(LCL-0.2*abs(LCL), UCL+0.2*abs(UCL)) ,xlab="Sample", ylab="Value",pch=16,col="red", cex  = 2 )
LineUp <- list(x=c(0,length(x)),y=c(UCL,UCL))
LineMean <- list(x=c(0,length(x)),y=c(Mean,Mean))
LineDown <- list(x=c(0,length(x)),y=c(LCL,LCL))
lines(LineUp,col="red",lwd  = 3 )
lines(LineMean,col="red" ,lwd  = 2 )
lines(LineDown,col="red" ,lwd  = 3 )
lines(x)


