clc <- function() cat(rep("\n", 50))
rm(list = setdiff(ls(), lsf.str()))
wants <- c("stats","ggplot2","plotly","xlsx")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
for (pkg in wants) {library(pkg, character.only = TRUE)}
clc()

Number=1:200
Data <- read.csv("HW4_1Data.csv", header = TRUE, row.names = 1)
Data <- Data[Number,]

pca1 = prcomp(Data, scale. = TRUE, center= TRUE)
# sqrt of eigenvalues
pca1$sdev
# loadings
pca1$rotation
# PCs (aka scores)
head(pca1$x)
# main data
head(Data)



# create data frame with scores
scores = as.data.frame(pca1$x)
# plot of observations
ggplot(data = scores, aes(x = PC1, y = PC2, label = rownames(scores))) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(colour = "tomato", alpha = .9, size = 4) +
  ggtitle("PCA plot")

# function to create a circle
circle <- function(center = c(0, 0), npoints = 100) {
  r = 1
  tt = seq(0, 2 * pi, length = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
corcir = circle(c(0, 0), npoints = 100)
# create data frame with correlations between variables and PCs
correlations = as.data.frame(cor(Data, pca1$x))
# data frame with arrows coordinates
arrows = data.frame(x1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), y1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), x2 = correlations$PC1, 
                    y2 = correlations$PC2)
# geom_path will do open circles
ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") + 
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") + 
  geom_text(data = correlations, aes(x = PC1, y = PC2, label = rownames(correlations))) + 
  geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0, 
                                                             colour = "gray65") + xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "pc1 aixs",  y = "pc2 axis") + ggtitle("Circle of correlations")

x=as.table(pca1$x)
s <- c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10")
x <- as.table(x)
plot(Data)
df = data.frame(pca1$x)       # df is a data frame
plot(df)






Number=201:300
Data2 <- read.csv("HW4_1Data.csv", header = TRUE, row.names = 1)
Data2 <- Data2[Number,]

par(mfrow=c(5,2))
for (i in 1:10) {
x <- Data[,i]
y <- Data2[,i]
Mean <- mean(x)
Var  <- var(x)
UCL= Mean+3*Var^.5
LCL= max(Mean-3*Var^.5)  
plot(y,ylim=c(LCL-0.2*abs(LCL), UCL+0.2*abs(UCL)) ,xlab="Sample", ylab="Value",pch=16,col="red", cex  = 1 )
LineUp <- list(x=c(0,length(x)),y=c(UCL,UCL))
LineMean <- list(x=c(0,length(x)),y=c(Mean,Mean))
LineDown <- list(x=c(0,length(x)),y=c(LCL,LCL))
lines(LineUp,col="red",lwd  = 3 )
lines(LineMean,col="red" ,lwd  = 2 )
lines(LineDown,col="red" ,lwd  = 3 )
lines(y)
}


pca2 = prcomp(Data2, scale. = TRUE, center= TRUE)
head(pca2$x)
par(mfrow=c(5,2))
for (i in 1:10) {
  x <- pca1$x[,i]
  y <- pca2$x[,i]
  Mean <- mean(x)
  Var  <- var(x)
  UCL= Mean+3*Var^.5
  LCL= max(Mean-3*Var^.5)  
  plot(y,ylim=c(LCL-0.2*abs(LCL), UCL+0.2*abs(UCL)) ,xlab="Sample", ylab="Value",pch=16,col="red", cex  = 1 )
  LineUp <- list(x=c(0,length(x)),y=c(UCL,UCL))
  LineMean <- list(x=c(0,length(x)),y=c(Mean,Mean))
  LineDown <- list(x=c(0,length(x)),y=c(LCL,LCL))
  lines(LineUp,col="red",lwd  = 3 )
  lines(LineMean,col="red" ,lwd  = 2 )
  lines(LineDown,col="red" ,lwd  = 3 )
  lines(y)
}


