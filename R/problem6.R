##########################################################################################
clc <- function() cat(rep("\n", 50))
rm(list = setdiff(ls(), lsf.str()))
wants <- c("ICSNP", "mvtnorm","IQCC")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
for (pkg in wants) {library(pkg, character.only = TRUE)}
clc()
##########################################################################################
alpha <-5/100
Data <- read.csv(file='Book3.csv',sep=',',header=T,row.names=1)
summary(Data)
Data

#lit_mod<-with(LITERACY, lm(literacy.rate~newspapers+radios+tv.sets))
#summary(lit_mod)

Regression <- lm(Data$Literacy.Rate~Data$Newspapers+Data$Radios+Data$TV)
summary(Regression)
confint(Regression,level = 1-alpha)
