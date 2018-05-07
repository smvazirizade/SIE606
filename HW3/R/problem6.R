##########################################################################################
clc <- function() cat(rep("\n", 50))
rm(list = setdiff(ls(), lsf.str()))
wants <- c("ICSNP", "mvtnorm","IQCC")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
for (pkg in wants) {library(pkg, character.only = TRUE)}
clc()
##########################################################################################
system.time(readLines("http://www.jhsph.edu"))
Rprof(tmp <- tempfile())
example(glm)
Rprof()
summaryRprof(tmp)

