library(quantmod)
install.packages("tidyquant")
library(tidyquant)
library(timetk)

aa <- 1:10
bb <- 11:20
cc <- 2002:2021

dt <- paste(cc,aa,bb, sep="-")
dt <- as.Date(dt); dt
AA <- as.data.frame(cbind(paste(dt), 41:60))
colnames(AA) <- c("kevdate", "kevnumbs")
AA$V1 <- as.Date(AA$V1)
AA$V2 <- as.numeric(AA$V2)

dim(AA)

AA <- tk_xts(AA)

AA_wekret <- weeklyReturn(AA, type = "log")

logsAA <- log(AA$V2)

head(AA_wekret); head(logsAA)
logsAArets <- cbind(logsAA, logsAA)
logsAArets$rets <- logsAArets$V2.1 - lag(logsAArets$V2)

head(AA_wekret); head(logsAArets$rets)
