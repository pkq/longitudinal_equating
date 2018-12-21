


source("gen_ir.R")


head(aSumScoreList)

#-------------------------
# Split sum score lists into each time point
#-------------------------

## "a" is the longitudinal anchor
## "b" is the benchmark form
aScoreTime1 <- aSumScoreList[[1]][1:250]
bScoreTime1 <- bSumScoreList[[1]][251:500]

## "a" is the longitudinal anchor
## "b" is the benchmark form
aScoreTime2 <- aSumScoreList[[2]][1:250]
bScoreTime2 <- bSumScoreList[[2]][251:500]


## "a" is the longitudinal anchor
## "b" is the benchmark form
aScoreTime3 <- aSumScoreList[[3]][1:250]
bScoreTime3 <- bSumScoreList[[3]][251:500]




#-------------------------
# Mean equate 
#-------------------------

## time 1


## time 2


## time 3



#-------------------------
# Linear equate
#-------------------------
