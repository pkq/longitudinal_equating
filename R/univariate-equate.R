

#------------------------------------------------------------------------------#
# univariate_equate.R equates benchmark scores to the anchor score scale.
# Each time point is treated seperately.
# Equating methods are mean, linear and equipercentile
#------------------------------------------------------------------------------#

## Define relative paths
rDir <- "./R"
rdsDir <- paste(rDir, "Rds", sep = "/")


#------------------------------------------------------------------------------#
# Read in sum score files
#------------------------------------------------------------------------------#

## Save anchor and benchmark sum score
aSumScoreList <- readRDS(paste(rdsDir, "anchorSumScores.Rds"))
bSumScoreList <- readRDS(paste(rdsDir, "benchmarkSumScores.Rds"))


#------------------------------------------------------------------------------#
# Split sum score lists into each time point
#------------------------------------------------------------------------------#
## time 1
aScore_1 <- aSumScoreList[[1]][1:250]
bScore_1 <- bSumScoreList[[1]][251:500]

## time 2
aScore_2 <- aSumScoreList[[2]][1:250]
bScore_2 <- bSumScoreList[[2]][251:500]

## time 3
aScore_3 <- aSumScoreList[[3]][1:250]
bScore_3 <- bSumScoreList[[3]][251:500]




#------------------------------------------------------------------------------#
# Mean equate 
#------------------------------------------------------------------------------#

## time 1


## time 2


## time 3



#------------------------------------------------------------------------------#
# Linear equate
#------------------------------------------------------------------------------#

## time 1


## time 2


## time 3


#------------------------------------------------------------------------------#
# Equipercentile equate
#------------------------------------------------------------------------------#

## time 1


## time 2


## time 3

