#------------------------------------------------------------------------------#
# univariate_equate.R equates benchmark scores to the anchor score scale.
# Each time point is treated separately.
# Equating methods are mean, linear and equipercentile
#------------------------------------------------------------------------------#

# Define relative paths
rDir <- "./R"
rdsDir <- paste(rDir, "Rds", sep = "/")

# load libraries
library(equate)
library(dplyr)

#------------------------------------------------------------------------------#
# Read in sum score files
#------------------------------------------------------------------------------#
aSumScoreList <- readRDS(paste(rdsDir, "anchorSumScores.Rds", sep = "/"))
bSumScoreList <- readRDS(paste(rdsDir, "benchmarkSumScores.Rds", sep = "/"))

# Number of time points
T <- length(aSumScoreList)

#------------------------------------------------------------------------------#
# Split sum score into samples
#------------------------------------------------------------------------------#
aObsScoreList <- lapply(aSumScoreList, function(vec) vec[1:250])
bObsScoreList <- lapply(bSumScoreList, function(vec) vec[251:500])

#------------------------------------------------------------------------------#
# Compute score frequencies
#------------------------------------------------------------------------------#
aFreqTable <- lapply(aObsScoreList, freqtab)
bFreqTable <- lapply(bObsScoreList, freqtab)

#------------------------------------------------------------------------------#
# Compute summary of frequencies
#------------------------------------------------------------------------------#
aFreqSummary <- lapply(aFreqTable, summary)
bFreqSummary <- lapply(bFreqTable, summary)

#------------------------------------------------------------------------------#
# Concordance functions
#------------------------------------------------------------------------------#
buildConcordance <- function(tt, eqType) 
{
  eqMean_t <- equate(bFreqTable[[tt]], aFreqTable[[tt]], 
                     type = eqType, verbose = TRUE)
]  
  eqMeanTab_t <- eqMean_t$concordance
  colnames(eqMeanTab_t) <- c("targetScore", "equatedScore")
   
  return(eqMeanTab_t)
}

#------------------------------------------------------------------------------#
# Equate
#------------------------------------------------------------------------------#
# Mean equating
eqMean <- lapply(seq_along(1:T), buildConcordance, eqType = "mean")

# Linear equating
eqLin <- lapply(seq_along(1:T), buildConcordance, eqType = "linear")

# Equipercentile equating
eqEqp <- lapply(seq_along(1:T), buildConcordance, eqType = "equipercentile")

#------------------------------------------------------------------------------#
# Save
#------------------------------------------------------------------------------#
saveRDS(eqMean, paste(rdsDir, "meanConcordance.Rds", sep = "/"))
saveRDS(eqLin, paste(rdsDir, "linConcordance.Rds", sep = "/"))
saveRDS(eqEqp, paste(rdsDir, "EqpConcordance.Rds", sep = "/"))
