

#------------------------------------------------------------------------------#
# univariate_equate.R equates benchmark scores to the anchor score scale.
# Each time point is treated seperately.
# Equating methods are mean, linear and equipercentile
#------------------------------------------------------------------------------#

setwd("./longitudinal_equating-master")
## Define relative paths
rDir <- "./R"
rdsDir <- paste(rDir, "Rds", sep = "/")


#------------------------------------------------------------------------------#
# Read in sum score files
#------------------------------------------------------------------------------#

## Save anchor and benchmark sum score
aSumScorePath<-paste(rdsDir, "anchorSumScores.Rds", sep = "/")
bSumScorePath<-paste(rdsDir, "benchmarkSumScores.Rds", sep = "/")
aSumScoreList <- readRDS(aSumScorePath)
bSumScoreList <- readRDS(bSumScorePath)


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
# Create frequency tables for sumscores  
#------------------------------------------------------------------------------#

library(equate)

## time 1
# Convert list to vectors # 
unlist(bScore_1, recursive = TRUE, use.names = TRUE)
unlist(aScore_1, recursive = TRUE, use.names = TRUE)
unlist(bScore_2, recursive = TRUE, use.names = TRUE)
unlist(aScore_2, recursive = TRUE, use.names = TRUE)
unlist(bScore_3, recursive = TRUE, use.names = TRUE)
unlist(aScore_3, recursive = TRUE, use.names = TRUE)

# create frequency table for anchor and benchmark forms # 
aScore_1_freq <- freqtab(aScore_1)
bScore_1_freq <- freqtab(bScore_1)
aScore_2_freq <- freqtab(aScore_2)
bScore_2_freq <- freqtab(bScore_2)
aScore_3_freq <- freqtab(aScore_3)
bScore_3_freq <- freqtab(bScore_3)

#summary of frequency table for anchor and benchmark forms # 
rbind(anchor_1 = summary(aScore_1_freq), benchmark_1 = summary(bScore_1_freq), 
      anchor_2 = summary(aScore_2_freq), benchmark_2 = summary(bScore_2_freq), 
      anchor_3 = summary(aScore_3_freq), benchmark_3 = summary(bScore_3_freq))
  kable()


#------------------------------------------------------------------------------#
# Mean equate 
#------------------------------------------------------------------------------#

## time 1
eqMean_1 <- equate(bScore_1_freq, aScore_1_freq, 
                 type = "mean", 
                 verbose = TRUE)
summary(eqMean_1)

# create a look-up table that includes target score and equated score 
eqMean_1_tb<-eqMean_1$concordance%>%
  #create new variables: season and equating method 
  mutate(season = 1, method = "mean")%>%
  #rename variables: season 
  rename(target_score = scale, equated_score = yx)%>%
  #reorder variables# 
  select(target_score, equated_score)
eqMean_1_tb


## time 2
eqMean_2 <- equate(bScore_2_freq, aScore_2_freq, 
                type = "mean", 
                verbose = TRUE)
summary(eqMean_2)

# create a look-up table that includes target score and equated score 
eqMean_2_tb<-eqMean_2$concordance%>%
  #create new variables: season and equating method 
  mutate(season = 2, method = "mean")%>%
  #rename variables
  rename(target_score = scale, equated_score = yx)%>%
  #reorder variables# 
  select(target_score, equated_score)
eqMean_2_tb


## time 3
eqMean_3 <- equate(bScore_3_freq, aScore_3_freq, 
                type = "mean", 
                verbose = TRUE)
summary(eqMean_3)

# create a look-up table that includes target score and equated score 
eqMean_3_tb<-eqMean_3$concordance%>%
  #create new variables: season and equating method 
  mutate(season = 3, method = "mean")%>%
  #rename variables
  rename(target_score = scale, equated_score = yx)%>%
  #reorder variables# 
  select(target_score, equated_score)
eqMean_3_tb
#------------------------------------------------------------------------------#
# Linear equate
#------------------------------------------------------------------------------#

## time 1
eqLin_1 <- equate(bScore_1_freq, aScore_1_freq, 
                type = "linear", 
                verbose = TRUE)
summary(eqLin_1)

# create a look-up table that includes target score and equated score 
eqLin_1_tb<-eqLin_1$concordance%>%
  #create new variables: season and equating method 
  mutate(season = 1, method = "linear")%>%
  #rename variables
  rename(target_score = scale, equated_score = yx)%>%
  #reorder variables# 
  select(target_score, equated_score)
eqLin_1_tb

## time 2
eqLin_2 <- equate(bScore_2_freq, aScore_2_freq, 
                  type = "linear", 
                  verbose = TRUE)
summary(eqLin_2)

# create a look-up table that includes target score and equated score 
eqLin_2_tb<-eqLin_2$concordance%>%
  #create new variables: season and equating method 
  mutate(season = 2, method = "linear")%>%
  #rename variables
  rename(target_score = scale, equated_score = yx)%>%
  #reorder variables# 
  select(target_score, equated_score)

eqLin_2_tb
## time 3
eqLin_3 <- equate(bScore_3_freq, aScore_3_freq, 
                  type = "linear", 
                  verbose = TRUE)
summary(eqLin_3)

# create a look-up table that includes target score and equated score 
eqLin_3_tb<-eqLin_3$concordance%>%
  #create new variables: season and equating method 
  mutate(season = 3, method = "linear")%>%
  #rename variables
  rename(target_score = scale, equated_score = yx)%>%
  #reorder variables# 
  select(target_score, equated_score)
eqLin_3_tb

#------------------------------------------------------------------------------#
# Equipercentile equate
#------------------------------------------------------------------------------#

## time 1
eqPct_1 <- equate(bScore_1_freq, aScore_1_freq, 
                  type = "equipercentile", 
                  verbose = TRUE)
summary(eqPct_1)

# create a look-up table that includes target score and equated score 
eqPct_1_tb<-eqPct_1$concordance%>%
  #create new variables: season and equating method 
  mutate(season = 1, method = "equipercentile")%>%
  #rename variables
  rename(target_score = scale, equated_score = yx)%>%
  #reorder variables# 
  select(target_score, equated_score)
eqPct_1_tb

## time 2
eqPct_2 <- equate(bScore_2_freq, aScore_2_freq, 
                  type = "equipercentile", 
                  verbose = TRUE)
summary(eqPct_2)

# create a look-up table that includes target score and equated score 
eqPct_2_tb<-eqPct_2$concordance%>%
  #create new variables: season and equating method 
  mutate(season = 2, method = "equipercentile")%>%
  #rename variables
  rename(target_score = scale, equated_score = yx)%>%
  #reorder variables# 
  select(target_score, equated_score)
eqPct_2_tb


## time 3
eqPct_3 <- equate(bScore_3_freq, aScore_3_freq, 
                  type = "equipercentile", 
                  verbose = TRUE)
summary(eqPct_3)

# create a look-up table that includes target score and equated score 
eqPct_3_tb<-eqPct_3$concordance%>%
  #create new variables: season and equating method 
  mutate(season = 3, method = "equipercentile")%>%
  #rename variables
  rename(target_score = scale, equated_score = yx)%>%
  #reorder variables# 
  select(target_score, equated_score)
eqPct_3_tb
