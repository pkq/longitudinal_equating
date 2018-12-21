


#------------------------------------------------------------------------------#
# Generate Thetas
#------------------------------------------------------------------------------#
set.seed(674665)    

#--- Number of Observations
T <- 3
n <- 500

#--- Fixed effects 
b0   <- -0.50         # intercept (same for both groups) 
b1   <-  0.50         # linear slope group 1 
bVec <- c(b0, b1)   # vector of fixed effects

#--- Residual Variances 
#--- e ~ N (0, s2)
sdResid     <- 0.20
varResidMat <- diag(sdResid^2, T)  

#--- Random Effects Covariance Matrix 
sdInt     <-  0.95
sdSlp     <-  0.30
corIntSlp <- -0.3  

tauMat   <- diag(c(sdInt^2, sdSlp^2))       # variances
lowerTau <- c(corIntSlp * sdInt * sdSlp)  # covariance

tauMat[lower.tri(tauMat)] <- lowerTau
tauMat <- tauMat + t(tauMat) - diag(diag(tauMat))

#--- Design matricies 
xMat <- zMat <- cbind(rep(1, T), seq(1:T)-1)

#--- Means and variances
thetaCov  <- (zMat %*% tauMat %*% t(zMat)) + varResidMat
thetaMean <- xMat %*% bVec

#--- Generate thetas
theta <- mvtnorm::rmvnorm(n, mean = thetaMean, sigma = thetaCov)
colnames(theta) <- paste0("theta", 1:T)


round(rbind(colMeans(theta[1:250, ]), 
            colMeans(theta[251:500, ])), 3)

round(rbind(apply(theta[1:250, ], 2, sd), 
            apply(theta[251:500, ], 2, sd)), 3)

#------------------------------------------------------------------------------#
# Generate Item Responses
#------------------------------------------------------------------------------#
set.seed(766576)    

#--- Number of Items
I <- c(30)

#--- Item difficulty bounds
bLo <- c(-3.00, -1.00, 1.50, -5.00)
bHi <- c( 3.00,  2.00, 3.00, -1.00)

#--- Anchor Test
aRspLongList   <- aRspWideList <- list()
aItemParamList <- list()

#--- Item parameters
ip <- lsasim::item_gen(n_1pl = I, thresholds = 1, b_bounds = c(bLo[1], bHi[1]))

for (tt in 1:T)
{
  #--- Generate responses  
  i_j <- lsasim::booklet_sample(n_subj = n, book_item_design = matrix(1:I))
    
  rspW <- lsasim::response_gen(subject = i_j$subject,
                               item    = i_j$item,
                               theta   = theta[, paste0("theta", tt)],
                               b_par   = ip$b)


  rspL <- reshape(rspW, var = colnames(rspW[, -ncol(rspW)]), v.names = "points",
                   timevar = "item", times = 1:I, dir = "long")
  
  rspL$id <- NULL
  colnames(rspL)[1] <- "id"
  rspL$time <- tt
  rspL <- rspL[order(rspL$id, rspL$item), ]
  rownames(rspL) <- NULL

  aItemParamList[[tt]] <- ip$b    
  aRspWideList[[tt]]   <- rspW[, -ncol(rspW)]
  aRspLongList[[tt]]   <- rspL

}

#--- Benchmark Tests
bRspLongList <- bRspWideList <- list()
bItemParamList <- list()

for (tt in 1:T)
{
  #--- Item parameters
  ip <- lsasim::item_gen(n_1pl = I, thresholds = 1, b_bounds = c(bLo[tt+1], bHi[tt+1]))
    
  #--- Generate responses  
  i_j <- lsasim::booklet_sample(n_subj = n, book_item_design = matrix(1:I))
    
  rspW <- lsasim::response_gen(subject = i_j$subject,
                               item    = i_j$item,
                               theta   = theta[, paste0("theta", tt)],
                               b_par   = ip$b)
   
  rspL <- reshape(rspW, var = colnames(rspW[, -ncol(rspW)]), v.names = "points",
                   timevar = "item", times = 1:I, dir = "long")
  
  rspL$id <- NULL
  colnames(rspL)[1] <- "id"
  rspL$time <- tt
  rspL <- rspL[order(rspL$id, rspL$item), ]
  rownames(rspL) <- NULL
  
  bItemParamList[[tt]] <- ip$b    
  bRspWideList[[tt]]   <- rspW[, -ncol(rspW)]
  bRspLongList[[tt]]   <- rspL
}


head(bRspWideList[[1]])

aSumScoreList <- lapply(aRspWideList, rowSums)
bSumScoreList <- lapply(bRspWideList, rowSums)

par(mfrow = c(1, 3))
for(ii in 1:T)
{
  plot(density(aSumScoreList[[ii]][1:250]freq = TRUE))
  lines(density(bSumScoreList[[ii]][251:500], freq = TRUE), col = "red")
}


aSumScoreDf <- do.call(cbind.data.frame, aSumScoreList)
bSumScoreDf <- do.call(cbind.data.frame, bSumScoreList)

par(mfrow = c(1, 1))
plot(x = 1:T, y = colMeans(aSumScoreDf), type = "l")
lines(x = 1:T, y = colMeans(bSumScoreDf), col = "red")




## Mean Equate

## Linear Equate
## Equipercentile Equate
## Longitudinal Linear Equate






