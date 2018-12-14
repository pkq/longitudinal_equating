df <- ttl_g_w[, c("total-1-4", "total-2-4", "total-3-4")]
naCount <- apply(df, 1, function(x) sum(is.na(x)))
DD <- df[naCount != 3, ]
rownames(DD) <- NULL

emout <- EM_y(DD)

str(emout)



ttl_w_a <- ttl_g_w[, c("total-1-4", "total-2-4", "total-3-4")]
aNaCount <- apply(ttl_w_a, 1, function(x) sum(is.na(x)))
ttl_w_a <- ttl_w_a[aNaCount != 3, ]
rownames(ttl_w_a) <- NULL

emOutA <- EM_y(ttl_w_a)
yEmMeanVec <- emOutA$mean
yEmCovMat <- emOutA$cov

ttl_w_b <- ttl_g_w[, c("total-1-1", "total-2-2", "total-3-3")]
bNaCount <- apply(ttl_w_b, 1, function(x) sum(is.na(x)))
ttl_w_b <- ttl_w_b[bNaCount != 3, ]
rownames(ttl_w_b) <- NULL

emOutB <- EM_y(ttl_w_b)
xEmMeanVec <- emOutB$mean
xEmCovMat <- emOutB$cov


equate_out <- list()

for(jj in 1:nrow(sub_ttl_g_w))
{
  xVec_j <-  t(unname(as.matrix(sub_ttl_g_w[jj, ])))

  mvOut1 <- mv_linear_equate_x_to_y(xVec     = xVec_j, 
                                    yMeanVec = yEmMeanVec, 
                                    yCovMat  = diag(diag(yEmCovMat)), 
                                    xMeanVec = xEmMeanVec, 
                                    xCovMat  = diag(diag(xEmCovMat)))
  
  is.positive.definite(yEmCovMat)
  SigmaYInvSigmaX <- yEmCovMat %*% solve(xEmCovMat)
  cholSigmaYInvSigmaX <- chol(SigmaYInvSigmaX)
 
  xEquateToY <- cholSigmaYInvSigmaX %*% xVec + (yMeanVec - cholSigmaYInvSigmaX %*% xMeanVec)  



  mvOut2 <- mv_linear_equate_x_to_y(xVec     = xVec_j, 
                                    yMeanVec = yEmMeanVec, 
                                    yCovMat  = yEmCovMat, 
                                    xMeanVec = xEmMeanVec, 
                                    xCovMat  = xEmCovMat)
  
  equate_out[[jj]] <- cbind("raw" = mvOut2[, 1], "m-eq1" = mvOut1[, 2], "m-eq2" = mvOut2[, 2])
}