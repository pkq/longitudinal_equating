mv_linear_equate_x_to_y <- function(xVec, yMeanVec, yCovMat, xMeanVec, xCovMat)
{

  SigmaYInvSigmaX <- yCovMat %*% solve(xCovMat)
  cholSigmaYInvSigmaX <- chol(SigmaYInvSigmaX)
 
  xEquateToY <- cholSigmaYInvSigmaX %*% xVec + (yMeanVec - cholSigmaYInvSigmaX %*% xMeanVec)  

  return(cbind.data.frame(rawX = xVec, equatedX = round(xEquateToY)))
}