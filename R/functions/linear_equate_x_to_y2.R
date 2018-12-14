linear_equate_x_to_y2 <- function(x, muY, sigmaY, muX, sigmaX)
{
  sigmaYBysigmaX <- sigmaY / sigmaX

  sqrt_sigmaYBysigmaX <- sqrt(sigmaYBysigmaX)

  xEquateToY <- (sqrt_sigmaYBysigmaX * x) + (muY - (sqrt_sigmaYBysigmaX * muX)) 
  
  return(cbind.data.frame(rawX = x, equatedX = round(xEquateToY)))
}