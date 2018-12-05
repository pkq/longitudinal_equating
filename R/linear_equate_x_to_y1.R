linear_equate_x_to_y1 <- function(x, muY, sigmaY, muX, sigmaX)
{
  sigmaYBysigmaX <- sigmaY / sigmaX

  xEquateToY <- (sigmaYBysigmaX * x) + (muY - (sigmaYBysigmaX * muX)) 
  
  return(cbind.data.frame(rawX = x, equatedX = round(xEquateToY)))
}