
#------------------------------------------------------------------------------#
# EM algorithm for regression when missing data mechanism is assumed ignorable
#------------------------------------------------------------------------------#
EM_y <- function(DD, inits = "empirical", eps = 1e-5, max.iter = 500, returnE = F) {
  # DD is data
	# inits can be a length 2 list for mean and cov matirx

	DD <- as.matrix(DD)
	MM <- ifelse(is.na(DD), 1, 0)

	n <- nrow(DD)
	p <- ncol(DD)

	bb <- array(NA, dim = c(n,p))
	AA <- array(NA, dim = c(p,p,n))

	if(inits[[1]][1] == "empirical"){

	  mean.in <- matrix(colMeans(DD, na.rm=T), p, 1)
		cov.in <- cov(DD, use = 'p')
		cov.in <- ifelse(is.na(cov.in), 0, cov.in)

	} else {

	  mean.in <- matrix(inits[[1]],p,1)
	  cov.in <- inits[[2]]

	}

  #--- control
	mean.1 <- mean.2 <- mean.in
	cov.1 <- cov.2 <- cov.in

	count <- 0
	ee <- 1

  while(ee > eps & count < max.iter){

    #--- E step -----------------------------------------------------------------#

    for(i in 1:n){
      
      miss.i <- which(MM[i, , drop=FALSE] == 1)
      ob.i <- which(MM[i, , drop=FALSE] == 0)
      
      if(length(miss.i) > 0){
      
        bb[i,miss.i] <- mean.1[miss.i,1 , drop=FALSE] + 
          cov.1[miss.i, ob.i , drop=FALSE] %*% 
            solve(cov.1[ob.i, ob.i, drop=FALSE]) %*% 
              (t(DD[i,ob.i , drop=FALSE]) - mean.1[ob.i,1, drop=FALSE])
        # E(z | y theta)
        
        AA[ob.i, miss.i, i] <- t(DD[i,ob.i, drop=FALSE]) %*% bb[i,miss.i, drop=FALSE]
        AA[miss.i, ob.i, i] <- t(bb[i,miss.i, drop=FALSE]) %*% DD[i,ob.i, drop=FALSE]
        # E(z | y theta)y'
        
        AA[miss.i, miss.i, i] <- cov.1[miss.i, miss.i, drop=FALSE] - 
          cov.1[miss.i, ob.i, drop=FALSE] %*% solve(cov.1[ob.i, ob.i, drop=FALSE]) %*% 
            cov.1[ob.i, miss.i, drop=FALSE] + t(bb[i,miss.i, drop=FALSE]) %*% 
              bb[i,miss.i, drop=FALSE]
        # E(zz' | y theta)
      }

      if(length(ob.i) > 0){
        bb[i,ob.i] <- DD[i,ob.i, drop=FALSE]
        AA[ob.i,ob.i,i] <- t(DD[i,ob.i, drop=FALSE]) %*% DD[i,ob.i, drop=FALSE]
      } #no missing data
      
    }

    #--- M Step ---------------------------------------------------------------#

    mean.2 <- matrix(apply(bb, 2, mean),p,1)
    cov.2 <- apply(AA, 1:2, mean) - mean.2 %*% t(mean.2)


    #--- control
	  ee <- max(c(abs(mean.2 - mean.1), abs(cov.2 - cov.1)))

	  mean.1 <- mean.2
	  cov.1 <- cov.2

	  count <- count+1
	}

  if(returnE == T){

	  return(list(mean = mean.1, cov = cov.1, n.iter = count, ex = bb))

  } else {

	  return(list(mean = mean.1, cov = cov.1, n.iter = count))

  }
}
