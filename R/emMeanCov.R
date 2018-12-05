
#------------------------------------------------------------------------------#
# EM algorithm for regression when missing d_mata mechanism is assumed ignorable
# The use of drop = FALSE througout preserve the matrix class
#------------------------------------------------------------------------------#
emMeanCov <- function(df, inits = "empirical", eps = 1e-5, max_iter = 500, returnE = F) {
  # df is data
  # inits can be a length 2 list for mean and cov matirx

  d_mat <- as.matrix(df)
  obs_vec <- ifelse(is.na(d_mat), 0, 1)

  n <- nrow(d_mat)
  p <- ncol(d_mat)

  bb <- array(NA, dim = c(n, p))
  aa <- array(NA, dim = c(p, p, n))

  #--- Starting values --------------------------------------------------------#
  if (inits == "empirical") {

    mean_start <- matrix(colMeans(d_mat, na.rm = T), nrow = p, ncol = 1)
    cov_start  <- cov(d_mat, use = 'p')
    cov_start  <- ifelse(is.na(cov_start), 0, cov_start)
  
  } else {

    mean_start <- matrix(inits[[1]], nrow = p, ncol = 1)
    cov_start  <- inits[[2]]
  
  }

  #--- control
  mean_1 <- mean_2 <- mean_start
  cov_1  <- cov_2  <- cov_start

  count <- 0
  ee    <- 1


  #=== Start EM ===============================================================#
  while(ee > eps & count < max_iter){

   #--- E-step ----------------------------------------------------------------#
   for (i in 1:n){
      
    v0 <- which(obs_vec[i, , drop=FALSE] == 0)  # missing data
    v1 <- which(obs_vec[i, , drop=FALSE] == 1)  # observed data
     
    #--- missing data  
    if (length(v0) > 0){
     d_mat_v1      <- d_mat[i, v1, drop = FALSE]
     inv_cov1_v1v1 <- solve(cov_1[v1, v1, drop = FALSE])
     cov1_v0v1     <- cov_1[v0, v1, drop = FALSE] 
     cov1_v1v0     <- cov_1[v1, v0, drop = FALSE]
     cov1_v0v0     <- cov_1[v0, v0, drop = FALSE]
     mean1_v0      <- mean_1[v0, 1, drop = FALSE]
     mean1_v1      <- mean_1[v1, 1, drop = FALSE]
     
     #--- E(z | y theta)
     bb[i, v0] <- mean1_v0 + cov1_v0v1 %*% inv_cov1_v1v1 %*% (t(d_mat_v1) - mean1_v1)
     
     bb_v0 <- bb[i, v0, drop = FALSE]

     #--- E(z | y theta)y'
     aa[v1, v0, i] <- t(d_mat_v1) %*% bb_v0
     aa[v0, v1, i] <- t(bb_v0) %*% d_mat_v1
        
     #--- E(zz' | y theta)
     aa[v0, v0, i] <- 
       cov1_v0v0 - cov1_v0v1 %*% inv_cov1_v1v1 %*% cov1_v1v0 + t(bb_v0) %*% bb_v0
        
      }

    #--- no missing data
    if (length(v1) > 0){
     d_mat_v1      <- d_mat[i, v1, drop = FALSE]
     bb[i, v1]     <- d_mat_v1
     aa[v1, v1, i] <- t(d_mat_v1) %*% d_mat_v1
    } 
      
   }

   #--- M-step ----------------------------------------------------------------#

   mean_2 <- matrix(apply(bb, 2, mean), p, 1)
   cov_2  <- apply(aa, 1:2, mean) - mean_2 %*% t(mean_2)

   #--- control
   ee <- max(c(abs(mean_2 - mean_1), abs(cov_2 - cov_1)))

   mean_1 <- mean_2
   cov_1  <- cov_2

   count <- count + 1
  }

  if (returnE == T){

    return(list(mean = mean_1, cov = cov_1, n_iter = count, ex = bb))

  } else {

    return(list(mean = mean_1, cov = cov_1, n_iter = count))

  }
}
