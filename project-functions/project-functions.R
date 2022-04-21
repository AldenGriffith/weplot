get.total.N <- function(x){
  
  dims <- 2:length(dim(x))
  return(apply(x, dims, sum))
  
} 


extinct.prob <- function(N, limit, time = NULL){
  
  
  if (length(dim(N)) == 3){
    
    if (is.null(time)) time <- dim(N)[2] - 1
    
    N <- apply(N[,1:(time+1),], 2:3, sum)
    
  } else {
    
    if (is.null(time)) time <- dim(N)[1] - 1
    
    N <- N[1:(time+1),]
  }
  
  ext <- sum(apply(N,2,function(x){any(x < limit)}))
  
  return(ext / dim(N)[2])
  
}


remove.extinct <- function(N, limit, time = NULL, replace = NA_real_){
  
  if (length(dim(N)) == 3){
    
    if (is.null(time)) time <- dim(N)[2] - 1
    
    N <- apply(N, 2:3, sum)
    
  } else {
    
    if (is.null(time)) time <- dim(N)[1] - 1
    
  }
  
  
  for (it in 1:ncol(N)){
    
    i <- which(N[1:(time+1),it] < limit)
    
    if (length(i) > 0) N[min(i):dim(N)[1],it] <- replace
    
  }
  
  return(N)
  
}


median.N <- function(N, time = NULL){
  
  if (length(dim(N)) == 3) N <- apply(N, 2:3, sum)
  
  if (is.null(time)){
    
    return(apply(N, 1, median, na.rm = TRUE))
    
  } else {
    
    return(median(N[time+1,], na.rm = TRUE))
  }
  
}


N.start <- function(A, Total.N){
  
  w <- as.numeric(eigen(A)$vectors[,1]/sum(eigen(A)$vectors[,1]))
  
  return(round(w*Total.N))
  
}


get.lam <- function(A) as.numeric(eigen(A)$values[1])


mat.dstoch <- function(A, Nt, repro.rows = 1, repro.cols = 2:ncol(A),
                       post.breed = TRUE){
  
  # repro.rows <- 1:2  #default = 1
  # repro.cols <- 2:ncol(A)
  # Nt <- rep(100, nrow(A))
  
  
  N.out <- Nt*0
  
  #need to specify post-pre census
  
  A.surv <- A
  A.surv[repro.rows, repro.cols] <- 0
  
  A.repro <- A*0
  A.repro[repro.rows, repro.cols] <- A[repro.rows, repro.cols]
  
  
  for (j in 1:ncol(A)){
    
    for (i in 1:nrow(A)){
      
      #survival contributions
      N.out[i] <- N.out[i] + sum(rbinom(Nt[j], 1, A.surv[i,j]))   #this currently allows the same individual to go to multiple classes
      
      #reproduction contributions
      N.out[i] <- N.out[i] + sum(rpois(Nt[j], A.repro[i,j]))
      
    }
  }
  
  return(N.out)  
  
}