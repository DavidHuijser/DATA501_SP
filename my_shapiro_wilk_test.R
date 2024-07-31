my_shapiro_wilk <- function(x) {
  
  # find length of array 
  n <-length(x) 
  
  # step 1 (order data)
  ordered <- sort(x)
  
  # step 2 (obtain ordered sample from N(0,1) distribution with a correction)
  m <- qnorm((1:n - 0.375) / (n + 0.25))
  
  # step 3 - use approximation to obtain coefficients vector a (except a[1] and a[n])
  a <- 2*m
  a[1] <- 0
  a[n] <- 0
  
  # Using Stirling approximation from Royston 1982 to calculate g
  if (n < 20){
    t <- n-1
    g <- ((6*t+7)/(6*t+13))*sqrt( (exp(1)/(t+2))*((t+1)/(t+2))**(t-2) )  
  }else
  {
    t <- n
    g <- ((6*t+7)/(6*t+13))*sqrt( (exp(1)/(t+2))*((t+1)/(t+2))**(t-2) )  
  }
  
  # There are two common was to calculate `g`
  # Use Gamma-function to calculate g
  if (n < 20){
    g <- gamma(0.5*n)/(sqrt(2)*gamma(0.5*n+0.5))
  }else
  {
    g <- gamma(0.5*n+0.5)/(sqrt(2)*gamma(0.5*n+1))
  }
  
  # Use g to find a[1] and a[n]
  a_start <- sqrt((g/(1-2*g))*sum(a**2))
  a[1] <--a_start
  a[n] <-a_start
  
  # normalize a 
  a <- a / sqrt(sum(a^2))
  
  # Step 4 Calculate W  
  X_bar <- mean(ordered)
  numerator <- (sum(a * ordered))^2
  numerator
  denominator <- sum((ordered - X_bar)^2)
  return(numerator/denominator)
}

my_shapiro_wilk_LA <- function(x) {
  
  # step 1 (sort data)
  sorted_x <- sort(x)
  n <-length(x)
  
  # step 2 (obtain ordered sample from N(0,1) distribution with a correction)
  m <-  matrix(qnorm((1:n - 0.375) / (n + 0.25)), nrow = n, byrow = TRUE)
  
  # step 3 Find co-efficients using LA
  V <- (1 / (n - 1)) * (m %*% t(m))
  
  # Use Pseudo-inverse instead of inverse to avoid SVD issues
  invV <- pseudoinverse(V)
  C <- sqrt(t(m)%*%invV%*%invV%*%m)[1]
  a <- t(m)%*%invV/C
  W <- sum(a*sorted_x)**2/(sum( (x-mean(x))**2))
  return(W)
}

my_shapiro_wilk_test <- function(x,approx=TRUE) {
  
  # Input test
  if (any(is.na(x))) 
    stop(paste("There seems to be at least one NA value in the data."))
  
  if (any(is.infinite(x))) 
    stop(paste("There seems to be at least one infinity value in the data."))
  
  #if (typeof(x)=="double")) print("There seems to be at least one infinity value in the data.")
  
  if ( (NROW(x)!=1) & (NCOL(x)!=1)) 
    stop(paste("There seems to be a problem with the dimensions of the data."))
  
  if (length(x) < 3) 
    stop(paste("The data does not have enough elements."))
  
  if (length(x) >2000) 
    stop(paste("The data has too many elements."))
  
  # Choose which type of calculation is preferred. 
  if (approx)
  {
    W= my_shapiro_wilk(x)
  }else
  {
    W= my_shapiro_wilk_LA(x) 
  }  
  return(W)
}


