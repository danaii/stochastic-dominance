# create 2 pseudosamples : Example
x <- rnorm(5000 ,0, 1)
y <- rnorm(5000, 0, 3)
ax <- x/x  # Abundance at x coordinates
ay <- ax
n_boot <- 1000

stochastic_dominance_test <- function(x,y,ax,ay,n_boot) {   # n_boot= #number of bootstraps
  nx <- length(x)
  ny <- length(y)
  
  edf_abundance <- function(x,y,a){
    # calculates the empirical distribution function for locations, x, at x and y locations
    
    locs_total <- c(x,y) # union of all locations
    
    tracker <- c(a, numeric(length(y))) # sets a tracker which assigns abundance only to those which are 'data'
    tracker_sort <- tracker[order(locs_total)]
    EDF <- cumsum(tracker_sort) / sum(tracker_sort) # calculates the empirical df for x
    xEDF <- sort(locs_total)
    return(list(xEDF=xEDF, EDF=EDF))
  }
  
  # Calculate Empirical CDF for random samples of x and y
  empx <- edf_abundance(x,y,ax) #  ECDF for x
  empy <- edf_abundance(y,x,ay) #  ECDF for y
  
  # Calculate observed test stat
  test_OBS <- max(empy$EDF - empx$EDF)
  
  # Bootstrapping
  fx_ <- empx$EDF
  fy_ <- empy$EDF
  # Creates an ordered version of the EDFs by Averaging the distributions
  # when the EDF of y is locally smaller than that of x
  for(j in 1:length(fx_)){
    if(empx$EDF[j] < empy$EDF[j]) {
      fx_[j] <- (empx$EDF[j] + empy$EDF[j]) / 2
      fy_[j] <- (fx_[j])
    }
  }
  
  temp1 <- numeric(n_boot) # Preallocate vector
  
  for(i in 1:n_boot){
    pp <- fx_ - c(0,fx_[1:(length(fx_)-1)])
    r <- rmultinom(1, length(x), pp) # resamples from the ECDF the number of samples that x had
    F <- edf_abundance(empx$xEDF,empy$xEDF,r)
    
    pp <- fy_ - c(0, fy_[1:(length(fy_)-1)])
    r <- rmultinom(1,length(y),pp) # resamples from the ECDF the number of samples that x had
    G <- edf_abundance(empy$xEDF,empx$xEDF,r)
    
    temp1[i] <- max(G$EDF - F$EDF) # bootstrapped test statistic value
  }
  
  p <- sum(temp1 > test_OBS) / n_boot # calculates what proportion of times the bootstrapped test statistic exceeds the observed
  p <- p + .5*sum(temp1==test_OBS)/ n_boot # adds 0.5 times the proportion of times the test statistic equals the observed
  p
}

