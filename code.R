# Read in Win/Loss matrix -----------------------------------------------------
N <- 28
win.matrix <- read.csv("~/OneDrive - University of Birmingham/Bayesian Inference and Computation/Assignments/3/win_matrix.csv")
wards <- read.csv("wards.csv")
# Functions ----------------------------------------------------------------
log.piij <- function(lambda.i, lambda.j) {
  # Calculates the log of the probability ward i is chosen over ward j
  # Input:
  #   lambda.i: lambda_i
  #   lambda.j: lambda_j
  # Output:
  #   log pi_ij
  
  lambda.i - log(exp(lambda.i)+exp(lambda.j))
}

log.likelihood <- function(lambda, win.matrix){
  # Calculates the log-likelihood function for the Bradley-Terry Model
  # Input:
  #   lambda: vector of lambda values
  #   win.matrix: matrix of times area i (rows) beat area j (columns)
  # Output:
  #   The log-likelihood
  
  result <- outer(lambda, lambda, log.piij)*win.matrix
  
  return(sum(result))
}


# Set Up MCMC -------------------------------------------------------------
n.iter <- 15000
lambda.current <- rep(0, N)
log.likelihood.current <- log.likelihood(lambda.current, win.matrix)
lambda.store   <- matrix(NA, N, n.iter)
counter <- 0


# Run MCMC ----------------------------------------------------------------
#Run the MCMC algorithm for n.iter iterations

for(j in 1:n.iter){
  
  for(i in 2:N){
    
    #Get a vector of the current lambda values and change one value
    #Propose the new value from a normal distirbution
    lambda.prop    <- lambda.current 
    lambda.prop[i] <- rnorm(1, lambda.current[i], 0.2)
    
    
    #Compute log acceptance probability
    log.likelihood.prop <- log.likelihood(lambda.prop, win.matrix)
    log.p.acc <- log.likelihood.prop - log.likelihood.current +
      dnorm(lambda.prop[i], 0, 10, log = TRUE) - dnorm(lambda.current[i], 0, 10, log = TRUE)
    
    #Accept or reject based on acceptance probability 
    if(log(runif(1)) < log.p.acc){
      lambda.current <- lambda.prop #Update value of lambda
      log.likelihood.current <- log.likelihood.prop #Update value of loglikelihood
      counter <- counter + 1
    }
    
  }
  
  lambda.store[, j] <- lambda.current
  # print(j)
}


# Check Trace Plots -------------------------------------------------------
plot(lambda.store[3, ], type = 'l')
plot(lambda.store[9, ], type = 'l')
plot(lambda.store[17, ], type = 'l')
plot(lambda.store[20, ], type = 'l')

# Compute Summary Statistics of Posterior ---------------------------------
#Compute and report posterior means and uncertainty
posterior.means       <- apply(lambda.store[, -c(1:100)], 1, mean)
posterior.ci.upper    <- apply(lambda.store[, -c(1:100)], 1, quantile, 0.975)
posterior.ci.lower    <- apply(lambda.store[, -c(1:100)], 1, quantile, 0.025)

#Order from highest to lowest risk
results.df <- data.frame("name" = wards$name, posterior.means, posterior.ci.upper, posterior.ci.lower)
results.df <- results.df[order(results.df$posterior.means), ]


#Round estimates to 2dp for report and output latex table
latex.df <- data.frame(lapply(results.df, function(x) if(is.numeric(x)) round(x, 2) else x))
knitr::kable(latex.df, "latex")

#Plot results for report
plot(results.df$posterior.means, ylab = "Posterior Mean Risk", xlab = "Wards Ordered By Risk", ylim = c(-1, 5))
segments(x0= 1:N, y0 = results.df$posterior.ci.upper, y1 = results.df$posterior.ci.lower)



