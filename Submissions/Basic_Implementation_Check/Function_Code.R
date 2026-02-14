


# A basic version, without any functions yet
# add some adjustable parameters when we make the function

# 1. generate n binomial samples
set.seed(1)
n <- 10^4 # number of simulated values

# just chose some random values for now
m <- 10 # total trials in binomial 
p_true <- 0.5 # true probability of success in the binomial population

x <- rbinom(n, size = m, p = p_true) # draw a random sample x with n number of simulated binomial values

p_0 <- 0.7 # define some null hypothesis proportion

# 2. Apply z-test to each sample
for(i in 1:n){
  p_hat <- x[i]/m # sample proportion, probability of sample successes / number of bin trials  
  
  z <- (p_hat - p_0) / sqrt(p_0*(1-p_0)/m) # calculate z-test statistic
  
  # add the hypothesis test here...
  
}

#Verification of Type I Error
#' Goal: When H0 is true, is the rejection rate close to alpha?
verify_type_i_error <- function(N = 10000, n = 100, p_true = 0.5, p_0 = 0.5, alpha = 0.05) {
  
  # 1. Generate Data (Vectorized)
  X <- rbinom(N, n, p_true)
  
  # 2. Calculate Sample Proportions
  p_hat <- X / n
  
  # 3. Calculate Z-scores (The Test)
  se_0 <- sqrt(p_0 * (1 - p_0) / n)
  Z <- (p_hat - p_0) / se_0
  
  # 4. Calculate P-values
  P_val <- 2 * (1 - pnorm(abs(Z)))
  
  # 5. Apply "Hit or Miss" (Check Rejections)
  rejections <- P_val < alpha
  
  # 6. Calculate Verification Metric
  type_i_error_rate <- mean(rejections)
  
  # Verification (Pass/Fail)
  if (abs(type_i_error_rate - alpha) < 0.01) {
    # Rejection rate is close to alpha
  } else {
    # Check for issues
  }
  
  return(type_i_error_rate)
}

#Verification of Power & Type II Error
verify_power_type_ii <- function(N = 10000, n = 100, p_true = 0.7, p_0 = 0.5, alpha = 0.05) {
  
  # 1. Generate Data
  X <- rbinom(N, n, p_true)
  
  # 2. Calculate Statistics
  p_hat <- X / n
  se_0 <- sqrt(p_0 * (1 - p_0) / n)
  Z <- (p_hat - p_0) / se_0
  P_val <- 2 * (1 - pnorm(abs(Z)))
  
  # 3. Apply "Hit or Miss" (Check Rejections)
  rejections <- P_val < alpha
  
  # 4. Calculate Verification Metrics
  power <- mean(rejections)
  type_ii_error_rate <- 1 - power
  
  # Verification (Pass/Fail)
  if (power > 0.95) {
    # Power is high as expected
  } else {
    # Power is unexpectedly low
  }
  
  return(list(power = power, type_ii = type_ii_error_rate))
}

# Run Verifications
set.seed(380) # For reproducibility
type_i_res <- verify_type_i_error()
power_res <- verify_power_type_ii()

# Display results
type_i_res
power_res

