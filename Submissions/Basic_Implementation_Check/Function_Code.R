

# A basic version, without any functions yet
# add some adjustable parameters when we make the function

# note on parameters in the function: 
# N: number of simulated values
# n: total trials in binomial 
# p_true: true probability of success in the binomial population
# p_0: null hypothesis

# This function calculates the p-values of Monte Carlo simulated binomial samples, 
# returns the pvalues, results of hypothesis test (in rejection), type 1 error rate, type 2 error rate and power

MCsim_binom <- function(seed = 1, N = 10000, n = 100, p_true = 0.5, p_0 = 0.5, alpha = 0.05) { # provided default values
  set.seed(seed)
  
  # 1. generate data 
  x <- rbinom(N, n, p_true) # draw a random sample x with n number of simulated binomial values
  
  # 2. Calculate p_hat for ALL samples (vectorized -- we don't need a for loop in this case)
  p_hat <- x / n # sample proportion, sample successes / number of bin trials
  
  # 3. Calculate z for ALL samples (vectorized)
  se_0 <- sqrt(p_0 * (1 - p_0) / n) # standard error
  z <- (p_hat - p_0) / se_0 # z-test stat

  # 4. Calculate p-values for ALL samples (vectorized)
  p_val <- 2 * (1 - pnorm(abs(z))) 
  
  # 5. apply "Hit or Miss" (check rejection of null hypothesis)
  rejections <- (p_val < alpha)
  
  # 6. calculate error rates and power: 
  
  type_i_error_rate <- mean(rejections) 
  # type 1 error rate (rejection rate)# when p_true = p_0
  power <- mean(rejections) # when p_true does not equal p_0 
  type_ii_error_rate <- 1 - power
  
  return(list(p_values = p_val_vec, rejections = rejections, type_i_error_rate = type_i_error_rate, power = power, type_ii_error_rate = type_ii_error_rate))
  
}

# Checking that the functions work: 
head(MCsim_binom()$p_values)
head(MCsim_binom()$rejections)