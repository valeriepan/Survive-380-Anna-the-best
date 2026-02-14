

# A basic version, without any functions yet
# add some adjustable parameters when we make the function

# note on parameters in the function: 
# N: number of simulated values (Simulations)
# n: total trials in binomial (Sample Size per Simulation)
# p_true: true probability of success in the binomial population
# p_0: null hypothesis

# This function calculates the p-values of Monte Carlo simulated binomial samples, 
# returns the pvalues, results of hypothesis test (in rejection), type 1 error rate, type 2 error rate and power

MCsim_binom <- function(seed = 1, N = 10000, n = 100, p_true = 0.5, p_0 = 0.5, alpha = 0.05, alternative = "two.sided") { # provided default values

  set.seed(seed)

  # 1. generate data 
  x <- rbinom(N, n, p_true) # draw a random sample x with n number of simulated binomial values
  
  # 2. Calculate p_hat for ALL samples (vectorized -- we don't need a for loop in this case)
  p_hat <- x / n # sample proportion, sample successes / number of bin trials
  
  # 3. Calculate z for ALL samples (vectorized)
  se_0 <- sqrt(p_0 * (1 - p_0) / n) # standard error
  z <- (p_hat - p_0) / se_0 # z-test stat
  
  # 4. Calculate p-values for ALL samples (vectorized)
  if (alternative == "two.sided") {
    p_val <- 2 * (1 - pnorm(abs(z)))
  } else if (alternative == "less") {
    p_val <- pnorm(z)
  } else if (alternative == "greater") {
    p_val <- 1 - pnorm(z)
  }
  
  # 5. apply "Hit or Miss" (check rejection of null hypothesis)
  rejections <- (p_val < alpha)
  
  # 6. calculate error rates and power based on truth
  if(p_true == p_0) {
    # H0 is True: Calculate Type I Error
    type_i_error_rate <- mean(rejections)
    power <- NULL
    type_ii_error_rate <- NULL
    
  } else {
    # H0 is False: Calculate Power and Type II Error
    power <- mean(rejections)
    type_ii_error_rate <- 1 - power
    type_i_error_rate <- NULL

  }

  return(list(
    type_i_error = type_i_error_rate, 
    power = power, 
    type_ii_error = type_ii_error_rate, 
    p_values = p_val,      
    rejections = rejections,
    z_scores = z           
  ))
}

res1 <- MCsim_binom(p_true = 0.5, p_0 = 0.5, alternative = "two.sided")
print(paste("Type I Error:", res1$type_i_error))

res2 <- MCsim_binom(p_true = 0.6, p_0 = 0.5, alternative = "greater")
print(paste("Power:", res2$power))