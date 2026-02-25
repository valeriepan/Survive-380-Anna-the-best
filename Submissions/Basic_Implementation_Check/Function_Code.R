# note on parameters in the function: 
# N: Total simulation times
# sample_size: total trials in binomial (Sample Size per Simulation)
# p_0: null hypothesis
# alpha: critical value
# alternative: a vector of three different possible alternative
# p_1: well-defined alternative hypothesis
# s_obs: number of success you observed in one data set


# This function calculates the p_value using Monte Carlo sampling
# returns the p_value under s_obs and Monte Carlo standard error for p_value
mc_pval_binom <- function(N = 10^5, p_0, s_obs, 
                          sample_size, 
                          alternative = c("greater", "less", "two_sided")){
  alternative <- match.arg(alternative)
  
  # 1. standardize s_obs
  p_hat <- s_obs / sample_size
  z_obs <- (p_hat - p_0) / sqrt((p_0 * (1 - p_0)) / sample_size)
  
  # 2. simulate data under H_0
  x <- rbinom(N, size = sample_size, prob = p_0)
  
  # 3. compute z-statistic under normal approximation
  p_hat <- x / sample_size
  z <- (p_hat - p_0)/sqrt((p_0 * (1 - p_0)) / sample_size)
  
  # 4. Estimating p_value using Monte Carlo sampling
  pval_hat <- switch(
    alternative,
    greater   = (1 + sum(z >= z_obs)) / (N + 1),
    less      = (1 + sum(z <= z_obs)) / (N + 1),
    two_sided = (1 + sum(abs(z) >= abs(z_obs))) / (N + 1)
  )
  
  # 5. Monte Carlo standard error for p_value
  mc_se <- sqrt(pval_hat * (1 - pval_hat) / (N + 1))
  
  # 6. return estimated p_value and Monte Carlo standard error
  return(list(
    p_value = pval_hat,
    mc_se = mc_se
  ))
}



# This function calculates the type I error rate using Monte Carlo simulated binomial samples, 
# returns the estimated type I error rate and Monte Carlo standard error

mc_type1_binom <- function(N = 10^5, p_0,
                           sample_size, alpha = 0.05, 
                           alternative = c("greater", "less", "two_sided")) { # provided default variables
  alternative <- match.arg(alternative)
  
  # 1. simulation data under H_0 : x ~ Bin(sample_size, p_0) 
  x <- rbinom(N, size = sample_size, prob = p_0)
  
  # 2. compute z-statistic under normal approximation
  p_hat <- x / sample_size # sample proportion, sample successes / number of bin trials
  z <- (p_hat - p_0)/sqrt((p_0 * (1 - p_0)) / sample_size) # computing z-statistic
  
  # 3. Rejection rule based on the chosen alternative
  reject <- switch(
    alternative,
    greater   = z >  qnorm(1 - alpha),
    less      = z <  qnorm(alpha),
    two_sided = abs(z) > qnorm(1 - alpha / 2)
  )
  
  # 4. Return the estimated type I error rate and Monte Carlo standard error of estimated type I error rate
  type1_hat <- mean(reject) # Estimated type I error rate
  mc_se <- sqrt(type1_hat * (1 - type1_hat) / N) # SE for type I error(In case we need this)
  
  return(list(
    type1_hat = type1_hat,
    mc_se = mc_se
  ))
}



# This function calculates the type II error rate and power of the test
# using Monte Carlo simulated binomial samples, 
# returns the estimated type II error rate, power of the test and Monte Carlo standard error for both

# Notice : this function calculate type II error rate and power when alternative hypothesis is well-define(i.e. p = p_1)
#if the alternative is not well-define, we have to compute the whole power curve !!!
mc_power_simple_binom <- function(N = 10^5, p_0, p_1, 
                                  alpha = 0.05, sample_size,
                                  alternative = c("greater", "less", "two_sided")) {
  alternative <- match.arg(alternative)
  
  # 1. Simulate under the alternative truth: x ~ Bin(sample_size, p_1)
  x <- rbinom(N, size = sample_size, prob = p_1)
  
  # 2. compute z-statistic under normal approximation
  p_hat <- x / sample_size # sample proportion, sample successes / number of bin trials
  z <- (p_hat - p_0)/sqrt((p_0 * (1 - p_0)) / sample_size) # computing z-statistic
  
  # 3. Rejection rule based on the chosen alternative
  reject <- switch(
    alternative,
    greater   = z >  qnorm(1 - alpha),
    less      = z <  qnorm(alpha),
    two_sided = abs(z) > qnorm(1 - alpha / 2)
  )
  
  
  # 4. return estimated type II error rate, power of the test and Monte Carlo standard errors
  power_hat <- mean(reject) # estimated power
  type2_hat <- 1 - power_hat # estimated type II error rate
  power_mc_se <- sqrt(power_hat * (1 - power_hat) / N) # SE for estimated power
  type2_mc_se <- power_mc_se  # SE for estimated type II error
  
  return(list(
    power_hat = power_hat,
    type2_hat = type2_hat,
    power_mc_se = power_mc_se,
    type2_mc_se = type2_mc_se
  ))
  
}



# This function calculates the type II error rate and power of the test
# using Monte Carlo simulated binomial samples, 
# returns the estimated type II error rate, power of the test and Monte Carlo standard error for both

# Notice : this function calculate power curve when alternative hypothesis is not well-define
#if the alternative is well-define(i.e. p = p_1), use the function mc_power_simple_binom!!!
mc_power_curve_binom <- function(N = 10^5, p_0, 
                                 alpha = 0.05, sample_size,
                                 alternative = c("greater", "less", "two_sided")){
  
  alternative <- match.arg(alternative)
  
  # 1. create default grid for different alternative 
  if (alternative == "greater") {
    p_grid <- seq(p_0, min(1, p_0 + 0.30), length.out = 31)
  } else if (alternative == "less") {
    p_grid <- seq(max(0, p_0 - 0.30), p_0, length.out = 31)
  } else {
    p_grid <- seq(max(0, p_0 - 0.30), min(1, p_0 + 0.30), length.out = 61)
  }
  
  k <- length(p_grid)
  
  # 2. simulate x ~ Bin(sample_size, p_true) for each p_true in p_grid (N repeats each)
  probs <- rep(p_grid, each = N)
  x_vec <- rbinom(n = N * k, size = sample_size, prob = probs)
  x_mat <- matrix(x_vec, nrow = N, ncol = k)
  
  # 3. compute z-statistic under normal approximation
  p_hat <- x_mat / sample_size # sample proportion, sample successes / number of bin trials
  z_mat <- (p_hat - p_0) / sqrt(p_0 * (1 - p_0) / sample_size) # computing z-statistic
  
  # 4. Rejection rule based on the chosen alternative
  reject_mat <- switch(
    alternative,
    greater   = z_mat >  qnorm(1 - alpha),
    less      = z_mat <  qnorm(alpha),
    two_sided = abs(z_mat) > qnorm(1 - alpha / 2)
  )
  
  # 4. compute estimated type II error rate, power of the test and Monte Carlo standard errors for each p_grid
  power_hat <- colMeans(reject_mat) # estimated power
  type2_hat <- 1 - power_hat # estimated type II error rate
  
  power_mc_se <- sqrt(power_hat * (1 - power_hat) / N) # SE of estimated power
  type2_mc_se <- power_mc_se # SE of estimated type II
  
  # 5. create the power curve 
  power_curve <- data.frame(
    p_true = p_grid,
    power_hat = power_hat,
    type2_hat = type2_hat,
    power_mc_se = power_mc_se,
    type2_mc_se = type2_mc_se
  )
  
  return(power_curve)
}









