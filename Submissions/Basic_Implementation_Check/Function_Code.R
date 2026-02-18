# A basic version, without any functions yet
# add some adjustable parameters when we make the function

# note on parameters in the function: 
# N: number of simulated values (Simulations)
# sample_size: total trials in binomial (Sample Size per Simulation)
# p_0: null hypothesis

# This function calculates the type I error rate using Monte Carlo simulated binomial samples, 
# returns the estimated type I error rate and Monte Carlo standard error

mc_type1_binom <- function(N = 10^5, 
                           sample_size,
                           p_0, 
                           alpha = 0.05, 
                           alternative = c("greater", "less", "not_equal")) { # provided default variables
  alternative <- match.arg(alternative)
  
  # 1. simulation under H_0 : x ~ Bin(sample_size, p_0) 
  x <- rbinom(N, size = sample_size, prob = p_0)
  
  # 2. compute z-statistic under normal approximation
  p_hat <- x / n # sample proportion, sample successes / number of bin trials
  z <- (p_hat - p_0)/sqrt((p_0 * (1 - P_0))/n) # computing z-statistic
  
  # 3. Rejection rule based on the chosen alternative
  reject <- switch(
    alternative,
    greater   = z >  qnorm(1 - alpha),
    less      = z <  qnorm(alpha),
    not_equal = abs(z) > qnorm(1 - alpha / 2)
  )
  
  # 4. Return the estimated type I error rate and Monte Carlo standard error of estimated type I error rate
  return(list(type1_hat <- mean(reject), # Estimated type I error rate
         mc_se <- sqrt(type1_hat * (1 - type1_hat) / m))) # SE for type I error
  # in case we need this
}



# note on parameters in the function: 
# N: number of simulated values (Simulations)
# sample_size: total trials in binomial (Sample Size per Simulation)
# p_0: null hypothesis

# This function calculates the type II error rate and power of the test
# using Monte Carlo simulated binomial samples, 
# returns the estimated type II error rate, power of the test and Monte Carlo standard error for both

# Notice : this function calculate type II error rate and power when alternative hypothesis is well-define(i.e. p = p_1)
#if the alternative is not well-define, we have to compute the whole power curve !!!
mc_power_simple_binom <- function(N, p_0, p_1, 
                                  alpha = 0.05, sample_size = 10^5,
                                  alternative = c("greater", "less", "two.sided")) {
  alternative <- match.arg(alternative)
  
  # 1. Simulate under the alternative truth: x ~ Bin(sample_size, p_1)
  x <- rbinom(m, size = n, prob = p1)
  
  # 2. compute z-statistic under normal approximation
  p_hat <- x / n # sample proportion, sample successes / number of bin trials
  z <- (p_hat - p_0)/sqrt((p_0 * (1 - P_0))/n) # computing z-statistic
  
  # 3. Rejection rule based on the chosen alternative
  reject <- switch(
    alternative,
    greater   = z >  qnorm(1 - alpha),
    less      = z <  qnorm(alpha),
    two.sided = abs(z) > qnorm(1 - alpha / 2)
  )
  
  power_hat <- mean(reject)
  type2_hat <- 1 - power_hat
  
  # Monte Carlo standard errors
  return(list(power_mc_se <- sqrt(power_hat * (1 - power_hat) / m), # SE for estimated power
              type2_mc_se <- power_mc_se))  # SE for estimated type II error
  
}
