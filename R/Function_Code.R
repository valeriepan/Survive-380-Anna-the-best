# Monte Carlo Simulation for Binomial Proportion Tests
# Author: Valerie Pan, Tianhong Shen, Junyi Hou, Siling Cheng, Feiyang Xue
# Course: STA 380
# Date: 2026-02-25
#
# This file contains functions for:
# - Monte Carlo p-value estimation
# - Type I error estimation
# - Power calculation (simple alternative)
# - Power curve estimation (composite alternative)
# ==============================================================================
# note on parameters in the function: 
# N: Total simulation times
# sample_size: total trials in binomial (Sample Size per Simulation)
# p_0: null hypothesis
# alpha: critical value
# alternative: a vector of three different possible alternative
# p_1: well-defined alternative hypothesis
# s_obs: number of success you observed in one data set
# ==============================================================================


#' Calculate p_value under a observed data
#' 
#' This function calculates the p_value using Monte Carlo sampling.
#' 
#' @param N Integer. Number of simulated values (Simulations). Default 10^5.
#' @param p_0 Numeric. Null hypothesis probability.
#' @param sample_size Integer. Total trials in binomial (Sample Size per Simulation).
#' @param alternative Character. One of "greater", "less", or "two_sided".
#' @param s_obs Integer. Number of success you observed in one data set.
#' @return the p_value under s_obs and Monte Carlo standard error for p_value
#' @importFrom stats pnorm qnorm rbinom
#' @examples
#' set.seed(1)
#' pvals <- mc_pval_null(10000, p_0 = 0.5,
#'                        sample_size = 50,
#'                        alternative = "two_sided")
#' hist(pvals)
#' @importFrom stats pnorm qnorm rbinom
#' @export
mc_pval_binom <- function(N = 10^5, p_0, s_obs, 
                          sample_size, 
                          alternative = c("greater", "less", "two_sided")){
  alternative <- match.arg(alternative)
  
  p_hat <- s_obs / sample_size
  z_obs <- (p_hat - p_0) / sqrt((p_0 * (1 - p_0)) / sample_size)
  
  x <- rbinom(N, size = sample_size, prob = p_0)
  
  p_hat <- x / sample_size
  z <- (p_hat - p_0)/sqrt((p_0 * (1 - p_0)) / sample_size)
  
  
  pval_hat <- switch(
    alternative,
    greater   = (1 + sum(z >= z_obs)) / (N + 1),
    less      = (1 + sum(z <= z_obs)) / (N + 1),
    two_sided = (1 + sum(abs(z) >= abs(z_obs))) / (N + 1)
  )
  
  mc_se <- sqrt(pval_hat * (1 - pval_hat) / (N + 1))
  
  return(list(
    p_value = pval_hat,
    mc_se = mc_se
  ))
}



# ==============================================================================

#' Calculate null p_value under H_0
#' 
#' This function calculates the p_value using Monte Carlo hypothesis testing
#' 
#' @param N Integer. Number of simulated values (Simulations). Default 10^5.
#' @param p_0 Numeric. Null hypothesis probability.
#' @param sample_size Integer. Total trials in binomial (Sample Size per Simulation).
#' @param alternative Character. One of "greater", "less", or "two_sided".
#' @return A numeric vector of length \code{N} containing the simulated null p-values.
#' @importFrom stats pnorm qnorm rbinom
#'@examples
#' set.seed(1)
#' pvals <- mc_pval_null(10000, p_0 = 0.5,
#'                        sample_size = 50,
#'                        alternative = "two_sided")
#' hist(pvals)
#' 
#' @export
mc_pval_null <- function(N = 10^5, p_0, 
                         sample_size, 
                         alternative = c("greater", "less", "two_sided")) {
  alternative <- match.arg(alternative)
  
  x <- rbinom(N, size = sample_size, prob = p_0)
  
  p_hat <- x / sample_size
  z <- (p_hat - p_0) / sqrt(p_0 * (1-p_0) / sample_size)
  
  if (alternative == "less") {
    p_val <- pnorm(z, lower.tail = TRUE)
    return(p_val)
  }
  if (alternative == "greater") {
    p_val <- pnorm(z, lower.tail = FALSE)
    return(p_val)
  }
  p_val <- 2 * pnorm(-abs(z))
  return(p_val)
}
# ==============================================================================

#' Calculate Type I Error Rate
#' 
#' This function calculates the type I error rate using Monte Carlo simulated binomial samples.
#' 
#' @param N Integer. Number of simulated values (Simulations). Default 10^5.
#' @param p_0 Numeric. Null hypothesis probability.
#' @param sample_size Integer. Total trials in binomial (Sample Size per Simulation).
#' @param alpha Numeric. Critical value. Default 0.05.
#' @param alternative Character. One of "greater", "less", or "two_sided".
#' @return A list containing the estimated type I error rate and Monte Carlo standard error.
#' @examples
#' set.seed(1)
#' mc_type1_binom(N = 20000, p_0 = 0.5, sample_size = 50, alpha = 0.05,
#'               alternative = "two_sided")
#' @importFrom stats pnorm qnorm rbinom
#' @export
mc_type1_binom <- function(N = 10^5, p_0,
                           sample_size, alpha = 0.05, 
                           alternative = c("greater", "less", "two_sided")) {
  alternative <- match.arg(alternative)
  
  x <- rbinom(N, size = sample_size, prob = p_0)
  
  p_hat <- x / sample_size 
  z <- (p_hat - p_0)/sqrt((p_0 * (1 - p_0)) / sample_size) 
  
  reject <- switch(
    alternative,
    greater   = z >  qnorm(1 - alpha),
    less      = z <  qnorm(alpha),
    two_sided = abs(z) > qnorm(1 - alpha / 2)
  )
  
  type1_hat <- mean(reject)
  mc_se <- sqrt(type1_hat * (1 - type1_hat) / N) 
  return(list(
    type1_hat = type1_hat,
    mc_se = mc_se,
    N = N,
    p_0 = p_0,
    sample_size = sample_size,
    alpha = alpha,
    alternative = alternative
  ))
}



#' Calculate simple power
#' 
#' This function calculates the type II error rate and power of the test
#' using Monte Carlo simulated binomial samples.
#' Notice : this function calculate type II error rate and power when alternative hypothesis is well-define(i.e. p = p_1)
#' if the alternative is not well-define, we have to compute the whole power curve !!!
#' 
#' @param N Integer. Number of simulated values (Simulations)
#' @param sample_size Integer. Total trials in binomial (Sample Size per Simulation)
#' @param p_0 Numeric. Null hypothesis
#' @param alpha Numeric. Critical value
#' @param alternative Character. A vector of three different possible alternative
#' @param p_1 Numeric. Well-defined alternative hypothesis
#' 
#' @return the estimated type II error rate, power of the test and Monte Carlo standard error for both
#' @importFrom stats pnorm qnorm rbinom
#' @examples
#' set.seed(1)
#' mc_power_simple_binom(N = 20000, p_0 = 0.5, p_1 = 0.6,
#'                      alpha = 0.05, sample_size = 100,
#'                      alternative = "greater")
#' 
#' @export
mc_power_simple_binom <- function(N = 10^5, p_0, p_1, 
                                  alpha = 0.05, sample_size,
                                  alternative = c("greater", "less", "two_sided")) {
  alternative <- match.arg(alternative)
  
  x <- rbinom(N, size = sample_size, prob = p_1)
  
  p_hat <- x / sample_size 
  z <- (p_hat - p_0)/sqrt((p_0 * (1 - p_0)) / sample_size) 
  
  reject <- switch(
    alternative,
    greater   = z >  qnorm(1 - alpha),
    less      = z <  qnorm(alpha),
    two_sided = abs(z) > qnorm(1 - alpha / 2)
  )
  
  
  power_hat <- mean(reject)
  type2_hat <- 1 - power_hat 
  power_mc_se <- sqrt(power_hat * (1 - power_hat) / N) 
  type2_mc_se <- power_mc_se  
  
  return(list(
    power_hat = power_hat,
    type2_hat = type2_hat,
    power_mc_se = power_mc_se,
    type2_mc_se = type2_mc_se,
    N = N,
    p_0 = p_0,
    p_1 = p_1,
    sample_size = sample_size,
    alpha = alpha,
    alternative = alternative
  ))
  
}



#' Calculate power curve
#' 
#' This function calculates the type II error rate and power of the test
#' using Monte Carlo simulated binomial samples, 
#' Notice : this function calculate power curve when alternative hypothesis is not well-define
#' if the alternative is well-define(i.e. p = p_1), use the function mc_power_simple_binom!!!
#' 
#' @param N Integer. Number of simulated values (Simulations)
#' @param sample_size Integer. Total trials in binomial (Sample Size per Simulation)
#' @param p_0 Numeric. Null hypothesis
#' @param alpha Numeric. Critical value
#' @param alternative Character. A vector of three different possible alternative
#' @return the estimated type II error rate, power of the test and Monte Carlo standard error for both
#' @importFrom stats pnorm qnorm rbinom
#' @examples
#' set.seed(1)
#' pc <- mc_power_curve_binom(N = 5000, p_0 = 0.5, alpha = 0.05,
#'                           sample_size = 100, alternative = "two_sided")
#' head(pc)
#' @export
mc_power_curve_binom <- function(N = 10^5, p_0, 
                                 alpha = 0.05, sample_size,
                                 alternative = c("greater", "less", "two_sided")){
  
  alternative <- match.arg(alternative)
  
  if (alternative == "greater") {
    p_grid <- seq(p_0, min(1, p_0 + 0.30), length.out = 31)
  } else if (alternative == "less") {
    p_grid <- seq(max(0, p_0 - 0.30), p_0, length.out = 31)
  } else {
    p_grid <- seq(max(0, p_0 - 0.30), min(1, p_0 + 0.30), length.out = 61)
  }
  
  k <- length(p_grid)
  
  probs <- rep(p_grid, each = N)
  x_vec <- rbinom(n = N * k, size = sample_size, prob = probs)
  x_mat <- matrix(x_vec, nrow = N, ncol = k)
  
  p_hat <- x_mat / sample_size 
  z_mat <- (p_hat - p_0) / sqrt(p_0 * (1 - p_0) / sample_size) 
  
  reject_mat <- switch(
    alternative,
    greater   = z_mat >  qnorm(1 - alpha),
    less      = z_mat <  qnorm(alpha),
    two_sided = abs(z_mat) > qnorm(1 - alpha / 2)
  )
  
  power_hat <- colMeans(reject_mat) 
  type2_hat <- 1 - power_hat
  
  power_mc_se <- sqrt(power_hat * (1 - power_hat) / N) 
  type2_mc_se <- power_mc_se
  
  power_curve <- data.frame(
    p_true = p_grid,
    power_hat = power_hat,
    type2_hat = type2_hat,
    power_mc_se = power_mc_se,
    type2_mc_se = type2_mc_se
  )
  
  return(power_curve)
}
