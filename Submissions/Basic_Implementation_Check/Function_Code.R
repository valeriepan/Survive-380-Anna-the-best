# note on parameters in the function: 
# N: Total simulation times
# sample_size: total trials in binomial (Sample Size per Simulation)
# p_0: null hypothesis
# alpha: critical value
# alternative: a vector of three different possible alternative
# p_1: well-defined alternative hypothesis
# s_obs: number of success you observed in one data set



#' Monte Carlo p-value for a binomial z-test with normal approximation
#'
#' Estimate the p-value of a one-sample binomial proportion test using
#' Monte Carlo sampling under the null hypothesis with  normal-approximation
#' of z-statistic.
#'
#' Under the null, we simulate \eqn{X \sim \mathrm{Bin}(n, p_0)} many times,
#' compute the corresponding z-statistics, and estimate the p-value by the
#' fraction of simulated z values at least as extreme as the observed z.
#' A +1 correction is used: \eqn{(1 + \#\{\cdot\})/(N+1)}.
#'
#' @param N Integer. Number of Monte Carlo simulations.
#' @param p_0 Numeric in (0, 1). Null hypothesis proportion.
#' @param s_obs Integer. Observed number of successes in the dataset.
#' @param sample_size Integer. Number of trials in the binomial sample (n).
#' @param alternative Character string specifying the alternative hypothesis:
#'   \code{"greater"}, \code{"less"}, or \code{"two_sided"}.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{p_value}: Estimated Monte Carlo p-value.
#'   \item \code{mc_se}: Monte Carlo standard error of \code{p_value}, computed as
#'   \eqn{\sqrt{\hat p(1-\hat p)/(N+1)}}.
#' }
#'
#' @examples
#' set.seed(1)
#' mc_pval_binom(N = 10000, p_0 = 0.5, s_obs = 60, sample_size = 100,
#'              alternative = "greater")
#'
#' @export
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

#' Monte Carlo estimate of Type I error for a binomial z-test
#'
#' Estimate the Type I error rate of a one-sample binomial proportion z-test
#' with normal approximation via Monte Carlo simulation under the null hypothesis.
#'
#' Under \eqn{H_0: p = p_0}, we simulate \eqn{X \sim \mathrm{Bin}(n, p_0)},
#' compute z-statistics, apply the rejection rule determined by \code{alternative},
#' and estimate \eqn{\alpha} as the fraction of rejections.
#'
#' @param N Integer. Number of Monte Carlo simulations.
#' @param p_0 Numeric in (0, 1). Null hypothesis proportion.
#' @param sample_size Integer. Number of trials in the binomial sample (n).
#' @param alpha Numeric in (0, 1). Nominal significance level.
#' @param alternative Character string specifying the alternative hypothesis:
#'   \code{"greater"}, \code{"less"}, or \code{"two_sided"}.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{type1_hat}: Estimated Type I error rate.
#'   \item \code{mc_se}: Monte Carlo standard error of \code{type1_hat}, computed as
#'   \eqn{\sqrt{\hat\alpha(1-\hat\alpha)/N}}.
#' }
#'
#' @examples
#' set.seed(1)
#' mc_type1_binom(N = 20000, p_0 = 0.5, sample_size = 50, alpha = 0.05,
#'               alternative = "two_sided")
#'
#' @export

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

#' Monte Carlo power and Type II error for a simple point alternative
#'
#' Estimate the power and Type II error rate of a one-sample binomial proportion
#' z-test with normal approximation under a *point* alternative \eqn{p = p_1}.
#'
#' We simulate \eqn{X \sim \mathrm{Bin}(n, p_1)}, compute z-statistics using the
#' null standardization (center at \code{p_0}), apply the rejection rule, and
#' estimate power as the rejection probability.
#'
#' @param N Integer. Number of Monte Carlo simulations.
#' @param p_0 Numeric in (0, 1). Null hypothesis proportion.
#' @param p_1 Numeric in (0, 1). Alternative (true) proportion for simulation.
#' @param alpha Numeric in (0, 1). Nominal significance level.
#' @param sample_size Integer. Number of trials in the binomial sample (n).
#' @param alternative Character string specifying the alternative hypothesis:
#'   \code{"greater"}, \code{"less"}, or \code{"two_sided"}.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{power_hat}: Estimated power.
#'   \item \code{type2_hat}: Estimated Type II error rate (= 1 - power).
#'   \item \code{power_mc_se}: Monte Carlo standard error for \code{power_hat}.
#'   \item \code{type2_mc_se}: Monte Carlo standard error for \code{type2_hat}
#'   (same as \code{power_mc_se}).
#' }
#'
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


#' Monte Carlo power curve for a binomial z-test
#'
#' Estimate the power curve and Type II error curve for a
#' one-sample binomial proportion z-test with normal approximation across a grid
#' of true proportions \code{p_true}. This is used when the alternative is not a
#' single point(welldefined).
#'
#' The function builds a default grid around \code{p_0} (±0.30,limited within [0,1]),
#' simulates \eqn{X \sim \mathrm{Bin}(n, p_{\mathrm{true}})} for each grid point,
#' applies the rejection rule, and returns estimated power with Monte Carlo SEs.
#'
#' @param N Integer. Number of Monte Carlo simulations per grid point.
#' @param p_0 Numeric in (0, 1). Null hypothesis proportion.
#' @param alpha Numeric in (0, 1). Nominal significance level.
#' @param sample_size Integer. Number of trials in the binomial sample (n).
#' @param alternative Character string specifying the alternative hypothesis:
#'   \code{"greater"}, \code{"less"}, or \code{"two_sided"}.
#'
#' @return A data frame with one row per grid value:
#' \itemize{
#'   \item \code{p_true}: True proportion used in simulation.
#'   \item \code{power_hat}: Estimated power at \code{p_true}.
#'   \item \code{type2_hat}: Estimated Type II error (= 1 - power).
#'   \item \code{power_mc_se}: Monte Carlo SE for \code{power_hat}.
#'   \item \code{type2_mc_se}: Monte Carlo SE for \code{type2_hat}.
#' }
#'
#' @examples
#' set.seed(1)
#' pc <- mc_power_curve_binom(N = 5000, p_0 = 0.5, alpha = 0.05,
#'                           sample_size = 100, alternative = "two_sided")
#' head(pc)
#'
#' @export
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









