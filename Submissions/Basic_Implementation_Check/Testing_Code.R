# test-binomial-mc.R
# Monte Carlo Binomial Z-Test - testthat checks

library(testthat)

# IMPORTANT:
# Before running this file, set your working directory to:
# Survive-380-Anna-the-best/Submissions/Basic_Implementation_Check
# so that both files are in the same folder.

source("Function_Code.R")

################
# mc_pval_binom
###############

test_that("mc_pval_binom returns correct structure and valid probabilities", {
  
  set.seed(123)
  
  out <- mc_pval_binom(
    N = 20000,
    p_0 = 0.30,
    s_obs = 9,
    sample_size = 30,
    alternative = "greater"
  )
  
  expect_type(out, "list")
  expect_named(out, c("p_value", "mc_se"))
  
  expect_true(out$p_value >= 0 && out$p_value <= 1)
  expect_true(out$mc_se >= 0)
  
  # Monte Carlo SE formula check
  expect_equal(
    out$mc_se,
    sqrt(out$p_value * (1 - out$p_value) / (20000 + 1)),
    tolerance = 1e-12
  )
})

test_that("mc_pval_binom detects extreme results appropriately", {
  
  set.seed(123)
  
  # All successes under 'greater'
  out_high <- mc_pval_binom(
    N = 30000,
    p_0 = 0.30,
    s_obs = 30,
    sample_size = 30,
    alternative = "greater"
  )
  
  expect_lt(out_high$p_value, 0.001)
  
  # No successes under 'less'
  out_low <- mc_pval_binom(
    N = 30000,
    p_0 = 0.30,
    s_obs = 0,
    sample_size = 30,
    alternative = "less"
  )
  
  expect_lt(out_low$p_value, 0.001)
})

# ##############
# mc_type1_binom
# #############

test_that("mc_type1_binom approximates alpha under null", {
  
  set.seed(42)
  
  alpha <- 0.05
  
  out <- mc_type1_binom(
    N = 30000,
    p_0 = 0.40,
    sample_size = 120,
    alpha = alpha,
    alternative = "greater"
  )
  
  expect_named(out, c("type1_hat", "mc_se"))
  
  expect_true(out$type1_hat >= 0 && out$type1_hat <= 1)
  expect_true(out$mc_se >= 0)
  
  # Should be close to alpha
  expect_equal(out$type1_hat, alpha, tolerance = 0.05)
  
  # MC SE formula check
  expect_equal(
    out$mc_se,
    sqrt(out$type1_hat * (1 - out$type1_hat) / 30000),
    tolerance = 1e-12
  )
})

# #####################
# mc_power_simple_binom
# #####################

test_that("mc_power_simple_binom returns internally consistent power and type II", {
  
  set.seed(99)
  
  out <- mc_power_simple_binom(
    N = 25000,
    p_0 = 0.50,
    p_1 = 0.62,
    alpha = 0.05,
    sample_size = 80,
    alternative = "greater"
  )
  
  expect_named(out,
               c("power_hat", "type2_hat",
                 "power_mc_se", "type2_mc_se"))
  
  expect_true(out$power_hat >= 0 && out$power_hat <= 1)
  expect_true(out$type2_hat >= 0 && out$type2_hat <= 1)
  
  # Power + Type II consistency
  expect_equal(out$type2_hat, 1 - out$power_hat, tolerance = 1e-12)
  expect_equal(out$type2_mc_se, out$power_mc_se, tolerance = 1e-12)
  
  # Since p1 > p0 and alternative = greater,
  # power should be meaningfully larger than alpha
  expect_gt(out$power_hat, 0.20)
})

# ####################
# mc_power_curve_binom
# ####################

test_that("mc_power_curve_binom returns correct structure and sensible trend", {
  
  set.seed(202)
  
  alpha <- 0.05
  p0 <- 0.30
  
  curve <- mc_power_curve_binom(
    N = 3000,
    p_0 = p0,
    alpha = alpha,
    sample_size = 100,
    alternative = "greater"
  )
  
  expect_s3_class(curve, "data.frame")
  
  expect_named(
    curve,
    c("p_true", "power_hat", "type2_hat",
      "power_mc_se", "type2_mc_se")
  )
  
  expect_equal(nrow(curve), 31)
  
  # First row corresponds to null
  expect_equal(curve$power_hat[1], alpha, tolerance = 0.05)
  
  # Power should increase with p_true
  expect_gt(cor(curve$p_true, curve$power_hat), 0.75)
  
  # Internal consistency
  expect_equal(curve$type2_hat,
               1 - curve$power_hat,
               tolerance = 1e-12)
})