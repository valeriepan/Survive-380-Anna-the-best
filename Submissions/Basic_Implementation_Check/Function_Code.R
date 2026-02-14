
# A basic version, without any functions yet
# add some adjustable parameters when we make the function

# 1. generate N binomial samples
set.seed(380)
N <- 10^4 # number of simulated values (Simulation Sample Size)
n <- 100  # number of trials per sample (Proposal matches this 'n')
p_true <- 0.5 # true probability of success in the population
x <- rbinom(N, size = n, p = p_true) 

p_0 <- 0.5 # define some null hypothesis proportion
alpha <- 0.05

# 2. Apply z-test to each sample
rejections <- logical(N)
for(i in 1:N){
  p_hat <- x[i]/n 
  
  z <- (p_hat - p_0) / sqrt(p_0*(1-p_0)/n) # calculate z-test statistic
  p_val <- 2 * (1 - pnorm(abs(z))) # calculate p-value
  
  # Determine if the test rejects H0
  rejections[i] <- p_val < alpha
}
type_1_error <- mean(rejections)

library(ggplot2)
library(gridExtra)

#' Run Binomial Z-Test Simulation
#' This function follows the project proposal requirements for flexible simulation control.
#' @param N Number of simulations (e.g., 1000 to 10000)
#' @param n Number of trials per unit (sample size n in proposal, e.g., 10 to 500)
#' @param p_true Success probability for population (user input p)
#' @param p_0 Null hypothesis proportion
#' @param alpha Significance level
#' @param alternative Hypothesis directionality ("two.sided", "greater", "less")
run_z_test_simulation <- function(N = 10000, n = 100, p_true = 0.5, p_0 = 0.5, alpha = 0.05, alternative = "two.sided") {
  # 1. Generate binomial samples
  x <- rbinom(N, n, p_true)
  p_hat <- x / n
  
  # 2. Calculate Z-scores
  se_0 <- sqrt(p_0 * (1 - p_0) / n)
  z <- (p_hat - p_0) / se_0
  
  # 3. Calculate P-values based on directionality
  if (alternative == "two.sided") {
    p_vals <- 2 * (1 - pnorm(abs(z)))
  } else if (alternative == "greater") {
    p_vals <- 1 - pnorm(z) # Probability of being even larger
  } else if (alternative == "less") {
    p_vals <- pnorm(z) # Probability of being even smaller
  }
  
  # 4. Check rejections
  rejections <- p_vals < alpha
  rejection_rate <- mean(rejections)
  
  return(list(
    rejection_rate = rejection_rate,
    p_vals = p_vals,
    p_hat = p_hat
  ))
}

# --- Core Simulation Controls (Simulating User Input) ---

# User Input Settings (Shiny Widget Simulation)
N_sim_input <- 10000
n_trials_input <- 100
p0_input <- 0.5
p_alt_input <- 0.7 # User selects a different p for Power analysis
alpha_input <- 0.05
test_direction <- "two.sided"

# 1. Calculate Type I Error (Set p_true = p0)
type_1_results <- run_z_test_simulation(
  N = N_sim_input, 
  n = n_trials_input, 
  p_true = p0_input, 
  p_0 = p0_input, 
  alpha = alpha_input, 
  alternative = test_direction
)

# 2. Calculate Power (Set p_true = p_alt_input)
power_results <- run_z_test_simulation(
  N = N_sim_input, 
  n = n_trials_input, 
  p_true = p_alt_input, 
  p_0 = p0_input, 
  alpha = alpha_input, 
  alternative = test_direction
)

# Output Summary (as requested in Proposal: Table of Error Rates)
cat("\n--- Simulation Summary ---\n")
cat(sprintf("Type I Error Rate (p=%.2f): %.4f\n", p0_input, type_1_results$rejection_rate))
cat(sprintf("Power (p=%.2f):            %.4f\n", p_alt_input, power_results$rejection_rate))
cat(sprintf("Type II Error Rate:         %.4f\n", 1 - power_results$rejection_rate))

# Visualization
plot_all_results <- function(res_type1, res_power, alpha) {
  
  # Plot 1: P-values for Type I Error (Should be Uniform [0,1])
  p1 <- ggplot(data.frame(p = res_type1$p_vals), aes(x = p)) +
    geom_histogram(breaks = seq(0, 1, by = 0.05), fill = "skyblue", color = "white") +
    geom_vline(xintercept = alpha, color = "red", linetype = "dashed") +
    labs(title = "Type I Error Ref check (H0 True)", subtitle = "P-value Distribution (Expect Uniform)", 
         x = "P-value", y = "Count") +
    theme_minimal()
  
  # Plot 2: P-values for Power (Should be skewed near 0)
  p2 <- ggplot(data.frame(p = res_power$p_vals), aes(x = p)) +
    geom_histogram(breaks = seq(0, 1, by = 0.05), fill = "lightgreen", color = "white") +
    geom_vline(xintercept = alpha, color = "red", linetype = "dashed") +
    labs(title = "Power Check (H0 False)", subtitle = paste("P-value Distribution (Expect Skewed)"), 
         x = "P-value", y = "Count") +
    theme_minimal()
    
  grid.arrange(p1, p2, ncol = 2)
}

png("C:/Users/Lenovo/Documents/Survive-380-Anna-the-best/Submissions/Basic_Implementation_Check/proposal_plots.png", width = 1000, height = 500)
plot_all_results(type_1_results, power_results, alpha_input)
dev.off()
