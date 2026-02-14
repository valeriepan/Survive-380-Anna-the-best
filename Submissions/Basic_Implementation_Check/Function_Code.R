#' Scenario A: Verification of Type I Error (犯错率)
#' Goal: When H0 is true, is the rejection rate close to alpha?
verify_type_i_error <- function(N = 10000, n = 100, p_true = 0.5, p_0 = 0.5, alpha = 0.05) {
  
  # 1. Generate Data (Vectorized)
  # 使用 R 的 rbinom(N, n, p_true) 生成 10,000 个实验结果
  X <- rbinom(N, n, p_true)
  
  # 2. Calculate Sample Proportions
  # 计算每一个实验的样本比例 p_hat = X / n
  p_hat <- X / n
  
  # 3. Calculate Z-scores (The Test)
  # 应用公式：Z = (p_hat - p_0) / sqrt(p_0*(1-p_0)/n)
  se_0 <- sqrt(p_0 * (1 - p_0) / n)
  Z <- (p_hat - p_0) / se_0
  
  # 4. Calculate P-values
  # 计算双尾 P-value：P_val = 2 * (1 - pnorm(|Z|))
  P_val <- 2 * (1 - pnorm(abs(Z)))
  
  # 5. Apply "Hit or Miss" (Check Rejections)
  # 创建一个逻辑向量：判断 P_val < alpha 是否成立
  rejections <- P_val < alpha
  
  # 6. Calculate Verification Metric
  # 计算 TRUE 的比例：mean(P_val < 0.05)
  type_i_error_rate <- mean(rejections)
  
  # Verification (Pass/Fail)
  # Allowing a small margin for simulation noise
  if (abs(type_i_error_rate - alpha) < 0.01) {
    
  } else {
    
  }
  
  return(type_i_error_rate)
}

#' Scenario B: Verification of Power & Type II Error (侦测能力)
#' Goal: When H0 is false, check if the rejection rate (Power) is high.
verify_power_type_ii <- function(N = 10000, n = 100, p_true = 0.7, p_0 = 0.5, alpha = 0.05) {
  
  # 1. Generate Data
  # 这次用新的真实概率生成数据：rbinom(N, n, 0.7)
  X <- rbinom(N, n, p_true)
  
  # 2. Calculate Statistics
  # 重复场景 A 中的步骤 2-4
  p_hat <- X / n
  se_0 <- sqrt(p_0 * (1 - p_0) / n)
  Z <- (p_hat - p_0) / se_0
  P_val <- 2 * (1 - pnorm(abs(Z)))
  
  # 3. Apply "Hit or Miss" (Check Rejections)
  # 判断 P_val < 0.05 是否成立
  rejections <- P_val < alpha
  
  # 4. Calculate Verification Metrics
  # Metric 1 (Power): 拒绝原假设的比例 = mean(P_val < 0.05)
  power <- mean(rejections)
  # Metric 2 (Type II Error): 没能拒绝原假设的比例 = 1 - Power
  type_ii_error_rate <- 1 - power
  
  # Verification (Pass/Fail)
  if (power > 0.95) {
    
  } else {
    
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
