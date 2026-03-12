library(shiny)
library(ggplot2)
library(bslib)
library(shinycssloaders)

# Global spinner settings
options(spinner.type = 8, spinner.color = "#6990EE")

# -----------------------------------------------------------------------------
# Load project functions.
# -----------------------------------------------------------------------------
required_funs <- c(
  "mc_pval_binom",
  "mc_pval_null",
  "mc_type1_binom",
  "mc_power_simple_binom",
  "mc_power_curve_binom"
)

if (!all(vapply(required_funs, exists, logical(1), mode = "function"))) {
  if (file.exists("Function_Code.R")) {
    source("Function_Code.R")
  } else if (file.exists(file.path("R", "Function_Code.R"))) {
    source(file.path("R", "Function_Code.R"))
  } else {
    stop("Could not find Function_Code.R. Put it in the same folder as app.R or inside an R/ folder.")
  }
}

# -----------------------------------------------------------------------------
# Plot palettes
# -----------------------------------------------------------------------------
palette_choices <- list(
  "Blue / Gold" = list(
    hist_fill = "#4C78A8",
    power_line = "#4C78A8",
    alpha_line = "#E45756",
    reject_fill = "#F2CF5B"
  ),
  "Colorblind Friendly" = list(
    hist_fill = "#1E88E5",
    power_line = "#D81B60",
    alpha_line = "#004D40",
    reject_fill = "#FFC107"
  ),
  "Forest" = list(
    hist_fill = "#2E8B57",
    power_line = "#1B5E20",
    alpha_line = "#B22222",
    reject_fill = "#C5E1A5"
  ),
  "Purple / Teal" = list(
    hist_fill = "#7E57C2",
    power_line = "#009688",
    alpha_line = "#E64A19",
    reject_fill = "#CE93D8"
  ),
  "Academic Grey" = list(
    hist_fill   = "#ADB5BD", 
    alpha_line  = "#212529", 
    power_line  = "#495057", 
    reject_fill = "#E9ECEF" 
  ),
  "Ocean" = list(
    hist_fill   = "#48CAE4", 
    alpha_line  = "#03045E", 
    power_line  = "#0077B6", 
    reject_fill = "#CAF0F8"  
  )
)

get_palette <- function(name) {
  palette_choices[[name]]
}

pretty_alternative <- function(x) {
  switch(
    x,
    greater = "right-tailed",
    less = "left-tailed",
    two_sided = "two-sided",
    x
  )
}

safe_default_s_obs <- function(sample_size, p0) {
  min(round(sample_size * p0), sample_size)
}

assumption_check <- function(n, p0) {
  np0 <- n * p0
  n1p0 <- n * (1 - p0)
  ok <- (np0 >= 10) && (n1p0 >= 10)
  
  list(
    ok = ok,
    np0 = np0,
    n1p0 = n1p0,
    
    # bolded part
    header = if (ok) "Normal approximation works: " else "Sample size is too small! ",
    # details part (not bolded)
    details = paste0("n × p₀ = ", np0, " and n × (1 - p₀) = ", n1p0, 
                     if(ok) ", both at least 10." else ", they should be at least 10.")
    )
}

make_sample_size_curve <- function(N, p0, p1, alpha, alternative, n_grid) {
  power_vals <- numeric(length(n_grid))
  se_vals <- numeric(length(n_grid))
  
  for (i in seq_along(n_grid)) {
    out <- mc_power_simple_binom(
      N = N,
      p_0 = p0,
      p_1 = p1,
      alpha = alpha,
      sample_size = n_grid[i],
      alternative = alternative
    )
    power_vals[i] <- out$power_hat
    se_vals[i] <- out$power_mc_se
  }
  
  data.frame(
    sample_size = n_grid,
    power_hat = power_vals,
    power_mc_se = se_vals
  )
}

make_summary_df <- function(res) {
  data.frame(
    Metric = c("Monte Carlo p-value", "Type I error rate", "Type II error rate", "Power"),
    Estimate = c(
      res$pval_out$p_value,
      res$type1_out$type1_hat,
      res$power_out$type2_hat,
      res$power_out$power_hat
    ),
    "Monte Carlo Standard Error" = c(
      res$pval_out$mc_se,
      res$type1_out$mc_se,
      res$power_out$type2_mc_se,
      res$power_out$power_mc_se
    ),
    check.names = FALSE
  )
}

make_pvalue_df <- function(res, s_obs, sample_size) {
  data.frame(
    Quantity = c("Observed successes", "Observed proportion", "Monte Carlo p-value", "Monte Carlo standard error"),
    Value = c(
      s_obs,
      s_obs / sample_size,
      res$pval_out$p_value,
      res$pval_out$mc_se
    ),
    check.names = FALSE
  )
}
