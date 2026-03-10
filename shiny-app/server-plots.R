output$s_obs_ui <- renderUI({
  sliderInput(
    inputId = "s_obs",
    label = "Observed number of successes",
    min = 0,
    max = input$sample_size,
    value = safe_default_s_obs(input$sample_size, input$p0),
    step = 1
  )
})

observeEvent(list(input$sample_size, input$p0), {
  updateSliderInput(
    session = session,
    inputId = "s_obs",
    max = input$sample_size,
    value = safe_default_s_obs(input$sample_size, input$p0)
  )
}, ignoreInit = TRUE)

sim_results <- eventReactive(input$run_sim, {
  req(input$s_obs)
  set.seed(input$seed)
  
  pal <- get_palette(input$palette_name)
  alpha_num <- as.numeric(input$alpha)
  check <- assumption_check(input$sample_size, input$p0)
  
  pval_out <- mc_pval_binom(
    N = input$N,
    p_0 = input$p0,
    s_obs = input$s_obs,
    sample_size = input$sample_size,
    alternative = input$alternative
  )
  
  null_pvals <- mc_pval_null(
    N = input$N,
    p_0 = input$p0,
    sample_size = input$sample_size,
    alternative = input$alternative
  )
  
  type1_out <- mc_type1_binom(
    N = input$N,
    p_0 = input$p0,
    sample_size = input$sample_size,
    alpha = alpha_num,
    alternative = input$alternative
  )
  
  power_out <- mc_power_simple_binom(
    N = input$N,
    p_0 = input$p0,
    p_1 = input$p1,
    alpha = alpha_num,
    sample_size = input$sample_size,
    alternative = input$alternative
  )
  
  power_curve <- mc_power_curve_binom(
    N = input$N,
    p_0 = input$p0,
    alpha = alpha_num,
    sample_size = input$sample_size,
    alternative = input$alternative
  )
  
  n_grid <- unique(round(seq(10, 500, length.out = 25)))
  sample_size_curve <- make_sample_size_curve(
    N = max(2000, round(input$N / 2)),
    p0 = input$p0,
    p1 = input$p1,
    alpha = alpha_num,
    alternative = input$alternative,
    n_grid = n_grid
  )
  
  list(
    palette = pal,
    alpha_num = alpha_num,
    check = check,
    pval_out = pval_out,
    null_pvals = null_pvals,
    type1_out = type1_out,
    power_out = power_out,
    power_curve = power_curve,
    sample_size_curve = sample_size_curve
  )
}, ignoreInit = FALSE)

output$summary_table <- renderTable({
  make_summary_df(sim_results())
}, digits = 4)

output$pvalue_table <- renderTable({
  make_pvalue_df(sim_results(), input$s_obs, input$sample_size)
}, digits = 5)

output$assumption_alert <- renderUI({
  check <- sim_results()$check
  
  if (check$ok) {
    div(
      class = "alert alert-success",
      tags$strong("Looks fine. "),
      check$text
    )
  } else {
    div(
      class = "alert alert-warning",
      tags$strong("Caution. "),
      check$text
    )
  }
})

output$power_curve_head <- renderTable({
  head(sim_results()$power_curve, 10)
}, digits = 4)

output$null_hist <- renderPlot({
  res <- sim_results()
  pal <- res$palette
  df <- data.frame(p_value = res$null_pvals)
  
  ggplot(df, aes(x = p_value)) +
    annotate(
      "rect",
      xmin = 0,
      xmax = res$alpha_num,
      ymin = 0,
      ymax = Inf,
      alpha = 0.20,
      fill = pal$reject_fill
    ) +
    geom_histogram(bins = 30, fill = pal$hist_fill, color = "black", alpha = 0.80) +
    geom_vline(
      xintercept = res$alpha_num,
      color = pal$alpha_line,
      linetype = "dashed",
      linewidth = 1
    ) +
    labs(
      title = "Histogram of simulated null p-values",
      subtitle = paste(
        "n =", input$sample_size,
        "| p0 =", input$p0,
        "|", pretty_alternative(input$alternative), "test"
      ),
      x = "P-value under H0",
      y = "Frequency",
      caption = paste(
        "Estimated proportion of p-values below alpha =",
        round(mean(res$null_pvals < res$alpha_num), 4)
      )
    ) +
    theme_minimal(base_size = 13)
})

output$power_curve_plot <- renderPlot({
  res <- sim_results()
  pal <- res$palette
  pc <- res$power_curve
  
  ggplot(pc, aes(x = p_true, y = power_hat)) +
    geom_line(color = pal$power_line, linewidth = 1.2) +
    geom_errorbar(
      aes(
        ymin = pmax(0, power_hat - 2 * power_mc_se),
        ymax = pmin(1, power_hat + 2 * power_mc_se)
      ),
      width = 0.005,
      alpha = 0.40
    ) +
    geom_vline(xintercept = input$p0, linetype = "dashed") +
    geom_hline(yintercept = res$alpha_num, linetype = "dashed", color = pal$alpha_line) +
    labs(
      title = "Monte Carlo power curve",
      subtitle = paste(
        "n =", input$sample_size,
        "| p0 =", input$p0,
        "| alpha =", res$alpha_num,
        "|", pretty_alternative(input$alternative), "test"
      ),
      x = "True proportion",
      y = "Power"
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_minimal(base_size = 13)
})

output$sample_size_plot <- renderPlot({
  res <- sim_results()
  pal <- res$palette
  ss <- res$sample_size_curve
  
  ggplot(ss, aes(x = sample_size, y = power_hat)) +
    geom_line(color = pal$power_line, linewidth = 1.2) +
    geom_errorbar(
      aes(
        ymin = pmax(0, power_hat - 2 * power_mc_se),
        ymax = pmin(1, power_hat + 2 * power_mc_se)
      ),
      width = 4,
      alpha = 0.35
    ) +
    geom_hline(yintercept = 0.80, linetype = "dashed", color = pal$alpha_line) +
    annotate(
      "text",
      x = min(ss$sample_size),
      y = 0.83,
      label = "Target power: 0.80",
      hjust = 0,
      color = pal$alpha_line
    ) +
    labs(
      title = "Sample size versus power",
      subtitle = paste(
        "Using p1 =", input$p1,
        "| p0 =", input$p0,
        "|", pretty_alternative(input$alternative), "test"
      ),
      x = "Sample size (n)",
      y = "Power"
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_minimal(base_size = 13)
})
