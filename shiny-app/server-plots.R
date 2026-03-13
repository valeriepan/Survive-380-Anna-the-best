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
}, ignoreInit = TRUE)

output$summary_boxes <- renderUI({
  res <- sim_results()
  
  pval_est <- sprintf("%.4f", res$pval_out$p_value)
  pval_se  <- sprintf("%.4f", res$pval_out$mc_se)
  
  type1_est <- sprintf("%.4f", res$type1_out$type1_hat)
  type1_se  <- sprintf("%.4f", res$type1_out$mc_se)
  
  type2_est <- sprintf("%.4f", res$power_out$type2_hat)
  type2_se  <- sprintf("%.4f", res$power_out$type2_mc_se)
  
  power_est <- sprintf("%.4f", res$power_out$power_hat)
  power_se  <- sprintf("%.4f", res$power_out$power_mc_se)
  
  layout_columns(
    col_widths = c(6, 6),
    
    value_box(
      title = "Monte Carlo p-value",
      value = pval_est,
      paste("SE:", pval_se),
      theme = "info"
    ), 
    value_box(
      title = "Power",
      value = power_est,
      paste("SE:", power_se),
      theme = "primary"
    ),
    value_box(
      title = "Type I error rate",
      value = type1_est,
      paste("SE:", type1_se),
      theme = "danger"
    ),
    value_box(
      title = "Type II error rate",
      value = type2_est,
      paste("SE:", type2_se),
      theme = "warning"
    )
  )
  })

output$pvalue_table <- renderTable({
  make_pvalue_df(sim_results(), input$s_obs, input$sample_size)
}, digits = 5, width = "100%")



output$power_curve_head <- renderTable({
  head(sim_results()$power_curve, 10)
}, digits = 4, width = "100%", align = "c")

output$null_hist <- renderPlotly({
  res <- sim_results()
  pal <- res$palette
  df <- data.frame(p_value = res$null_pvals)
  
  hist_data <- hist(df$p_value, breaks = 30, plot = FALSE)
  max_count <- max(hist_data$counts)
  y_limit <- max_count * 1.15
  
  p <- ggplot(df, aes(x = p_value)) +
    annotate(
      "rect",
      xmin = 0,
      xmax = res$alpha_num,
      ymin = 0,
      ymax = input$N,
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
    coord_cartesian(ylim = c(0, y_limit)) +
    theme_minimal(base_size = 13)+theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  ggplotly(p) %>% 
    layout(margin = list(t = 90)) %>%
    config(displaylogo = FALSE)
  
})

output$power_curve_plot <- renderPlotly({
  res <- sim_results()
  pal <- res$palette
  pc <- res$power_curve
  
 p <- ggplot(pc, aes(x = p_true, y = power_hat)) +
    geom_line(aes(group = 1), color = pal$power_line, linewidth = 1.2) +
    geom_ribbon(
      aes(
        ymin = pmax(0, power_hat - 2 * power_mc_se),
        ymax = pmin(1, power_hat + 2 * power_mc_se)
      ),
      fill = pal$power_line,
      alpha = 0.35,
      color = NA
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
    theme_minimal(base_size = 13)+
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  ggplotly(p, tooltip = c("x", "y")) %>% 
    layout(margin = list(t = 70))
})

output$sample_size_plot <- renderPlotly({
  res <- sim_results()
  pal <- res$palette
  ss <- res$sample_size_curve
  
  p <- ggplot(ss, aes(x = sample_size, y = power_hat)) +
    geom_line(aes(group = 1), color = pal$power_line, linewidth = 1.2) +
    geom_ribbon(
      aes(
        ymin = pmax(0, power_hat - 2 * power_mc_se),
        ymax = pmin(1, power_hat + 2 * power_mc_se)
      ),
      fill = pal$power_line,
      alpha = 0.35,
      color = NA
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
    theme_minimal(base_size = 13)+
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  ggplotly(p, tooltip = c("x", "y")) %>% 
    layout(margin = list(t = 90)) %>%
    config(displaylogo = FALSE)
})
