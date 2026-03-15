output$s_obs_ui <- renderUI({
  current_s_obs <- isolate(input$s_obs)
  default_s_obs <- safe_default_s_obs(input$sample_size, input$p0)
  slider_value <- if (is.null(current_s_obs)) {
    default_s_obs
  } else {
    min(current_s_obs, input$sample_size)
  }
  
  sliderInput(
    inputId = "s_obs",
    label = "Observed number of successes",
    min = 0,
    max = input$sample_size,
    value = slider_value,
    step = 1
  )
})

format_pval_box <- function(x) {
  if (is.na(x)) {
    "NA"
  } else if (x < 1e-4) {
    "< 0.0001"
  } else {
    sprintf("%.4f", x)
  }
}

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
}, ignoreInit = FALSE)

output$summary_boxes <- renderUI({
  res <- sim_results()
  
  pval_est <- format_pval_box(res$pval_out$p_value)
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

output$assumption_alert <- renderUI({
  check <- sim_results()$check
  
  if (check$ok) {
    div(
      class = "alert text-white", 
      style = "background-color: #2C3E50;",
      tags$strong("Nice! "),
      check$text
    )
  } else {
    div(
      class = "alert alert-warning",
      tags$strong("Warning! "),
      check$text
    )
  }
})

output$power_curve_head <- renderUI({
  pc <- head(sim_results()$power_curve, 10)
  
  display_df <- data.frame(
    `True proportion` = sprintf("%.3f", pc$p_true),
    `Estimated power` = sprintf("%.4f", pc$power_hat),
    `Estimated Type II error` = sprintf("%.4f", pc$type2_hat),
    `Power MC SE` = sprintf("%.4f", pc$power_mc_se),
    `Type II MC SE` = sprintf("%.4f", pc$type2_mc_se),
    check.names = FALSE
  )
  
  header_cells <- lapply(
    names(display_df),
    function(lbl) tags$th(
      lbl,
      style = paste(
        "background-color: #f8f9fa;",
        "color: #2C3E50;",
        "font-weight: 600;",
        "text-align: center;",
        "white-space: nowrap;"
      )
    )
  )
  
  body_rows <- lapply(seq_len(nrow(display_df)), function(i) {
    row_vals <- as.list(display_df[i, , drop = TRUE])
    tags$tr(
      lapply(row_vals, function(val) tags$td(val, style = "text-align: center;"))
    )
  })
  
  div(
    style = "border: 1px solid #dee2e6; border-radius: 8px; overflow-x: auto; background-color: white;",
    tags$table(
      class = "table table-sm table-striped table-hover mb-0 align-middle",
      style = "margin-bottom: 0;",
      tags$thead(tags$tr(header_cells)),
      tags$tbody(body_rows)
    )
  )
})

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
    theme_minimal(base_size = 13) +
    theme(
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
    theme_minimal(base_size = 13) +
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
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  ggplotly(p, tooltip = c("x", "y")) %>% 
    layout(margin = list(t = 90)) %>%
    config(displaylogo = FALSE)
})

output$teaching_mode <- renderUI({
  req(input$s_obs)
  
  phat <- input$s_obs / input$sample_size
  
  alt_symbol <- switch(
    input$alternative,
    "less" = "<",
    "greater" = ">",
    "two_sided" = "\u2260"
  )
  
  alt_text_long <- switch(
    input$alternative,
    "less" = "the true proportion is smaller than the null value",
    "greater" = "the true proportion is greater than the null value",
    "two_sided" = "the true proportion is different from the null value"
  )
  
  res <- tryCatch(sim_results(), error = function(e) NULL)
  
  # Optional result text
  result_block <- if (is.null(res)) {
    tags$div(
      class = "app-alert app-alert-warning",
      style = "margin-top: 1rem;",
      tags$strong("No simulation results yet. "),
      "Click the Run button to generate the Monte Carlo p-value, power, and final decision."
    )
  } else {
    decision_text <- if (res$pval_out$p_value < res$alpha_num) {
      "Reject H<sub>0</sub>"
    } else {
      "Fail to reject H<sub>0</sub>"
    }
    
    plain_english <- if (res$pval_out$p_value < res$alpha_num) {
      "The observed result would be quite unusual if the null hypothesis were true, so we have evidence against H<sub>0</sub>."
    } else {
      "The observed result is not unusual enough under the null hypothesis, so we do not have strong enough evidence against H<sub>0</sub>."
    }
    
    tags$div(
      class = "app-alert app-alert-success",
      style = "margin-top: 1rem;",
      tags$h5("4. Final interpretation", style = "margin-bottom: 0.6rem; font-weight: 700;"),
      tags$p(
        HTML(sprintf(
          "The Monte Carlo estimated p-value is <b>%.4f</b> and the significance level is <b>\u03B1 = %.2f</b>.",
          res$pval_out$p_value, res$alpha_num
        ))
      ),
      tags$p(
        HTML(sprintf(
          "Because <b>p-value %s \u03B1</b>, the decision is: <b>%s</b>.",
          if (res$pval_out$p_value < res$alpha_num) "<" else "\u2265",
          decision_text
        ))
      ),
      tags$p(HTML(plain_english)),
      tags$p(
        HTML(
          "<b>Important note for beginners:</b> “Fail to reject H<sub>0</sub>” does <u>not</u> mean that H<sub>0</sub> has been proven true. It only means the data do not provide strong enough evidence against H<sub>0</sub> at the chosen significance level."
        )
      )
    )
  }
  
  tags$div(
    class = "app-alert",
    
    tags$h4("Teaching mode: step-by-step explanation(For Monte Carlo Simulation beginners)", class = "app-title"),
    tags$p(
      class = "app-subtitle",
      "This panel explains what the hypothesis test is doing, what the symbols mean, and how to interpret the result."
    ),
    
    tags$hr(),
    
    tags$h5("1. What question are we testing?", style = "font-weight: 700;"),
    tags$p(
      HTML(sprintf(
        "We are testing whether the population proportion <b>p</b> is equal to <b>%.2f</b> or whether there is evidence that <b>%s</b>.",
        input$p0, alt_text_long
      ))
    ),
    tags$ul(
      tags$li(HTML(sprintf("<b>Null hypothesis H<sub>0</sub>:</b> p = %.2f", input$p0))),
      tags$li(HTML(sprintf("<b>Alternative hypothesis H<sub>a</sub>:</b> p %s %.2f", alt_symbol, input$p0)))
    ),
    tags$p(
      "The null hypothesis is the baseline assumption. In a Monte Carlo test, we simulate data as if this null hypothesis were true, then ask whether the observed result looks unusual."
    ),
    
    tags$hr(),
    
    tags$h5("2. What does the observed sample tell us?", style = "font-weight: 700;"),
    tags$p(
      HTML(sprintf(
        "You observed <b>%d successes</b> out of <b>%d trials</b>.",
        input$s_obs, input$sample_size
      ))
    ),
    tags$ul(
      tags$li(HTML(sprintf("<b>n = %d</b> is the sample size.", input$sample_size))),
      tags$li(HTML(sprintf("<b>x = %d</b> is the observed number of successes.", input$s_obs))),
      tags$li(HTML(sprintf("<b>p̂ = x / n = %d / %d = %.4f</b> is the sample proportion.", input$s_obs, input$sample_size, phat)))
    ),
    tags$p(
      "The sample proportion is your data-based estimate of the true population proportion. It is the proportion actually seen in this sample."
    ),
    
    tags$hr(),
    
    tags$h5("3. How does the Monte Carlo hypothesis test work?", style = "font-weight: 700;"),
    tags$ol(
      tags$li(
        HTML(sprintf(
          "Assume for the moment that <b>H<sub>0</sub> is true</b>, so the true proportion is <b>p = %.2f</b>.",
          input$p0
        ))
      ),
      tags$li(
        HTML(sprintf(
          "Generate many simulated samples from a <b>Binomial(n = %d, p = %.2f)</b> model.",
          input$sample_size, input$p0
        ))
      ),
      tags$li(
        "For each simulated sample, compute how extreme the result is relative to the null hypothesis."
      ),
      tags$li(
        "Count how often the simulated result is at least as extreme as the observed one."
      ),
      tags$li(
        "That proportion is the Monte Carlo p-value."
      )
    ),
    tags$p(
      "In simple language: the p-value tells us how surprising our observed result would be if the null hypothesis were actually true."
    ),
    
    
    result_block,
    
    if (!is.null(res)) {
      tags$div(
        class = "app-alert",
        style = "margin-top: 1rem;",
        tags$h5("Extra learning notes", style = "margin-bottom: 0.6rem; font-weight: 700;"),
        tags$ul(
          tags$li(
            HTML(sprintf(
              "<b>Type I error rate:</b> %.4f. This is the probability of rejecting H<sub>0</sub> when H<sub>0</sub> is actually true.",
              res$type1_out$type1_hat
            ))
          ),
          tags$li(
            HTML(sprintf(
              "<b>Power:</b> %.4f. This is the probability of correctly rejecting H<sub>0</sub> when the true proportion is <b>p<sub>1</sub> = %.2f</b>.",
              res$power_out$power_hat, input$p1
            ))
          ),
          tags$li(
            HTML(sprintf(
              "<b>Type II error rate:</b> %.4f. This is the probability of failing to reject H<sub>0</sub> when the true proportion is actually <b>%.2f</b>.",
              res$power_out$type2_hat, input$p1
            ))
          )
        ),
        tags$p(
          "These quantities are different from the p-value. The p-value is about this specific observed sample. Power and error rates describe the long-run behavior of the testing procedure."
        )
      )
    }
  )
})