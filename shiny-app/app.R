library(shiny)

ui <- navbarPage(
  title = "Monte Carlo Binomial Test App",
  
  tabPanel(
    "Home",
    fluidPage(
      h2("STA380 Monte Carlo Simulation Project"),
      p("This Shiny app demonstrates Monte Carlo methods for binomial proportion tests."),
      tags$ul(
        tags$li("Monte Carlo p-value estimation"),
        tags$li("Null p-value distribution"),
        tags$li("Type I error estimation"),
        tags$li("Simple power calculation"),
        tags$li("Power curve visualization")
      ),
      p("Choose a tab above to run a simulation.")
    )
  ),
  
  tabPanel(
    "Monte Carlo p-value",
    sidebarLayout(
      sidebarPanel(
        numericInput("pval_N", "Number of simulations (N):", value = 5000, min = 100),
        numericInput("pval_p0", "Null hypothesis p0:", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("pval_n", "Sample size:", value = 50, min = 1, step = 1),
        numericInput("pval_sobs", "Observed successes (s_obs):", value = 30, min = 0, step = 1),
        selectInput("pval_alt", "Alternative hypothesis:",
                    choices = c("greater", "less", "two_sided")),
        actionButton("run_pval", "Run Simulation")
      ),
      mainPanel(
        h3("Results"),
        tableOutput("pval_table")
      )
    )
  ),
  
  tabPanel(
    "Null p-value distribution",
    sidebarLayout(
      sidebarPanel(
        numericInput("null_N", "Number of simulations (N):", value = 5000, min = 100),
        numericInput("null_p0", "Null hypothesis p0:", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("null_n", "Sample size:", value = 50, min = 1, step = 1),
        selectInput("null_alt", "Alternative hypothesis:",
                    choices = c("greater", "less", "two_sided")),
        actionButton("run_null", "Run Simulation")
      ),
      mainPanel(
        h3("Histogram of Null p-values"),
        plotOutput("null_hist")
      )
    )
  ),
  
  tabPanel(
    "Type I Error",
    sidebarLayout(
      sidebarPanel(
        numericInput("type1_N", "Number of simulations (N):", value = 5000, min = 100),
        numericInput("type1_p0", "Null hypothesis p0:", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("type1_n", "Sample size:", value = 50, min = 1, step = 1),
        numericInput("type1_alpha", "Alpha:", value = 0.05, min = 0.001, max = 0.999, step = 0.01),
        selectInput("type1_alt", "Alternative hypothesis:",
                    choices = c("greater", "less", "two_sided")),
        actionButton("run_type1", "Run Simulation")
      ),
      mainPanel(
        h3("Results"),
        tableOutput("type1_table")
      )
    )
  ),
  
  tabPanel(
    "Simple Power",
    sidebarLayout(
      sidebarPanel(
        numericInput("power_N", "Number of simulations (N):", value = 5000, min = 100),
        numericInput("power_p0", "Null hypothesis p0:", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("power_p1", "Alternative hypothesis p1:", value = 0.6, min = 0, max = 1, step = 0.01),
        numericInput("power_n", "Sample size:", value = 100, min = 1, step = 1),
        numericInput("power_alpha", "Alpha:", value = 0.05, min = 0.001, max = 0.999, step = 0.01),
        selectInput("power_alt", "Alternative hypothesis:",
                    choices = c("greater", "less", "two_sided")),
        actionButton("run_power", "Run Simulation")
      ),
      mainPanel(
        h3("Results"),
        tableOutput("power_table")
      )
    )
  ),
  
  tabPanel(
    "Power Curve",
    sidebarLayout(
      sidebarPanel(
        numericInput("curve_N", "Number of simulations (N):", value = 3000, min = 100),
        numericInput("curve_p0", "Null hypothesis p0:", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("curve_n", "Sample size:", value = 100, min = 1, step = 1),
        numericInput("curve_alpha", "Alpha:", value = 0.05, min = 0.001, max = 0.999, step = 0.01),
        selectInput("curve_alt", "Alternative hypothesis:",
                    choices = c("greater", "less", "two_sided")),
        actionButton("run_curve", "Run Simulation")
      ),
      mainPanel(
        h3("Power Curve"),
        plotOutput("curve_plot"),
        br(),
        tableOutput("curve_table")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ----------------------------
  # Monte Carlo p-value
  # ----------------------------
  pval_res <- eventReactive(input$run_pval, {
    
    validate(
      need(input$pval_p0 > 0 && input$pval_p0 < 1, "p0 must be between 0 and 1."),
      need(input$pval_n >= 1, "Sample size must be at least 1."),
      need(input$pval_sobs >= 0 && input$pval_sobs <= input$pval_n,
           "Observed successes must be between 0 and sample size.")
    )
    
    mc_pval_binom(
      N = input$pval_N,
      p_0 = input$pval_p0,
      s_obs = input$pval_sobs,
      sample_size = input$pval_n,
      alternative = input$pval_alt
    )
  })
  
  output$pval_table <- renderTable({
    req(pval_res())
    data.frame(
      Statistic = c("Estimated p-value", "Monte Carlo SE"),
      Value = c(pval_res()$p_value, pval_res()$mc_se)
    )
  })
  
  # ----------------------------
  # Null p-value distribution
  # ----------------------------
  null_res <- eventReactive(input$run_null, {
    
    validate(
      need(input$null_p0 > 0 && input$null_p0 < 1, "p0 must be between 0 and 1."),
      need(input$null_n >= 1, "Sample size must be at least 1.")
    )
    
    mc_pval_null(
      N = input$null_N,
      p_0 = input$null_p0,
      sample_size = input$null_n,
      alternative = input$null_alt
    )
  })
  
  output$null_hist <- renderPlot({
    req(null_res())
    hist(
      null_res(),
      main = "Null p-value Distribution",
      xlab = "p-value",
      breaks = 20
    )
    abline(v = 0.05, lty = 2)
  })
  
  # ----------------------------
  # Type I Error
  # ----------------------------
  type1_res <- eventReactive(input$run_type1, {
    
    validate(
      need(input$type1_p0 > 0 && input$type1_p0 < 1, "p0 must be between 0 and 1."),
      need(input$type1_n >= 1, "Sample size must be at least 1."),
      need(input$type1_alpha > 0 && input$type1_alpha < 1, "Alpha must be between 0 and 1.")
    )
    
    mc_type1_binom(
      N = input$type1_N,
      p_0 = input$type1_p0,
      sample_size = input$type1_n,
      alpha = input$type1_alpha,
      alternative = input$type1_alt
    )
  })
  
  output$type1_table <- renderTable({
    req(type1_res())
    data.frame(
      Statistic = c("Type I Error Estimate", "Monte Carlo SE"),
      Value = c(type1_res()$type1_hat, type1_res()$mc_se)
    )
  })
  
  # ----------------------------
  # Simple Power
  # ----------------------------
  power_res <- eventReactive(input$run_power, {
    
    validate(
      need(input$power_p0 > 0 && input$power_p0 < 1, "p0 must be between 0 and 1."),
      need(input$power_p1 > 0 && input$power_p1 < 1, "p1 must be between 0 and 1."),
      need(input$power_n >= 1, "Sample size must be at least 1."),
      need(input$power_alpha > 0 && input$power_alpha < 1, "Alpha must be between 0 and 1.")
    )
    
    mc_power_simple_binom(
      N = input$power_N,
      p_0 = input$power_p0,
      p_1 = input$power_p1,
      alpha = input$power_alpha,
      sample_size = input$power_n,
      alternative = input$power_alt
    )
  })
  
  output$power_table <- renderTable({
    req(power_res())
    data.frame(
      Statistic = c("Power", "Type II Error", "Power MC SE", "Type II MC SE"),
      Value = c(
        power_res()$power_hat,
        power_res()$type2_hat,
        power_res()$power_mc_se,
        power_res()$type2_mc_se
      )
    )
  })
  
  # ----------------------------
  # Power Curve
  # ----------------------------
  curve_res <- eventReactive(input$run_curve, {
    
    validate(
      need(input$curve_p0 > 0 && input$curve_p0 < 1, "p0 must be between 0 and 1."),
      need(input$curve_n >= 1, "Sample size must be at least 1."),
      need(input$curve_alpha > 0 && input$curve_alpha < 1, "Alpha must be between 0 and 1.")
    )
    
    mc_power_curve_binom(
      N = input$curve_N,
      p_0 = input$curve_p0,
      alpha = input$curve_alpha,
      sample_size = input$curve_n,
      alternative = input$curve_alt
    )
  })
  
  output$curve_plot <- renderPlot({
    req(curve_res())
    plot(
      curve_res()$p_true,
      curve_res()$power_hat,
      type = "l",
      lwd = 2,
      xlab = "True p",
      ylab = "Estimated Power",
      main = "Power Curve"
    )
  })
  
  output$curve_table <- renderTable({
    req(curve_res())
    head(curve_res(), 10)
  })
}

shinyApp(ui = ui, server = server)