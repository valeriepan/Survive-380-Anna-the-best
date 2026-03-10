help_panel <- tagList(
  tags$h4("How to read the outputs"),
  tags$ul(
    tags$li("Observed p-value: Computes the Monte Carlo p-value for the selected observed number of successes."),
    tags$li("Null p-value histogram: Shows the empirical p-value distribution under the null model."),
    tags$li("Power curve: When the alternative hypothesis is not well defined, shows estimated power as the true proportion moves away from p₀."),
    tags$li("Sample size and Power: shows how power of the test changes with sample size for the chosen p₁.")
  ),
  tags$hr(),
  tags$p(
    strong("Notice:"),
    "Please make sure that you are choosing the correct value for p₁ under different alternative choice!"
  )
)
