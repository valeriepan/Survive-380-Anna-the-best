help_panel <- tagList(
  tags$h4("How to read the outputs"),
  tags$ul(
    tags$li("Observed p-value: Computes the Monte Carlo p-value for the selected observed number of successes."),
    tags$li("Null p-value histogram: Shows the empirical p-value distribution under the null model."),
    tags$li("Power curve: Shows estimated power as the true proportion moves away from p₀."),
    tags$li("Sample size and Power: Shows how power of the test changes with sample size for the chosen p₁.")
  ),
  tags$p(strong("How to choose p₁:")),
  tags$ul(
    tags$li("If the alternative is ", tags$code("Greater"), ", choose a value of ", tags$code("p₁"), " larger than ", tags$code("p₀"), "."),
    tags$li("If the alternative is ", tags$code("Less"), ", choose a value of ", tags$code("p₁"), " smaller than ", tags$code("p₀"), "."),
    tags$li("If the alternative is ", tags$code("Two Sided"), ", choose a value of ", tags$code("p₁"), " different from ", tags$code("p₀"), ".")
  )
)