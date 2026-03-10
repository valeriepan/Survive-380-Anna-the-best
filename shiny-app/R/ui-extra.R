intro_card <- card(
  full_screen = FALSE,
  card_header("Project overview"),
  card_body(
    tags$p(
      "This website prototype uses your Monte Carlo functions to estimate the p-value for observed data,",
      "simulate null p-values, estimate Type I error, estimate Type II error and power, and display a power curve",
      "plus a sample-size planner."
    ),
    tags$p(
      "The normal approximation check shown below follows the usual rule of thumb based on $np_0$ and $n(1-p_0)$."
    )
  )
)

help_panel <- tagList(
  tags$h4("How to read the outputs"),
  tags$ul(
    tags$li("Observed p-value tab: computes the Monte Carlo p-value for the selected observed number of successes."),
    tags$li("Null p-value histogram: shows the empirical p-value distribution under the null model."),
    tags$li("Power curve: shows estimated power as the true proportion moves away from p0."),
    tags$li("Sample size planner: shows how power changes with n for the chosen p1.")
  ),
  tags$hr(),
  tags$p(
    strong("Tiny gremlin warning:"),
    " when p0 is very close to 0 or 1 and n is small, the normal approximation can wobble."
  )
)
