library(shiny)
library(bsicons) 
library(bslib)

sim_inputs <- div(
  h4("Simulation controls"),
  
  sliderInput(
    inputId = "N",
    label = tooltip(
      trigger = span("Number of Monte Carlo simulations ", bs_icon("info-circle", class = "text-muted")),
      "The number of simulated datasets generated. Higher values increase estimation accuracy but take longer to run."
    ),
    min = 1000,
    max = 10000,
    value = 5000,
    step = 500
  ),
  
  sliderInput(
    inputId = "sample_size",
    label = tooltip(
      trigger = span("Number of trials $n$ ", bs_icon("info-circle", class = "text-muted")),
      "The total number of independent observations in each simulated dataset."
    ),
    min = 10,
    max = 500,
    value = 60,
    step = 5
  ),
  
  sliderInput(
    inputId = "p0",
    label = tooltip(
      trigger = span("Null proportion ($H_0$ : $p = p_0$) ", bs_icon("question-circle", class = "text-muted")),
      "The hypothesized true probability of success under the null hypothesis."
    ),
    min = 0.01,
    max = 0.99,
    value = 0.50,
    step = 0.01
  ),
  
  sliderInput(
    inputId = "p1",
    label = tooltip(
      trigger = span("True proportion ($H_a$ : $p = p_1$) ", bs_icon("question-circle", class = "text-muted")),
      "The actual probability of success used to generate data for calculating Statistical Power and Type II Error."
    ),
    min = 0.01,
    max = 0.99,
    value = 0.65,
    step = 0.01
  ),
  
  uiOutput("s_obs_ui"),
  
  selectInput(
    inputId = "alpha",
    label = tooltip(
      trigger = span("Significance level \u03b1 ", bs_icon("info-circle", class = "text-muted")),
      "The probability of rejecting the null hypothesis when it is actually true (Type I error rate)."
    ),
    choices = c(0.01, 0.05, 0.10),
    selected = 0.05
  ),
  
  selectInput(
    inputId = "alternative",
    label = tooltip(
      trigger = span("Alternative Choice ", bs_icon("sliders", class = "text-muted")),
      "Select the direction of the statistical test."
    ),
    choices = list(
      "Greater: Hₐ: p > p₀" = "greater",
      "Less: Hₐ: p < p₀" = "less",
      "Two Sided: Hₐ: p ≠ p₀" = "two_sided"
    ),
    selected = "two_sided"
  ),
  
  
  
  selectInput(
    inputId = "palette_name",
    label = "Plot palette",
    choices = names(palette_choices),
    selected = "Colorblind Friendly"
  ),
  
  
  actionButton(
    inputId = "run_sim",
    label = "Run",
    class = "btn-primary"
  )
)
