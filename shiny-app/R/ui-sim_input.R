sim_inputs <- div(
  h4("Simulation controls"),
  
  numericInput(
    inputId = "seed",
    label = "Random seed",
    value = 380,
    min = 1,
    step = 1
  ),
  
  sliderInput(
    inputId = "N",
    label = "Number of Monte Carlo simulations",
    min = 1000,
    max = 10000,
    value = 5000,
    step = 500
  ),
  
  sliderInput(
    inputId = "sample_size",
    label = "Number of trials $n$",
    min = 10,
    max = 500,
    value = 60,
    step = 5
  ),
  
  sliderInput(
    inputId = "p0",
    label = "Null proportion $p_0$",
    min = 0.01,
    max = 0.99,
    value = 0.50,
    step = 0.01
  ),
  
  sliderInput(
    inputId = "p1",
    label = "True proportion $p_1$ for power / Type II error",
    min = 0.01,
    max = 0.99,
    value = 0.65,
    step = 0.01
  ),
  
  uiOutput("s_obs_ui"),
  
  selectInput(
    inputId = "alpha",
    label = "Significance level $\u03b1$",
    choices = c(0.01, 0.05, 0.10),
    selected = 0.05
  ),
  
  selectInput(
    inputId = "alternative",
    label = "Alternative Choice",
    choices = list(
      "Greater: Hₐ: p > p₀" = "greater",
      "Less: Hₐ: p < p₀" = "less",
      "Two Sided: Hₐ: p ≠ p₀" = "two_sided"
    ),
    selected = "two_sided"
  ),
  
  selectInput(
    inputId = "palette_name",
    label = "Colour theme",
    choices = names(palette_choices),
    selected = "Colorblind Friendly"
  ),
  
  actionButton(
    inputId = "run_sim",
    label = "Run",
    class = "btn-primary"
  )
)
