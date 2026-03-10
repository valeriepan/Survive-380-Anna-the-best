library(shiny)
library(bsicons)
library(bslib)
library(shinycssloaders)
library(plotly)


ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    "navbar-bg" = "#2C3E50"
  ),
  title = "Monte Carlo Simulation for Binomial Proportion Tests",
  tags$head(
    tags$style(HTML(".nav-tabs .nav-link {
        color: #2C3E50 !important;
    }
      .nav-tabs .nav-link:hover {
        color: #1a252f !important; 
      }
    "))
  ),
  
  latex_tags,
  
  sidebar = sidebar(
    sim_inputs,
    width = 420,
    open = "always"
  ),
  
  layout_columns(
    col_widths = c(12),
    
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = FALSE,
        card_header("Simulation Table"),
        card_body(withSpinner(uiOutput("summary_boxes")))
      ),
      card(
        full_screen = FALSE,
        card_header("Normal approximation check"),
        card_body(uiOutput("assumption_alert"))
      )
    ),
    
    navset_card_tab(
      id = "main_tabs",
      full_screen = TRUE,
      
      nav_panel(
        "Observed p-value",
        br(),
        withSpinner(tableOutput("pvalue_table"))
      ),
      
      nav_panel(
        "Null p-value histogram",
        br(),
        withSpinner(plotlyOutput("null_hist", height = "460px"))
      ),
      
      nav_panel(
        "Power curve",
        br(),
        withSpinner(plotlyOutput("power_curve_plot", height = "460px")),
        br(),
        withSpinner(tableOutput("power_curve_head"))
      ),
      
      nav_panel(
        "Sample size and Power",
        br(),
        withSpinner(plotlyOutput("sample_size_plot", height = "460px"))
      ),
      
      nav_panel(
        "Help",
        br(),
        help_panel
      )
    )
  )
)


server <- function(input, output, session) {
  source(file.path("server-plots.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)
