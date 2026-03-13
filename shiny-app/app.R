library(shiny)
library(bsicons)
library(bslib)
library(shinycssloaders)
library(plotly)
library(ggplot2)

ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    "navbar-bg" = "#2C3E50"
  ),
  
  title = div(
    h2(
      "Monte Carlo Simulation of a Hypothesis Test for Binomial Proportions",
      style = "color: #FFFFFF; margin: 0; font-weight: 700;"
    )
  ),
  
  tags$head(
    tags$style(HTML("
    body {
      background: #F4F7FB;
      color: #1F2937;
    }

    .bslib-sidebar-layout > .sidebar {
      background: #FFFFFF;
      border-right: 1px solid #E5E7EB;
      padding: 1.25rem;
    }

    .card,
    .value-box,
    .navset-card,
    .sidebar {
      border: none !important;
      border-radius: 18px !important;
      box-shadow: 0 8px 24px rgba(15, 23, 42, 0.06);
    }

    .card-header {
      background: transparent !important;
      border-bottom: 1px solid #EEF2F7 !important;
      font-weight: 600;
      color: #0F172A;
    }

    .nav-tabs {
      border-bottom: none !important;
      gap: 0.35rem;
      margin-bottom: 0.75rem;
    }

    .nav-tabs .nav-link {
      color: #475569 !important;
      background: #EEF3FB;
      border: none !important;
      border-radius: 12px !important;
      padding: 0.65rem 1rem;
      margin-right: 0.15rem;
    }

    .nav-tabs .nav-link:hover {
      background: #E2E8F0;
      color: #1F2937 !important;
    }

    .nav-tabs .nav-link.active {
      color: #0F172A !important;
      background: #FFFFFF !important;
      box-shadow: 0 4px 14px rgba(15, 23, 42, 0.08);
      font-weight: 600;
    }

    .btn-primary {
      background-color: #2952CC !important;
      border-color: #2952CC !important;
      border-radius: 12px !important;
      font-weight: 600;
      padding: 0.65rem 1.15rem;
    }

    .btn-primary:hover {
      background-color: #1E40AF !important;
      border-color: #1E40AF !important;
    }

    .form-label,
    .control-label {
      font-weight: 600;
      color: #0F172A;
      margin-bottom: 0.45rem;
    }

    .shiny-input-container {
      margin-bottom: 1rem;
    }

    .irs--shiny .irs-bar,
    .irs--shiny .irs-single {
      background: #2952CC !important;
      border-color: #2952CC !important;
    }

    .irs--shiny .irs-handle > i:first-child {
      background: #2952CC !important;
    }

    .app-alert {
      background: #FFFFFF;
      border: 1px solid #E5E7EB;
      border-left: 5px solid #2952CC;
      border-radius: 16px;
      padding: 1rem 1.2rem;
      margin-bottom: 0.5rem;
    }

    .app-alert-warning {
      border-left-color: #D97706;
      background: #FFF7ED;
    }

    .app-alert-success {
      border-left-color: #059669;
      background: #ECFDF5;
    }

    .app-title {
      margin-bottom: 0.2rem;
      font-weight: 700;
      color: #0F172A;
    }

    .app-subtitle {
      color: #64748B;
      margin-bottom: 0;
    }
  "))
  ),
  
  latex_tags,
  
  sidebar = sidebar(
    sim_inputs,
    width = 300,
    open = "always"
  ),
  
  layout_columns(
    col_widths = c(12),
    
    fill = FALSE, # keeps boxes same size as contents (prevent unnecessary gaps)
    
    div(
      class = "app-alert",
      h4("Normal approximation check", style = "font-size: 1rem; margin-bottom: 0.35rem;"),
      uiOutput("assumption_alert", style = "font-size: 0.9rem;")
    ),
    
    card(
        full_screen = FALSE,
        card_header("Simulation Table"),
        card_body(withSpinner(uiOutput("summary_boxes")))
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
  
  output$assumption_alert <- renderUI({
    result <- assumption_check(input$sample_size, input$p0)
    
    alert_class <- if (result$ok) "app-alert app-alert-success" else "app-alert app-alert-warning"
    text_color  <- if (result$ok) "#065F46" else "#92400E"
    
    tags$div(
      class = alert_class,
      style = paste0("color: ", text_color, "; margin-bottom: 0;"),
      tags$strong(result$header),
      tags$span(result$details)
    )
  })
  
}

shinyApp(ui = ui, server = server)
