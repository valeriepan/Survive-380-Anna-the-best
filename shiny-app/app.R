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
  title = "Monte Carlo Simulation of a Hypothesis Test for Binomial Proportions",
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
    width = 300,
    open = "always"
  ),
  
  layout_columns(
    col_widths = c(12),
    
    fill = FALSE, # keeps boxes same size as contents (prevent unnecessary gaps)
    
    div( 
      # note: changed from card to this method including server function for 
      # increased customization, e.g. changing colours, font size, etc.
      
      # build a text box
      style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; 
        border: 1px solid #dee2e6; margin-bottom: 5px;",
      
      h4("Normal approximation check", style = "color: #2C3E50; font-size: 1rem;"),
      uiOutput("assumption_alert", style = "font-size: 0.85rem;")
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
    
    # Set color based on status
    text_color <- if (result$ok) "#2ecc71" else "#e74c3c" 
    
    tags$div(
      style = paste0("color: ", text_color),
      tags$strong(result$header), # make the text bold 
      tags$span(result$details)    # keep it on the same line
    )
  })
}

shinyApp(ui = ui, server = server)
