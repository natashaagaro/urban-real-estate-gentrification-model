# =============================================================================
# app.R
# Entry point for the Bristol Gentrification Index Shiny dashboard.
# Run with: shiny::runApp("bristol_dashboard")  (or click "Run App" in RStudio)
# =============================================================================

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

rsconnect::appDependencies("bristol_dashboard")