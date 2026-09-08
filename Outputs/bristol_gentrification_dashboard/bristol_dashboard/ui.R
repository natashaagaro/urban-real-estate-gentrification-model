# =============================================================================
# ui.R
# Bristol Gentrification Index — Shiny Dashboard UI
# =============================================================================

ui <- navbarPage(

  title = "Bristol Gentrification Index",
  id = "main_nav",
  collapsible = TRUE,

  # ---- TAB 1: WARD MAP -----------------------------------------------------
  tabPanel(
    "Ward Map",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Gentrification Index by Ward"),
        p("Wards are shaded by their composite Gentrification Risk Score (0-100),
           combining educational attainment, knowledge-sector employment, and
           average house price (Chapter 3.4 / 4.5)."),
        selectInput("map_ward_select", "Zoom to a ward:", choices = c("None", ward_choices)),
        hr(),
        p(strong("Note:"), "A small number of wards in the boundary file have no
           matched socio-economic data (e.g. jurisdictional or naming mismatches
           discussed in Chapter 3.3) and appear grey on the map.")
      ),
      mainPanel(
        width = 9,
        leafletOutput("ward_map", height = "650px")
      )
    )
  ),

  # ---- TAB 2: HISTORICAL TRENDS ---------------------------------------------
  tabPanel(
    "Historical Trends",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("2016 - 2024 Price Trajectories"),
        selectInput("trend_ward_select", "Select a ward:", choices = ward_choices,
                    selected = if ("Lawrence Hill" %in% ward_choices) "Lawrence Hill" else ward_choices[1]),
        checkboxInput("show_uk_avg", "Show UK National Average", value = TRUE),
        checkboxInput("show_bau", "Show Business-As-Usual projection (+5 years)", value = FALSE),
        hr(),
        p("The 'Business-As-Usual' baseline extends the ward's own 2016-2024
           trend forward with no policy intervention, per Chapter 4.5 / 5.3.")
      ),
      mainPanel(
        width = 9,
        plotlyOutput("ward_trend_plot", height = "420px"),
        hr(),
        h4("Gentrification Leaderboard: % Price Growth 2016-2024"),
        plotlyOutput("leaderboard_plot", height = "600px")
      )
    )
  ),

  # ---- TAB 3: MODEL & INDEX --------------------------------------------------
  tabPanel(
    "Model & Index",
    fluidRow(
      column(
        width = 6,
        h4("Regression Feature Impact (OLS)"),
        p("Estimated £ change in average ward house price per one-unit increase
           in each predictor."),
        plotOutput("coef_plot", height = "380px"),
        verbatimTextOutput("model_summary_text")
      ),
      column(
        width = 6,
        h4("Composite Gentrification Risk Score (0-100)"),
        plotOutput("index_plot", height = "700px")
      )
    )
  ),

  # ---- TAB 4: SCENARIO SIMULATOR ----------------------------------------------
  tabPanel(
    "Scenario Simulator",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h4("Simulate a socio-economic shift"),
        selectInput("sim_ward_select", "Ward:", choices = ward_choices,
                    selected = if ("Easton" %in% ward_choices) "Easton" else ward_choices[1]),
        sliderInput("sim_edu_shift", "% change in highly-educated residents:",
                    min = -50, max = 50, value = 0, step = 5),
        sliderInput("sim_crime_shift", "% change in recorded crime:",
                    min = -50, max = 50, value = 0, step = 5),
        sliderInput("sim_jobs_shift", "% change in knowledge-sector jobs:",
                    min = -50, max = 50, value = 0, step = 5),
        actionButton("run_sim", "Run Simulation", class = "btn-primary"),
        hr(),
        p("Example from Chapter 4.5: try Easton with +10% knowledge jobs and
           -15% crime.")
      ),
      mainPanel(
        width = 8,
        uiOutput("sim_result_box"),
        plotOutput("sim_bar_plot", height = "350px")
      )
    )
  ),

  # ---- TAB 5: DATA TABLE ------------------------------------------------------
  tabPanel(
    "Ward Data Table",
    br(),
    DTOutput("ward_data_table")
  )
)
