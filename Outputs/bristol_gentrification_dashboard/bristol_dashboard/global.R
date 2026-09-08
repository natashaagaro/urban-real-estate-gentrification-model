# =============================================================================
# global.R
# Bristol Gentrification Index — Shiny Dashboard
#
# Loads the two cleaned datasets produced by Bristol_Gentrification_1_EDA.R
# and Bristol_Gentrification_2_Modelling.R, refits the OLS model so the app
# is fully self-contained and defines the two functions the UI calls: 
# simulate_ward_scenario() and project_bau().
# =============================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(sf)
library(leaflet)
library(DT)
library(plotly)
library(htmltools)
library(tibble)

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

# Historical ward-year panel (2016-2024) — from Bristol_Gentrification_1_EDA.R
ts_data <- read.csv("bristol_gentrification_final.csv", stringsAsFactors = FALSE) %>%
  mutate(
    ward_name = str_trim(ward_name),
    ward_code = str_trim(ward_code),
    sale_year = as.integer(sale_year),
    avg_price = as.numeric(avg_price),
    med_price = as.numeric(med_price)
  )

# 2024 cross-sectional snapshot used for the regression model
snapshot_data <- read.csv("bristol_final_dashboard_input.csv", stringsAsFactors = FALSE) %>%
  mutate(
    ward_name = str_trim(ward_name),
    ward_code = str_trim(ward_code)
  )

# Ward boundary polygons
wards_geo <- st_read("bristol_wards.geojson", quiet = TRUE) %>%
  mutate(NAME = str_trim(NAME))

# Official UK National Average House Prices (2016-2024), as used in Chapter 4 EDA
uk_national_benchmark <- tibble(
  sale_year = 2016:2024,
  avg_price = c(212000, 223000, 228000, 231000, 239000, 264000, 286000, 282000, 291000)
)

# -----------------------------------------------------------------------------
# 2. FIT THE OLS MODEL
#    Mirrors Bristol_Gentrification_2_Modelling.R exactly, refit here so the
#    app never depends on a stale pre-computed CSV or a live DB connection.
# -----------------------------------------------------------------------------

model_linear <- lm(
  avg_price ~ pc_highly_educated + total_crimes + pc_knowledge_jobs + total_schools,
  data = snapshot_data
)

snapshot_data$predicted_price <- predict(model_linear)

# -----------------------------------------------------------------------------
# 3. COMPOSITE GENTRIFICATION INDEX (0-100)
#    Weighted: 40% Education / 30% Knowledge Jobs / 30% Price (Chapter 3/4)
# -----------------------------------------------------------------------------

snapshot_data <- snapshot_data %>%
  mutate(
    norm_edu   = (pc_highly_educated - min(pc_highly_educated)) / (max(pc_highly_educated) - min(pc_highly_educated)),
    norm_jobs  = (pc_knowledge_jobs - min(pc_knowledge_jobs)) / (max(pc_knowledge_jobs) - min(pc_knowledge_jobs)),
    norm_price = (avg_price - min(avg_price)) / (max(avg_price) - min(avg_price)),
    gentrification_index = round((0.4 * norm_edu + 0.3 * norm_jobs + 0.3 * norm_price) * 100, 1)
  ) %>%
  select(-norm_edu, -norm_jobs, -norm_price)

# -----------------------------------------------------------------------------
# 4. SCENARIO SIMULATION ENGINE
#    Re-predicts a ward's price under a hypothetical socio-economic shift.
#    Same logic as simulate_ward_scenario() in the modelling script, wired to
#    the Shiny sliders instead of hardcoded test values.
# -----------------------------------------------------------------------------

simulate_ward_scenario <- function(target_ward, pct_edu_shift = 0, pct_crime_shift = 0, pct_jobs_shift = 0) {

  ward_data <- snapshot_data %>% filter(ward_name == target_ward)
  if (nrow(ward_data) == 0) return(NULL)

  new_edu   <- ward_data$pc_highly_educated * (1 + pct_edu_shift / 100)
  new_crime <- ward_data$total_crimes * (1 + pct_crime_shift / 100)
  new_jobs  <- ward_data$pc_knowledge_jobs * (1 + pct_jobs_shift / 100)

  scenario_row <- data.frame(
    pc_highly_educated = new_edu,
    total_crimes        = new_crime,
    pc_knowledge_jobs   = new_jobs,
    total_schools       = ward_data$total_schools
  )

  new_predicted_price <- predict(model_linear, newdata = scenario_row)
  price_diff <- new_predicted_price - ward_data$predicted_price

  list(
    ward               = target_ward,
    original_predicted = round(ward_data$predicted_price, 2),
    scenario_predicted = round(new_predicted_price, 2),
    pound_difference   = round(price_diff, 2),
    pct_change         = round((price_diff / ward_data$predicted_price) * 100, 2)
  )
}

# -----------------------------------------------------------------------------
# 5. BUSINESS-AS-USUAL (BAU) BASELINE PROJECTION
#    Fits a simple year-on-year linear trend per ward from the 2016-2024
#    history and projects it forward. Used by the "Business-As-Usual"
#    toggle described in Chapter 4.5 / Chapter 5.3.
# -----------------------------------------------------------------------------

project_bau <- function(target_ward, years_ahead = 5) {

  ward_hist <- ts_data %>% filter(ward_name == target_ward) %>% arrange(sale_year)
  if (nrow(ward_hist) < 3) return(NULL)  # not enough history to trend reliably

  trend_model <- lm(avg_price ~ sale_year, data = ward_hist)

  future_years  <- (max(ward_hist$sale_year) + 1):(max(ward_hist$sale_year) + years_ahead)
  future_prices <- predict(trend_model, newdata = data.frame(sale_year = future_years))

  bind_rows(
    ward_hist %>% select(sale_year, avg_price) %>% mutate(type = "Historical"),
    tibble(sale_year = future_years, avg_price = as.numeric(future_prices), type = "Business-As-Usual Projection")
  )
}

# Dropdown choices, alphabetical
ward_choices <- sort(unique(snapshot_data$ward_name))
