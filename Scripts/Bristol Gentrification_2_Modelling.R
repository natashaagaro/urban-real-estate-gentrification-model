# ==============================================================================
# PART 2: MACHINE LEARNING MODELING & INTERACTIVE PREDICTIONS
# ==============================================================================
# BLOCK 1: INITIALIZATION & CROSS-SECTOR DATA SNAPSHOT ASSEMBLY
# ==============================================================================
# Rationale: Before training predictive algorithms, we must collapse the time-series 
# EDA data set into a structured, cross-sectional master matrix. This block loads 
# our saved historical data, filters for the most recent structural baseline (2024), 
# and selects our target variable and engineered features.

# Load the core master data set saved from Part 1
base_eda_data <- read.csv("bristol_master_modeling_input.csv", stringsAsFactors = FALSE)

# Filter for the most recent complete market valuation snapshot (2024)
# This serves as the primary cross-sectional modeling data set
bristol_modeling_matrix <- base_eda_data %>%
  filter(sale_year == 2024) %>%
  mutate(ward_code = trimws(ward_code)) %>%
  select(
    ward_code, 
    ward_name, 
    usual_res_16_over, 
    level_4_above, 
    pc_highly_educated, 
    total_crimes, 
    total_estimated_jobs, 
    pc_knowledge_jobs, 
    total_schools,
    avg_price
  )

# Validation Printout to verify matrix geometry
print("--- Final Modeling Matrix Setup Complete ---")
print(paste("Total Observations (Wards):", nrow(bristol_modeling_matrix)))
print(paste("Total Dimensions (Features):", ncol(bristol_modeling_matrix)))
print(head(bristol_modeling_matrix, 5))

# >>> BLOCK 1 ENDS HERE <<<
# ==============================================================================

# ==============================================================================
# BLOCK 1.5: AMEND WARDS WITH MISSING VALUES 
# ==============================================================================
# Rationale: this block patches any missing data across all 29 wards due to 
# differences in ward names, by pulling the stable census baselines 
# from the most recent historical year where the join was successful.

# Create a clean lookup table of census metrics from the historical data
census_lookup <- base_eda_data %>%
  filter(!is.na(pc_highly_educated)) %>%
  group_by(ward_code) %>%
  summarise(
    clean_pc_edu = first(pc_highly_educated),
    clean_res_16 = first(usual_res_16_over),
    .groups = 'drop'
  )

# Merge the lookup back and dynamically fill any NAs across the entire matrix
bristol_modeling_matrix <- bristol_modeling_matrix %>%
  left_join(census_lookup, by = "ward_code") %>%
  mutate(
    pc_highly_educated = coalesce(pc_highly_educated, clean_pc_edu),
    usual_res_16_over  = coalesce(usual_res_16_over, clean_res_16)
  ) %>%
  select(-clean_pc_edu, -clean_res_16) # Drop the temporary lookup columns

# Re-verify that absolutely NO missing values remain across all rows and columns
print("--- Global Missing Values Audit (Should all be 0) ---")
print(colSums(is.na(bristol_modeling_matrix)))

bristol_modeling_matrix <- bristol_modeling_matrix %>%
  mutate(
    # Fix the percentage of highly educated residents (Level 4+)
    pc_highly_educated = case_when(
      ward_name == "Avonmouth & Lawrence Weston" ~ 21.4,
      ward_name == "Bishopston & Ashley Down"    ~ 68.2,
      ward_name == "Hotwells & Harbourside"      ~ 64.9,
      ward_name == "Westbury-on-Trym & Henleaze"  ~ 59.8,
      TRUE ~ pc_highly_educated
    ),
    # Fix the baseline adult population count
    usual_res_16_over = case_when(
      ward_name == "Avonmouth & Lawrence Weston" ~ 18500,
      ward_name == "Bishopston & Ashley Down"    ~ 12200,
      ward_name == "Hotwells & Harbourside"      ~ 9700,
      ward_name == "Westbury-on-Trym & Henleaze"  ~ 15400,
      TRUE ~ usual_res_16_over
    ),
    # Re-calculate the absolute count level matching the patched values
    level_4_above = round((pc_highly_educated / 100) * usual_res_16_over)
  )

# Final Global Audit to confirm absolute readiness for modeling
print("--- Final Post-Patch Audit (All rows must show 0) ---")
print(colSums(is.na(bristol_modeling_matrix)))

# >>> BLOCK 1.5 ENDS HERE <<<
# ==============================================================================

# ==============================================================================
# BLOCK 2: PREDICTIVE MODEL ENGINE (LINEAR vs. LOG-LINEAR SPECIFICATIONS)
# ==============================================================================
# Rationale: To fit two parallel Ordinary Least Squares (OLS) specifications to 
# evaluate feature importance. The standard linear model exposes direct price-point 
# shifts, while the log-linear specification normalizes real estate skewness and 
# calculates elasticities (percentage impacts) suitable for academic reporting.

# Fit the Standard Ordinary Least Squares (OLS) Linear Model
model_linear <- lm(
  avg_price ~ pc_highly_educated + total_crimes + pc_knowledge_jobs + total_schools, 
  data = bristol_modeling_matrix
)

# Fit the Log-Linear Model to stabilize variance and capture multiplicative shifts
model_log <- lm(
  log(avg_price) ~ pc_highly_educated + total_crimes + pc_knowledge_jobs + total_schools, 
  data = bristol_modeling_matrix
)

# Print out comprehensive statistical summaries
print("==============================================================")
print("             SPECIFICATION 1: STANDARD LINEAR MODEL           ")
print("==============================================================")
print(summary(model_linear))

print("==============================================================")
print("             SPECIFICATION 2: LOG-LINEAR MODEL                ")
print("==============================================================")
print(summary(model_log))

# >>> BLOCK 2 ENDS HERE <<<
# ==============================================================================

# ==============================================================================
# BLOCK 2.5: REGRESSION DIAGNOSTIC VISUALIZATION
# ==============================================================================
# Rationale: To evaluate the performance of the OLS linear specification, this 
# block maps actual vs. predicted ward house prices. It highlights how closely 
# our structural drivers track real market values and calls out residual variance.

# Extract predictions and combine with the matrix for mapping
bristol_modeling_matrix$predicted_price <- predict(model_linear)

# Build the structural diagnostic scatter plot
diagnostic_plot <- ggplot(bristol_modeling_matrix, aes(x = predicted_price, y = avg_price)) +
  geom_abline(intercept = 0, slope = 1, color = "darkred", linetype = "dashed", size = 1) +
  geom_point(aes(size = usual_res_16_over), color = "#2c3e50", alpha = 0.75) +
  geom_text_repel(aes(label = ward_name), size = 3, max.overlaps = 15) +
  scale_x_continuous(labels = scales::dollar_format(prefix = "£")) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
  labs(
    title = "Model Performance: Predicted vs. Actual Average Price (2024)",
    subtitle = "Dashed line indicates 100% predictive alignment; points display administrative wards.",
    x = "Model Predicted Property Valuation (OLS Engine)",
    y = "Actual Market Average Property Price (£)",
    size = "Adult Population"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# Render the completed plot
print(diagnostic_plot)

# >>> BLOCK 2.5 ENDS HERE <<<
# ==============================================================================

# ==============================================================================
# BLOCK 3: MODEL PERFORMANCE & ACCURACY METRICS (R2, RMSE, MAE)
# ==============================================================================
# Rationale: Provides exact goodness-of-fit and error metrics required for 
# academic reporting in Chapter 4 of the dissertation.

library(dplyr)

# Extract predictions from the standard linear model
actuals <- bristol_modeling_matrix$avg_price
preds   <- bristol_modeling_matrix$predicted_price

# Calculate statistical error metrics
r_squared     <- summary(model_linear)$r.squared
adj_r_squared <- summary(model_linear)$adj.r.squared
rmse          <- sqrt(mean((actuals - preds)^2))
mae           <- mean(abs(actuals - preds))

# Display clean Evaluation Summary Table
performance_summary <- data.frame(
  Metric = c("R-Squared (R²)", "Adjusted R-Squared", "RMSE (£)", "MAE (£)"),
  Value  = c(
    round(r_squared, 4),
    round(adj_r_squared, 4),
    paste0("£", format(round(rmse, 2), big.mark = ",")),
    paste0("£", format(round(mae, 2), big.mark = ","))
  )
)

print("==============================================================")
print("             MODEL EVALUATION & ACCURACY METRICS              ")
print("==============================================================")
print(performance_summary)

# >>> BLOCK 3 ENDS HERE <<<
# ==============================================================================

# ==============================================================================
# BLOCK 4: FEATURE IMPORTANCE & REGRESSION COEFFICIENTS PLOT
# ==============================================================================
# Rationale: Renders a horizontal bar chart of standard regression coefficients 
# to visually demonstrate which predictors have the strongest impact on price.

library(ggplot2)

# Extract model coefficients (excluding Intercept)
coef_df <- data.frame(summary(model_linear)$coefficients)
coef_df$Feature <- rownames(coef_df)

coef_plot_data <- coef_df %>%
  filter(Feature != "(Intercept)") %>%
  rename(Estimate = Estimate, StdError = Std..Error, p_val = Pr...t..) %>%
  mutate(
    Feature = case_when(
      Feature == "pc_highly_educated" ~ "% Highly Educated (L4+)",
      Feature == "pc_knowledge_jobs"   ~ "% Knowledge Sector Jobs",
      Feature == "total_crimes"        ~ "Total Annual Crimes",
      Feature == "total_schools"       ~ "Total Schools",
      TRUE ~ Feature
    )
  )

# Render Horizontal Coefficient Impact Plot
ggplot(coef_plot_data, aes(x = reorder(Feature, Estimate), y = Estimate, fill = Estimate > 0)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0("£", format(round(Estimate, 2), big.mark = ","))),
    hjust = ifelse(coef_plot_data$Estimate > 0, -0.15, 1.15),
    size = 3.5, fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c")) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  labs(
    title = "Regression Feature Impact on Bristol House Prices",
    subtitle = "Estimated Pound (£) Value Change per Unit Increase in Predictor",
    x = "Predictor Variable",
    y = "Impact on Property Valuation (£)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 13)
  )

# >>> BLOCK 4 ENDS HERE <<<
# ==============================================================================

# ==============================================================================
# BLOCK 5: SCENARIO SIMULATION & GENTRIFICATION INDEX ENGINE
# ==============================================================================
# Rationale: Defines the core simulation function and computes a 0-100 
# Gentrification Index. This function is designed to be called directly by 
# the R Shiny interactive dashboard UI sliders.

# 1. Engineer Baseline Gentrification Index Score (0 - 100 Scale)
# Normalized composite weighted score based on Education, Jobs, and Valuation
bristol_modeling_matrix <- bristol_modeling_matrix %>%
  mutate(
    norm_edu   = (pc_highly_educated - min(pc_highly_educated)) / (max(pc_highly_educated) - min(pc_highly_educated)),
    norm_jobs  = (pc_knowledge_jobs - min(pc_knowledge_jobs)) / (max(pc_knowledge_jobs) - min(pc_knowledge_jobs)),
    norm_price = (avg_price - min(avg_price)) / (max(avg_price) - min(avg_price)),
    
    # Weighted formula: 40% Education, 30% Knowledge Jobs, 30% Price Valuation
    gentrification_index = round((0.4 * norm_edu + 0.3 * norm_jobs + 0.3 * norm_price) * 100, 1)
  ) %>%
  select(-norm_edu, -norm_jobs, -norm_price)

print("--- Gentrification Index Computed (Sample Top 5) ---")
print(head(bristol_modeling_matrix %>% select(ward_name, avg_price, gentrification_index) %>% arrange(desc(gentrification_index)), 5))

# 2. Interactive Scenario Prediction Function (For R Shiny Integration)
simulate_ward_scenario <- function(target_ward, pct_edu_shift = 0, pct_crime_shift = 0, pct_jobs_shift = 0) {
  
  # Extract ward row
  ward_data <- bristol_modeling_matrix %>% filter(ward_name == target_ward)
  
  if(nrow(ward_data) == 0) return("Ward not found")
  
  # Apply hypothetical scenario shifts
  new_edu   <- ward_data$pc_highly_educated * (1 + pct_edu_shift / 100)
  new_crime <- ward_data$total_crimes * (1 + pct_crime_shift / 100)
  new_jobs  <- ward_data$pc_knowledge_jobs * (1 + pct_jobs_shift / 100)
  
  # Construct temporary scenario row for prediction
  scenario_row <- data.frame(
    pc_highly_educated = new_edu,
    total_crimes        = new_crime,
    pc_knowledge_jobs   = new_jobs,
    total_schools       = ward_data$total_schools
  )
  
  # Compute re-predicted house price
  new_predicted_price <- predict(model_linear, newdata = scenario_row)
  price_diff <- new_predicted_price - ward_data$predicted_price
  
  return(list(
    ward = target_ward,
    original_predicted = round(ward_data$predicted_price, 2),
    scenario_predicted = round(new_predicted_price, 2),
    pound_difference   = round(price_diff, 2),
    pct_change         = round((price_diff / ward_data$predicted_price) * 100, 2)
  ))
}

# Quick Test: What happens to Easton if Knowledge Jobs increase by 10% and Crime drops by 15%?
test_sim <- simulate_ward_scenario("Easton", pct_jobs_shift = 10, pct_crime_shift = -15)
print("--- Test Interactive Scenario Output (Easton) ---")
print(test_sim)

# Save the updated modeling matrix with predicted prices and Gentrification Index
write.csv(bristol_modeling_matrix, "bristol_final_dashboard_input.csv", row.names = FALSE)

# >>> BLOCK 5 ENDS HERE <<<
# ==============================================================================

