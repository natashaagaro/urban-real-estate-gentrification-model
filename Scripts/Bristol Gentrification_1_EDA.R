# ==========================================
# BRISTOL GENTRIFICATION PROJECT: R LAYER
# ==========================================

# === BLOCK 1: CONNECT TO DATABASE ====
# Load required required libraries
library(DBI)
library(RMariaDB)
library(tidyverse)

# Establish connection to MySQL database
con <- dbConnect(
  RMariaDB::MariaDB(),
  username = "root",
  password = "Bristol_2026",
  host     = "127.0.0.1",
  dbname   = "bristol_gentrification"
)

# Check: List the tables to verify the connection is live
print("--- Active Database Tables ---")
print(dbListTables(con))

# Test Join: Look at housing prices alongside their real Ward Names
test_query <- "
  SELECT h.transaction_id, h.price, h.date_of_transfer, h.postcode, b.ward_name
  FROM bristol_housing_prices h
  INNER JOIN postcode_bridge b ON h.postcode = b.postcode
  LIMIT 10;
"

test_data <- dbGetQuery(con, test_query)

print("--- Test Join Results ---")
print(test_data)

# >>> BLOCK 1 ENDS HERE <<<
# ==========================================================================

# ========================================================
# ANALYSIS 1: ANNUAL WARD HOUSING PRICE TRAJECTORIES
# ========================================================

# === BLOCK 2: ANNUAL WARD HOUSING PRICE TRAJECTORIES =====================
print("Calculate annual ward-level housing statistics")

# Pull the necessary columns from the database
ward_housing_trends <- tbl(con, "bristol_housing_prices") %>%
  inner_join(tbl(con, "postcode_bridge"), by = "postcode") %>%
  mutate(sale_year = year(date_of_transfer)) %>%
  select(ward_code, ward_name, sale_year, price) %>%
  collect() %>%  # Bring those columns into R's memory
  
  # Calculate the counts, averages, and medians
  group_by(ward_code, ward_name, sale_year) %>%
  summarise(
    total_sales = n(),
    avg_price   = mean(price, na.rm = TRUE),
    med_price   = median(price, na.rm = TRUE),
    .groups     = "drop"
  )

print("--- Ward Housing Trends Sample ---")
print(head(ward_housing_trends))

# >>> BLOCK 2 ENDS HERE <<<
# ==========================================================================

# ========================================================
# ANALYSIS 2: ANNUAL WARD CRIME PROFILES
# ========================================================

# === BLOCK 3: ANNUAL WARD CRIME PROFILES ==================================
print("Calculate annual ward-level crime statistics")

# Pull the crime records and aggregate them by ward and year
ward_crime_trends <- tbl(con, "ward_crime") %>%
  # Columns needed to track type, year, and counts
  select(ward_code, ward_name, crime_type, period, ward_statistic) %>%
  collect() %>%  # Bring it into R's memory
  
  # Group by ward and time period to get total crime counts
  group_by(ward_code, ward_name, period) %>%
  summarise(
    total_crimes     = sum(ward_statistic, na.rm = TRUE),
    avg_crimes_typed = mean(ward_statistic, na.rm = TRUE),
    .groups          = "drop"
  )

print("--- Ward Crime Trends Sample ---")
print(head(ward_crime_trends))

# >>> BLOCK 3 ENDS HERE <<<
# ==========================================================================

# ========================================================
# ANALYSIS 3: WARD EDUCATION AND QUALIFICATION
# ========================================================

# === BLOCK 4: WARD EDUCATION AND QUALIFICATION ==================
print("Processing ward-level census qualification data...")

# Get census qualifications from MySQL
ward_education_snapshot <- tbl(con, "ward_qualifications") %>%
  # Select the key columns needed for the educational metric
  select(ward_code, ward_name, usual_res_16_over, level_4_above) %>%
  collect() %>%  # Bring into R's memory
  
  # Calculate the proportion of highly educated residents
  mutate(
    pc_highly_educated = (level_4_above / usual_res_16_over) * 100
  )

print("--- Ward Education ---")
print(head(ward_education_snapshot))

# >>> BLOCK 4 ENDS HERE <<<
# ==========================================================================

# ========================================================
# ANALYSIS 4: WARD EMPLOYMENT & SCHOOL METRICS
# ========================================================

# === BLOCK 5: PULL NEW STAGING TABLES ==================
print("Processing new staging tables for jobs and schools...")

# Pull the job infrastructure metrics
ward_jobs_snapshot <- tbl(con, "staging_ward_jobs") %>%
  select(ward_code, total_estimated_jobs, pc_knowledge_jobs) %>%
  collect()

# Pull the school infrastructure metrics
ward_schools_snapshot <- tbl(con, "staging_ward_schools") %>%
  select(ward_code, total_schools) %>%
  collect()

print("--- Ward Jobs Sample ---")
print(head(ward_jobs_snapshot))
print("--- Ward Schools Sample ---")
print(head(ward_schools_snapshot))

# >>> BLOCK 5 ENDS HERE <<<
# ==========================================================================

# ========================================================
# MASTER INTEGRATION LAYER
# ========================================================

# === BLOCK 6: STITCH METRICS INTO MASTER DATAFRAME ======
print("Standardizing names and building final master dataframe...")

# Clean text quirks in the housing data
ward_housing_trends_fixed <- ward_housing_trends %>%
  mutate(
    ward_name = stringr::str_trim(ward_name),
    ward_name = stringr::str_replace_all(ward_name, " and ", " & ")
  )

# Clean text quirks and extract the year from the crime data
ward_crime_trends_fixed <- ward_crime_trends %>% 
  mutate(
    ward_name = stringr::str_trim(ward_name),
    ward_name = stringr::str_replace_all(ward_name, " and ", " & "),
    sale_year = as.integer(stringr::str_extract(period, "\\d{4}"))
  )

# Combine everything together using the cleaned data sets into a master data set
bristol_master_dataset <- ward_housing_trends_fixed %>%
  # Join yearly crime metrics with standardized names and years
  left_join(ward_crime_trends_fixed, by = c("ward_code", "ward_name", "sale_year")) %>%
  
  # Join static snapshots
  left_join(ward_education_snapshot, by = c("ward_code", "ward_name")) %>%
  left_join(ward_jobs_snapshot, by = "ward_code") %>%
  left_join(ward_schools_snapshot, by = "ward_code") %>%
  
  # Remove the extra tracking columns
  select(-period)

# Filter the data set to only keep rows within official boundary of Bristol
bristol_master_dataset <- bristol_master_dataset %>%
  filter(!is.na(total_crimes))

# Overwrite the EDA data set with the newly fixed numeric versions
bristol_eda_data <- bristol_master_dataset %>%
  mutate(
    sale_year = as.integer(sale_year),
    avg_price = as.numeric(avg_price),
    med_price = as.numeric(med_price)
  )

print(paste("New total unique wards:", length(unique(bristol_eda_data$ward_name))))

# >>> BLOCK 6 ENDS HERE <<<
# ==========================================================================

# Convert integer64 columns to normal numeric values for ggplot2
bristol_eda_data <- bristol_master_dataset %>%
  mutate(
    sale_year = as.integer(sale_year),
    avg_price = as.numeric(avg_price),
    med_price = as.numeric(med_price)
  )

# ========================================================
# Exploratory Data Analysis (EDA)
# ========================================================

# ==============================================================================
# BLOCK 7: INDIVIDUAL WARD BREAKDOWN (WITH DEDICATED UK NATIONAL BENCHMARK)
# ==============================================================================
# Rationale: Appending official UK National Average house prices as a dedicated 
# 30th category fills the bottom-right grid slot, allowing direct comparison 
# between local Bristol ward trajectories and the national baseline.

library(ggplot2)
library(dplyr)

# 1. Official UK National Average House Prices (2016 - 2024)
uk_national_benchmark <- tibble::tibble(
  sale_year = 2016:2024,
  avg_price = c(212000, 223000, 228000, 231000, 239000, 264000, 286000, 282000, 291000),
  ward_name = " UK National Avg" # Space prefix places it cleanly in the final slot
)

# 2. Combine individual ward data with the UK National Benchmark
faceted_plot_data <- bristol_eda_data %>%
  select(ward_name, sale_year, avg_price) %>%
  bind_rows(uk_national_benchmark)

# Render Faceted Grid (Uniform Y-Axis + 30th National Benchmark Panel)
ggplot(faceted_plot_data, aes(x = sale_year, y = avg_price, color = ward_name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.2) +
  
  # Highlight the National Benchmark panel with a distinct color (e.g., Red/Coral)
  scale_color_manual(values = c(
    rep("#3498db", length(unique(bristol_eda_data$ward_name))), 
    "#e74c3c" # Accent color for the UK National Avg tile
  )) +
  
  # Keep Y-axis scale fixed and uniform across all 30 panels
  scale_y_continuous(labels = scales::label_dollar(prefix = "£")) +
  facet_wrap(~ward_name, scales = "fixed", ncol = 5) + 
  
  labs(
    title = "Bristol Housing Trajectories vs. UK National Average",
    subtitle = "Faceted Ward Trends with Dedicated UK National Benchmark Panel (Uniform Y-Axis Scale)",
    x = "Year",
    y = "Average Sale Price (£)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 8),
    panel.spacing = unit(0.8, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )

# >>> BLOCK 7 ENDS HERE <<<
# ==============================================================================

# ==============================================================================
# BLOCK 8: PERCENTAGE GROWTH LEADERBOARD WITH IN-BAR ANNOTATIONS
# ==============================================================================
# Rationale: While absolute price changes highlight capital volume, percentage 
# growth captures localized gentrification shocks relative to a ward's baseline. 

library(dplyr)
library(ggplot2)

# Filter and calculate percentage growth
gentrification_leaderboard <- bristol_eda_data %>%
  filter(sale_year %in% c(2016, 2024)) %>%
  group_by(ward_name) %>%
  summarise(
    price_2016 = mean(avg_price[sale_year == 2016]),
    price_2024 = mean(avg_price[sale_year == 2024]),
    pct_growth = ((price_2024 - price_2016) / price_2016) * 100
  ) %>%
  filter(!is.na(pct_growth)) %>%
  arrange(desc(pct_growth))

print("--- Top Gentrifying Wards by % Price Growth (2016-2024) ---")
print(head(gentrification_leaderboard, 10))

# Plot the leader board 
ggplot(gentrification_leaderboard, aes(x = reorder(ward_name, pct_growth), y = pct_growth, fill = pct_growth)) +
  geom_col() +
  # Add labels inside the bars
  geom_text(
    aes(label = sprintf("%.1f%%", pct_growth)), 
    hjust = 1.2,
    color = "white", 
    fontface = "bold",
    size = 3.5
  ) +
  coord_flip() + 
  scale_fill_gradient(low = "#7bc8f6", high = "#0343df") +
  labs(
    title = "Bristol Ward Gentrification Leaderboard",
    subtitle = "Total Percentage House Price Growth (2016 - 2024)",
    x = "Ward Name",
    y = "Percentage Growth (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold", size = 9)
  )

# >>> BLOCK 8 ENDS HERE <<<
# ==========================================================================

# ==============================================================================
# BLOCK 9: MULTI-VARIABLE CO-MOVEMENT & EMPIRICAL CORRELATION HEATMAP
# ==============================================================================
# Rationale: To justify multi-variable regression frameworks, we must measure the 
# linear dependency across public safety records, housing costs, and market activity. 
# This block isolates parameters to prevent collinearity issues during modeling phases.

library(corrplot)

# Isolate the key numerical tracking metrics for the 29 wards
corr_data <- bristol_eda_data %>%
  select(avg_price, med_price, total_sales, total_crimes) %>%
  # Rename columns cleanly for formal paper presentation
  rename(
    `Avg Price` = avg_price,
    `Median Price` = med_price,
    `Sales Volume` = total_sales,
    `Total Crimes` = total_crimes
  ) %>%
  na.omit()

# Compute the mathematical correlation matrix
matrix_coefficients <- cor(corr_data)

# Render the heat map
corrplot(matrix_coefficients, 
         method = "color", 
         type = "upper",          # Only show the top half to avoid duplicate triangles
         addCoef.col = "black",   # Print the exact correlation number inside the square
         tl.col = "black",        # Color of the text labels
         tl.srt = 45,             # Tilt text labels to make them readable
         diag = FALSE,            # Hide variables matching with themselves
         mar = c(0, 0, 2, 0),
         title = "Empirical Correlation Matrix: Bristol Urban Indicators")

# >>> BLOCK 9 ENDS HERE <<<
# ==========================================================================

# ==============================================================================
# BLOCK 10: POPULATION-ADJUSTED MARKET VELOCITY MATRIX (NO CSV REQUIRED)
# ==============================================================================
# Rationale: Normalizes market liquidity by calculating an annual 
# "Sales per 1,000 Residents" metric using the population data already loaded 
# in R memory, mapping true turnover intensity against average valuations.

library(dplyr)
library(ggplot2)
library(ggrepel)

# Calculate population-adjusted metrics directly from bristol_eda_data
adjusted_velocity <- bristol_eda_data %>%
  mutate(ward_name = trimws(ward_name)) %>%
  group_by(ward_name) %>%
  summarise(
    mean_price   = mean(avg_price, na.rm = TRUE),
    mean_pop     = mean(usual_res_16_over, na.rm = TRUE),
    # Calculate average annual sales per 1,000 adult residents
    sales_per_1k = (mean(total_sales, na.rm = TRUE) / mean_pop) * 1000,
    .groups      = "drop"
  ) %>%
  filter(!is.na(sales_per_1k) & sales_per_1k > 0)

# Validation Check: Ensure all wards are accounted for
print(paste("Total unique wards on plot:", nrow(adjusted_velocity)))

# Render the Population-Normalized Velocity Matrix
ggplot(adjusted_velocity, aes(x = sales_per_1k, y = mean_price, label = ward_name)) +
  geom_point(aes(size = sales_per_1k, color = mean_price), alpha = 0.7) +
  geom_text_repel(size = 3.2, fontface = "bold", max.overlaps = 20) +
  
  # Dynamic crosshairs based on population-adjusted averages
  geom_hline(yintercept = mean(adjusted_velocity$mean_price), linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = mean(adjusted_velocity$sales_per_1k), linetype = "dashed", color = "gray50") +
  
  scale_y_continuous(labels = scales::label_dollar(prefix = "£")) +
  scale_color_gradient(low = "#10ac84", high = "#ff9f43") +
  labs(
    title = "Bristol Housing Market Velocity Matrix (Population-Normalized)",
    subtitle = "Analyzing Per-Capita Trading Intensity vs. Price Valuations (2016-2024)",
    x = "Average Annual Property Sales per 1,000 Adult Residents",
    y = "Average Housing Sale Price"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12)
  )

# >>> BLOCK 10 ENDS HERE <<<
# ==============================================================================

# ==================================================================
# BLOCK 11: HUMAN CAPITAL & EDUCATION CONCENTRATION VS. VALUATION
# ==================================================================
# Rationale: Socioeconomic displacement is heavily underpinned by shifts in 
# localised human capital. This block uses the pre-joined Census education 
# metrics to map degree prevalence against 2024 house prices.

library(dplyr)
library(ggplot2)
library(ggrepel)

# Join baseline price data (2024) with pre-calculated education metrics
price_vs_education <- bristol_eda_data %>%
  filter(sale_year == 2024) %>%
  mutate(ward_name = trimws(ward_name)) %>%
  filter(!is.na(pc_highly_educated))

# Render the Education vs. Valuation Scatter Analysis
ggplot(price_vs_education, aes(x = pc_highly_educated, y = avg_price, label = ward_name)) +
  geom_point(aes(size = pc_highly_educated), color = "#4834d4", alpha = 0.6) +
  geom_smooth(method = "lm", color = "#ffbe76", linetype = "dashed", se = FALSE) +
  geom_text_repel(size = 3.2, fontface = "bold", max.overlaps = 15) +
  scale_y_continuous(labels = scales::label_dollar(prefix = "£")) +
  labs(
    title = "Bristol Property Value vs. Local Human Capital Index",
    subtitle = "Cross-Analysis of Degree Prevalence (Census 2021) and 2024 House Prices",
    x = "Percentage of Adult Population with Higher Ed Degrees (Level 4+ %)",
    y = "2024 Average Sale Price"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12)
  )

# >>> BLOCK 11 ENDS HERE <<<
# ==============================================================================

# ============================================================
# BLOCK 12: WORKFORCE EVOLUTION & KNOWLEDGE ECONOMY INDEX
# ============================================================
# Rationale: Pulls the staging_ward_jobs table directly from MySQL, dynamically 
# finds the knowledge jobs column, and renders a ranked horizontal percentage bar chart.

library(dplyr)
library(ggplot2)

# Pull the staging table from MySQL
jobs_data_raw <- tbl(con, "staging_ward_jobs") %>% collect()

# Clean column names to lowercase to ensure smooth matching
colnames(jobs_data_raw) <- tolower(colnames(jobs_data_raw))

# Find columns for 'ward_code' and 'knowledge'
code_col <- colnames(jobs_data_raw)[grep("code", colnames(jobs_data_raw))][1]
knowledge_col <- colnames(jobs_data_raw)[grep("knowledge", colnames(jobs_data_raw))][1]

print(paste("Found ward code column:", code_col))
print(paste("Found knowledge column:", knowledge_col))

# Standardize column names for joining
jobs_data <- jobs_data_raw %>%
  select(ward_code = all_of(code_col), pc_knowledge = all_of(knowledge_col)) %>%
  mutate(pc_knowledge = as.numeric(pc_knowledge))

# Combine with 2024 pricing and ward names
job_market_analysis <- bristol_eda_data %>%
  filter(sale_year == 2024) %>%
  mutate(ward_code = trimws(ward_code)) %>%
  left_join(jobs_data, by = "ward_code") %>%
  filter(!is.na(pc_knowledge)) %>%
  distinct(ward_name, .keep_all = TRUE) %>%
  arrange(desc(pc_knowledge))

# Render Horizontal Percentage Bar Chart
ggplot(job_market_analysis, aes(x = reorder(ward_name, pc_knowledge), y = pc_knowledge, fill = pc_knowledge)) +
  geom_col(width = 0.75) +
  geom_text(
    aes(label = sprintf("%.1f%%", pc_knowledge)), 
    hjust = -0.15,
    color = "black", 
    fontface = "bold",
    size = 3
  ) +
  coord_flip() + 
  scale_fill_gradient(low = "#81ecec", high = "#00cec9") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + # Prevents label clipping
  labs(
    title = "Bristol Knowledge Sector Employment Share by Ward",
    subtitle = "Percentage of Total Local Jobs in High-Value Knowledge Sectors",
    x = "Ward Name",
    y = "Knowledge Sector Employment Share (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold", size = 8.5),
    plot.title = element_text(face = "bold", size = 12)
  )

# >>> BLOCK 12 ENDS HERE <<<
# ==============================================================================

# ==============================================================================
# BLOCK 13: INSTITUTIONAL ANCHORS - SCHOOL INFRASTRUCTURE DENSITY (NO CSV REQUIRED)
# ==============================================================================
# Rationale: Educational infrastructure acts as a primary anchor for middle-class 
# families, heavily influencing localized real estate demand. This block uses 
# pre-loaded school metrics to categorize wards into density tiers for box plot analysis.

library(dplyr)
library(ggplot2)
library(ggrepel)

# 1. Categorize wards into structural density tiers using data already in memory
school_market_distribution <- bristol_eda_data %>%
  filter(sale_year == 2024) %>%
  mutate(
    ward_code = trimws(ward_code),
    school_density_tier = case_when(
      total_schools <= 3 ~ "Low Density (0-3 Schools)",
      total_schools >= 4 & total_schools <= 7 ~ "Moderate Density (4-7 Schools)",
      total_schools >= 8 ~ "High Density (8+ Schools)"
    ),
    school_density_tier = factor(school_density_tier, levels = c(
      "Low Density (0-3 Schools)", 
      "Moderate Density (4-7 Schools)", 
      "High Density (8+ Schools)"
    ))
  ) %>%
  filter(!is.na(school_density_tier))

# 2. Render Comparative Distribution Box Plot
ggplot(school_market_distribution, aes(x = school_density_tier, y = avg_price, fill = school_density_tier)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) + # Hide default outliers to avoid duplicate dots
  
  # Add points and label them cleanly with ward names
  geom_point(position = position_jitter(width = 0.15, seed = 42), color = "black", alpha = 0.5, size = 1.5) +
  geom_text_repel(
    aes(label = ward_name),
    position = position_jitter(width = 0.15, seed = 42), 
    fontface = "bold",
    box.padding = 0.2,
    point.padding = 0.2,
    max.overlaps = 30
  ) +
  
  scale_y_continuous(labels = scales::label_dollar(prefix = "£"), limits = c(0, 1200000)) +
  scale_fill_manual(values = c("#ff7675", "#74b9ff", "#a29bfe")) +
  labs(
    title = "Bristol Property Value Distribution by School Infrastructure Density",
    subtitle = "Comparative Box Plot of 2024 Average House Prices with Labeled Wards",
    x = "School Infrastructure Density Tier",
    y = "2024 Average Sale Price"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 10)
  )

# >>> BLOCK 13 ENDS HERE <<<
# ==============================================================================


write.csv(bristol_eda_data, "bristol_master_modeling_input.csv", row.names = FALSE)
