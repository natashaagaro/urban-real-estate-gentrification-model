# Bristol Gentrification Index — R Shiny Dashboard

This is the interactive artefact described in Chapter 3.5, Chapter 4.5, and
Chapter 5.3 of your dissertation. It's built directly on the model, index
formula, and scenario-simulation function from your two R scripts — nothing
in the underlying logic was changed, it's just wired to a Shiny UI.

## Files

```
bristol_dashboard/
├── app.R                              # entry point — source this to run
├── global.R                           # loads data, refits OLS model, defines
│                                       # simulate_ward_scenario() and project_bau()
├── ui.R                                # 5-tab layout
├── server.R                            # render logic for each tab
├── bristol_gentrification_final.csv    # 2016-2024 ward-year panel (from your EDA script)
├── bristol_final_dashboard_input.csv   # 2024 cross-sectional snapshot
├── bristol_wards.geojson               # ward boundaries
└── README.md
```

## How to run

1. Put the whole `bristol_dashboard/` folder somewhere on your machine and
   open it as your working directory in RStudio (or open `app.R` — RStudio
   will offer a "Run App" button automatically).
2. Install any missing packages:

   ```r
   install.packages(c(
     "shiny", "dplyr", "tidyr", "stringr", "ggplot2", "scales",
     "sf", "leaflet", "DT", "plotly", "htmltools", "tibble"
   ))
   ```

3. Run:

   ```r
   shiny::runApp("bristol_dashboard")
   ```

   or click **Run App** in RStudio with `app.R` open.

## What each tab does

- **Ward Map** — Leaflet choropleth of the 0–100 Gentrification Index across
  ward boundaries, with hover tooltips.
- **Historical Trends** — per-ward 2016–2024 price line, optional UK national
  average overlay, optional 5-year Business-As-Usual projection, and the
  % growth leaderboard (Lawrence Hill, Hotwells & Harbourside, etc.).
- **Model & Index** — the OLS regression coefficient plot (£ impact per
  predictor) and the ranked Gentrification Index bar chart.
- **Scenario Simulator** — sliders for % change in education, crime, and
  knowledge jobs, calling `simulate_ward_scenario()` exactly as written in
  `Bristol_Gentrification_2_Modelling.R`.
- **Ward Data Table** — sortable/searchable table of all underlying metrics.

## Known data limitations (worth stating explicitly in Chapter 5.4)

- **5 wards with no socio-economic match**: `bristol_wards.geojson` contains
  34 wards, but only 29 have matched data in the snapshot file (Frome Vale,
  Hartcliffe & Withywood, Henbury & Brentry, Hengrove & Whitchurch Park,
  Hillfields). These render grey on the map with "No data" tooltips rather
  than being silently dropped.
- **4 wards with no full 2016–2024 history**: Avonmouth & Lawrence Weston,
  Bishopston & Ashley Down, Hotwells & Harbourside, and Westbury-on-Trym &
  Henleaze appear in the 2024 snapshot (so they're on the map, index, and
  simulator) but not in the time-series file, so they won't have a line on
  the Historical Trends tab or a Business-As-Usual projection.

## Before you submit / share this anywhere public

Your uploaded `Bristol_Gentrification_1_EDA.R` and
`02_further_database_development.ipynb` contain a **hardcoded plaintext
MySQL password** (`Bristol_2026`). It isn't used anywhere in this dashboard
(the app reads from CSV/GeoJSON only, no DB connection), but if those two
files go into your dissertation appendix or a GitHub repo, scrub the
credential first — e.g. read it from an environment variable or a
`.Renviron` file that's excluded via `.gitignore`.
