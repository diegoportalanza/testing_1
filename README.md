# cmip6extremes

R package for downloading and processing **NASA NEX-GDDP-CMIP6** data — high-resolution (0.25°), bias-corrected, daily downscaled climate projections from CMIP6 models (1950–2100).  
Useful for calculating ETCCDI-style extreme climate indices (e.g., TXx, TNn, RX1day, R95p, R10mm) from temperature and precipitation, with support for point-based and raster workflows (clipping to shapefiles, mapping).

**Dataset details**  
- Source: NASA THREDDS server — https://ds.nccs.nasa.gov/thredds/catalog/AMES/NEX/GDDP-CMIP6/catalog.html  
- Resolution: 0.25° × 0.25° (~25 km)  
- Time: Historical (1950–2014) + future SSPs (2015–2100)  
- Scenarios: historical, ssp126, ssp245, ssp370, ssp585  
- Variables: tas, tasmax, tasmin, pr, huss, hurs, rlds, rsds, sfcWind (availability varies by model)  
- Models: 35 total (e.g., ACCESS-CM2, GFDL-ESM4, MRI-ESM2-0, UKESM1-0-LL — full list in function docs or catalog)  

## Installation

Install from your GitHub repo (recommended for latest version):

```r
# install.packages("remotes")
remotes::install_github("diegoportalanza/testing_1", ref = "codex/create-r-package-for-extreme-indices-ck7mvf")
For local development (from package root folder):
R# install.packages(c("devtools", "remotes"))
devtools::install()
The package requires:

terra (raster handling)
sf (shapefiles)
dplyr (data manipulation)
ggplot2 (plotting)

Usage
Load the package:
Rlibrary(cmip6extremes)
1. Download daily data from NASA NEX-GDDP-CMIP6
R# Build URL for tasmax in 2050 (GFDL-ESM4, SSP5-8.5)
url <- build_nex_gddp_url(
  model    = "GFDL-ESM4",
  scenario = "ssp585",
  variable = "tasmax",
  ensemble = "r1i1p1f1",   # most common
  grid     = "gn",
  year     = 2050
)

print(url)
# Download to local file
download_nex_gddp(url, destfile = "tasmax_GFDL-ESM4_ssp585_2050.nc", overwrite = TRUE)
2. Clip to a study area (e.g., basin or country shapefile)
Rclip_nex_gddp_to_shape(
  nc_path    = "tasmax_GFDL-ESM4_ssp585_2050.nc",
  shape_path = "data/ecuador_basin.gpkg",          # your shapefile
  out_path   = "tasmax_2050_basin.nc",
  var_name   = "tasmax",
  overwrite  = TRUE
)
3. Calculate extreme indices
From tabular daily data (e.g., station or extracted points):
R# Assume daily_values is a data.frame with columns: date, tasmax, tasmin, pr
indices <- calculate_extreme_indices(
  data      = daily_values,
  date_col  = "date",
  tmax_col  = "tasmax",
  tmin_col  = "tasmin",
  pr_col    = "pr",
  baseline_years = c(1995, 2014)   # for R95p threshold
)

print(indices)
From raster NetCDF (yearly maps of indices):
R# Compute multiple indices at once
extremes_rast <- calculate_extreme_indices_raster(
  nc_path       = "tasmax_2050_basin.nc",
  var_name      = "tasmax",
  indices       = c("txx", "tnn"),
  baseline_years = c(1995, 2014),   # optional for r95p if included
  shape_path    = NULL              # already clipped
)

# Plot one year's map
plot_extreme_index_map(
  extremes_rast,
  year  = 2050,
  title = "Annual Maximum Daily Temperature (TXx) - GFDL-ESM4 SSP585 2050"
)
