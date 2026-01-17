# cmip6extremes

Tools for downloading CMIP6 NEXT GGPD data and calculating extreme climate
indices from daily temperature and precipitation inputs.

## Installation

```r
# install.packages("remotes")
# remotes::install_local(".")
```

## Usage

```r
library(cmip6extremes)

# Build a dataset URL (example values)
# Note: set a base URL if your GGPD host differs.
options(cmip6extremes.base_url = "https://cmip6-next.ggpd.org")
url <- build_ggpd_url(
  model = "GFDL-ESM4",
  scenario = "ssp585",
  variable = "tasmax",
  year = 2050
)

# Build a NEX-GDDP-CMIP6 URL from the NASA THREDDS catalog
# (If you see "could not find function", run library(cmip6extremes) or use
# the namespace-qualified call shown below.)
options(cmip6extremes.nex_gddp_base_url = "https://ds.nccs.nasa.gov/thredds/fileServer/AMES/NEX/GDDP-CMIP6")
nex_url <- cmip6extremes::build_nex_gddp_url(
  model = "ACCESS-CM2",
  scenario = "historical",
  variable = "pr",
  ensemble = "r1i1p1f1",
  grid = "gn",
  year = 2014
)

# Download data
file_path <- download_cmip6_next_ggpd(url, "tasmax_2050.nc")

# Clip daily NetCDF using a shapefile
clipped_path <- clip_ggpd_to_shape(
  nc_path = file_path,
  shape_path = "data/basin.shp",
  out_path = "tasmax_2050_basin.nc",
  var_name = "tasmax",
  overwrite = TRUE
)

# Calculate indices from a data.frame of daily values
indices <- calculate_extreme_indices(
  data = daily_values,
  date_col = "date",
  tmax_col = "tasmax",
  tmin_col = "tasmin",
  pr_col = "pr"
)

# Calculate a raster index and plot a map
rx1day_raster <- calculate_extreme_indices_raster(
  nc_path = clipped_path,
  var_name = "pr",
  index = "rx1day"
)

rx1day_map <- plot_extreme_index_map(rx1day_raster, year = 2050, title = "RX1day")
```
