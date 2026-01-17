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
url <- build_ggpd_url(
  model = "GFDL-ESM4",
  scenario = "ssp585",
  variable = "tasmax",
  year = 2050
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
