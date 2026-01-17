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

# Calculate indices from a data.frame of daily values
indices <- calculate_extreme_indices(
  data = daily_values,
  date_col = "date",
  tmax_col = "tasmax",
  tmin_col = "tasmin",
  pr_col = "pr"
)
```
