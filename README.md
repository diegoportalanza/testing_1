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
