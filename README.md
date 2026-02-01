# Guayaquil OSM + rayrender scene

This repository contains an R script that downloads OpenStreetMap data around an urban area of Guayaquil, Ecuador and renders a 3D scene using `rayrender`.

## What it does
- Defines a circular area of interest (AOI) around `lon = -79.9100`, `lat = -2.1700`.
- Fetches buildings, landuse, parks, water, roads, and railways from Overpass via `osmdata`.
- Clips, projects, and recenters geometries for local rendering.
- Estimates building heights and applies a height-based color palette.
- Renders a high‑resolution image (`guayaquil_detallado.png`).

## Requirements
Install R packages:

```r
install.packages(c("pacman", "osmdata", "sf", "dplyr", "rayrender"))
```

## Run
From the repo root:

```r
source("script1.R")
```

The render will be saved as `guayaquil_detallado.png` in the working directory.
