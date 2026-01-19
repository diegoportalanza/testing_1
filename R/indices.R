#' Calculate ETCCDI-style extreme climate indices from daily values
#'
#' Computes annual extreme temperature and precipitation indices from daily data.
#' Follows conventions similar to ETCCDI (Expert Team on Climate Change Detection and Indices).
#'
#' @details
#' Indices calculated:
#' - TXx: Monthly maximum value of daily maximum temperature
#' - TNx: Monthly maximum value of daily minimum temperature
#' - TXn: Monthly minimum value of daily maximum temperature
#' - TNn: Monthly minimum value of daily minimum temperature
#' - RX1day: Monthly maximum 1-day precipitation
#' - R10mm: Annual count of days when precipitation >= 10 mm
#' - R95p: Annual total precipitation from days exceeding the 95th percentile (baseline period)
#'
#' @note R95p uses a user-provided baseline period (default: 1961-1990) to compute the threshold,
#' following ETCCDI recommendations for consistency across time.
#'
#' @param data A data.frame or tibble with daily observations.
#' @param date_col Character. Name of the date column (must be Date or coercible).
#' @param tmax_col Character. Name of daily maximum temperature column (°C).
#' @param tmin_col Character. Name of daily minimum temperature column (°C).
#' @param pr_col Character. Name of daily precipitation column (mm/day).
#' @param baseline_years Integer vector of length 2. Years defining the baseline for R95p threshold
#'   (default: c(1961, 1990)). Set to NULL to use full period (not recommended).
#'
#' @return A data.frame with one row per year, containing the indices.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with synthetic data
#' library(dplyr)
#' daily <- data.frame(
#'   date   = seq(as.Date("1980-01-01"), as.Date("2020-12-31"), by = "day"),
#'   tasmax = runif(14976, 20, 40),
#'   tasmin = runif(14976, 10, 25),
#'   pr     = rgamma(14976, shape = 0.8, scale = 5)  # skewed precip
#' )
#' calculate_extreme_indices(daily, "date", "tasmax", "tasmin", "pr")
#' }
calculate_extreme_indices <- function(
    data,
    date_col,
    tmax_col,
    tmin_col,
    pr_col,
    baseline_years = c(1961, 1990)
) {
  # Load dplyr (add to Imports in DESCRIPTION)
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it.")
  }

  required_cols <- c(date_col, tmax_col, tmin_col, pr_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Prepare data
  df <- data %>%
    dplyr::mutate(
      date  = as.Date(.data[[date_col]]),
      year  = as.integer(format(date, "%Y")),
      month = as.integer(format(date, "%m"))
    ) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::select(year, month, tmax = !!tmax_col, tmin = !!tmin_col, pr = !!pr_col)

  if (nrow(df) == 0) stop("No valid dates after conversion.")

  # Compute R95p threshold from baseline period
  if (!is.null(baseline_years) && length(baseline_years) == 2) {
    baseline_df <- df %>% dplyr::filter(year >= baseline_years[1] & year <= baseline_years[2])
    if (nrow(baseline_df) < 365 * 10) {
      warning("Baseline period has few observations (<10 years). Threshold may be unreliable.")
    }
    r95_threshold <- stats::quantile(baseline_df$pr, 0.95, na.rm = TRUE, names = FALSE)
  } else {
    warning("No baseline period provided → using full period for R95p (not ETCCDI-recommended).")
    r95_threshold <- stats::quantile(df$pr, 0.95, na.rm = TRUE, names = FALSE)
  }

  # Annual summaries
  yearly <- df %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      # Temperature extremes
      txx = max(tmax, na.rm = TRUE),
      txn = max(tmin, na.rm = TRUE),
      tnx = min(tmax, na.rm = TRUE),
      tnn = min(tmin, na.rm = TRUE),

      # Precipitation extremes
      rx1day   = max(pr, na.rm = TRUE),
      r10mm    = sum(pr >= 10, na.rm = TRUE),
      r95p     = sum(pr[pr > r95_threshold], na.rm = TRUE),

      # Optional extras (uncomment as needed)
      # prcptot  = sum(pr, na.rm = TRUE),
      # rx5day   = max(zoo::rollapply(pr, 5, sum, na.rm = TRUE, fill = NA, align = "right"), na.rm = TRUE),
      # sdii     = mean(pr[pr > 1], na.rm = TRUE),

      n_days   = n(),  # number of days with data
      .groups  = "drop"
    ) %>%
    dplyr::mutate(
      across(c(txx, txn, tnx, tnn, rx1day, r95p), ~ifelse(is.infinite(.), NA_real_, .))
    )

  # Add warning if any year has few days
  if (any(yearly$n_days < 300)) {
    warning("Some years have <300 days of data; indices may be unreliable.")
  }

  yearly
}
