#' Calculate extreme indices from daily values
#'
#' Compute a set of common temperature and precipitation extremes by year.
#'
#' @param data A data.frame containing daily data.
#' @param date_col Column name for the dates.
#' @param tmax_col Column name for daily maximum temperature.
#' @param tmin_col Column name for daily minimum temperature.
#' @param pr_col Column name for daily precipitation.
#'
#' @return A data.frame with yearly extreme indices.
#' @export
#'
#' @examples
#' daily <- data.frame(
#'   date = as.Date("2000-01-01") + 0:9,
#'   tasmax = runif(10, 25, 40),
#'   tasmin = runif(10, 15, 25),
#'   pr = runif(10, 0, 20)
#' )
#' calculate_extreme_indices(daily, "date", "tasmax", "tasmin", "pr")
calculate_extreme_indices <- function(data,
                                       date_col,
                                       tmax_col,
                                       tmin_col,
                                       pr_col) {
  required_cols <- c(date_col, tmax_col, tmin_col, pr_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  dates <- as.Date(data[[date_col]])
  if (any(is.na(dates))) {
    stop("date_col must be coercible to Date values.")
  }

  tmax <- data[[tmax_col]]
  tmin <- data[[tmin_col]]
  pr <- data[[pr_col]]

  year <- as.integer(format(dates, "%Y"))
  data_year <- data.frame(year = year, tmax = tmax, tmin = tmin, pr = pr)

  r95_threshold <- stats::quantile(pr, 0.95, na.rm = TRUE, names = FALSE)

  yearly <- stats::aggregate(
    data_year[, c("tmax", "tmin", "pr")],
    by = list(year = data_year$year),
    FUN = function(x) x
  )

  result <- data.frame(
    year = yearly$year,
    txx = vapply(yearly$tmax, max, numeric(1), na.rm = TRUE),
    txn = vapply(yearly$tmin, max, numeric(1), na.rm = TRUE),
    tnx = vapply(yearly$tmax, min, numeric(1), na.rm = TRUE),
    tnn = vapply(yearly$tmin, min, numeric(1), na.rm = TRUE),
    rx1day = vapply(yearly$pr, max, numeric(1), na.rm = TRUE),
    r10mm = vapply(yearly$pr, function(x) sum(x >= 10, na.rm = TRUE), numeric(1)),
    r95p = vapply(yearly$pr, function(x) sum(x[x > r95_threshold], na.rm = TRUE), numeric(1))
  )

  result
}
