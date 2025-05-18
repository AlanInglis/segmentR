#' exclude_shades
#'
#' Exclude near-black and near-white colours based on RGB distance.
#'
#' @param data A data frame with a column `hex` containing HEX colours.
#' @param threshold Numeric (default 0.1). Distance from black or white to consider exclusion.
#' @param custom_exclude Optional character vector of custom HEX colours to exclude.
#'
#' @return Filtered data frame.
#'
#' @export
exclude_shades <- function(data, threshold = 0.1, custom_exclude = NULL) {
  if (!"hex" %in% names(data)) stop("Input data must contain a 'hex' column.")

  rgb <- t(grDevices::col2rgb(data$hex)) / 255
  dist_to_black <- sqrt(rowSums((rgb - 0)^2))
  dist_to_white <- sqrt(rowSums((rgb - 1)^2))

  keep <- dist_to_black > threshold & dist_to_white > threshold

  if (!is.null(custom_exclude)) {
    keep <- keep & !(data$hex %in% custom_exclude)
  }

  data[keep, , drop = FALSE]
}


