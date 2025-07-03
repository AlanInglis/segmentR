#' exclude_shades
#'
#' @description
#' Removes near-black and near-white colours from a palette based on Euclidean
#' RGB distance from black (`#000000`) and white (`#FFFFFF`). You can also
#' exclude custom hex codes explicitly.
#'
#' @param data A data frame with a column `hex` containing hexadecimal colour strings.
#' @param threshold Numeric (default = 0.1). Distance from black or white below which
#'   a colour will be excluded. Range is from 0 to sqrt(3).
#' @param custom_exclude Optional character vector of hex codes to exclude manually.
#'
#' @return A filtered data frame, preserving the structure of the input.
#'
#' @examples
#' pal <- data.frame(
#' hex = c("#000000", "#FFFFFF", "#FF0000", "#00FF00", "#0000FF"))
#' exclude_shades(pal, threshold = 0.1)
#' exclude_shades(pal, custom_exclude = "#00FF00")
#'
#' @importFrom grDevices col2rgb
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


