#' avg_hex
#'
#' Compute the average hex colour per group.
#'
#' @param df A data frame with hex colour values and grouping labels.
#' @param hex_col Character. Name of the column containing hex values.
#' @param group_col Character. Name of the column containing group labels.
#'
#' @return A data frame with columns: \code{group}, \code{avg_color}, and \code{freq}.
#'
#' @examples
#' df <- data.frame(
#'   hex = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF"),
#'   group = c(1, 1, 2, 2, 3)
#' )
#' avg_hex(df, hex_col = "hex", group_col = "group")
#'
#' @importFrom grDevices col2rgb rgb
#' @export
avg_hex <- function(df, hex_col = "hex", group_col = "group") {
  if (!hex_col %in% names(df)) {
    stop(paste0("Column '", hex_col, "' not found in data."))
  }
  if (!group_col %in% names(df)) {
    stop(paste0("Column '", group_col, "' not found in data."))
  }

  # Function to average hex colours
  average_hex <- function(hex_vals) {
    rgb_matrix <- grDevices::col2rgb(hex_vals)
    mean_rgb <- rowMeans(rgb_matrix)
    grDevices::rgb(mean_rgb[1], mean_rgb[2], mean_rgb[3], maxColorValue = 255)
  }

  # Compute group frequency
  freq <- as.data.frame(table(df[[group_col]]))
  names(freq) <- c("group", "freq")

  # Compute average hex per group
  avg_colors <- tapply(df[[hex_col]], df[[group_col]], average_hex)

  out <- data.frame(
    group = names(avg_colors),
    avg_color = unname(avg_colors),
    stringsAsFactors = FALSE
  )

  merge(out, freq, by = "group")
}
