#' avg_hex
#'
#' Compute the average hexadecimal colour for each group.
#'
#' @description
#' Given a data frame of hex colours and group labels, this function calculates the
#' average RGB colour per group and returns a summary table. The output includes
#' the group ID, averaged colour (as hex), and the number of observations per group.
#'
#' @param df A data frame containing at least two columns: one with hexadecimal
#'   colour values and one with group labels.
#' @param hex_col Character string. The name of the column in `df` containing
#'   hex colour values (e.g., `"#FF0000"`).
#' @param group_col Character string. The name of the column in `df` containing
#'   group labels (e.g., cluster assignments).
#'
#' @return A data frame with columns:
#'   * `group` – group label
#'   * `avg_color` – average hex colour per group
#'   * `freq` – number of rows per group
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
    grDevices::rgb(red = mean_rgb[1],
                   green = mean_rgb[2],
                   blue = mean_rgb[3],
                   maxColorValue = 255)
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
