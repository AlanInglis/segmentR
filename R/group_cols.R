#' group_cols
#'
#' Cluster a set of hex colours into perceptually similar groups using k-means in RGB space.
#'
#' @description
#' Converts hex colours to RGB and applies k-means clustering to group visually similar
#' colours. Returns a data frame mapping each colour to a cluster label.
#'
#' @param hex_colors A character vector of hex colour values (e.g., `"#FF0000"`).
#' @param n_clusters Integer. Number of clusters to form (default = 5).
#'
#' @return A data frame with:
#'   * `hex_color`: the original hex code
#'   * `group`: the assigned cluster label (integer)
#'
#' @examples
#' hex_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF")
#' group_cols(hex_colors, n_clusters = 2)
#'
#' @seealso [avg_hex()], [get_top_col()]
#' @importFrom grDevices col2rgb
#' @importFrom stats kmeans
#' @export
group_cols <- function(hex_colors, n_clusters = 5) {
  if (!is.character(hex_colors)) {
    stop("`hex_colors` must be a character vector of hex colour codes.")
  }

  if (length(hex_colors) < n_clusters) {
    stop("Number of colours must be >= number of clusters.")
  }

  rgb_colors <- t(grDevices::col2rgb(hex_colors)) / 255

  clust_result <- stats::kmeans(rgb_colors, centers = n_clusters)

  grouped_colors <- data.frame(
    hex_color = hex_colors,
    group = clust_result$cluster,
    stringsAsFactors = FALSE
  )

  grouped_colors[order(grouped_colors$group, decreasing = TRUE), ]
}
