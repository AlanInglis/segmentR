#' plot_palette
#'
#' Display a simple horizontal strip of colours from a palette.
#'
#' @description
#' This function visualises a colour palette as a sequence of tiles.
#' The input can be either:
#'
#' - A character vector of hex colour strings, or
#' - A data frame with a specified column containing hex codes.
#'
#' @param x A character vector of hex colours, or a data frame.
#' @param color_col Optional string. If `x` is a data frame, the name of the column
#'        containing colour hex codes.
#'
#' @return A `ggplot2` object displaying the palette.
#'
#' @examples
#' # From a vector
#' plot_palette(c("#FF0000", "#00FF00", "#0000FF"))
#'
#' # From a data frame
#' df <- data.frame(col = c("#FF0000", "#0000FF", "#00FF00"))
#' plot_palette(df, color_col = "col")
#'
#' @import ggplot2
#' @export
plot_palette <- function(x, color_col = NULL) {
  if (is.character(x)) {
    colours <- x
  } else if (is.data.frame(x) && !is.null(color_col)) {
    if (!color_col %in% names(x)) {
      stop("Specified column '", color_col, "' does not exist in the data frame.")
    }
    colours <- x[[color_col]]
  } else {
    stop("Input must be a character vector or a data frame with a specified colour column.")
  }

  # Convert factors to character
  if (is.factor(colours)) {
    colours <- as.character(colours)
  }

  # Standardise hex case
  #colours <- toupper(colours)

  df <- data.frame(
    colour = colours,
    index = seq_along(colours),
    stringsAsFactors = FALSE
  )

  ggplot(df, aes(x = index, y = 1, fill = colour)) +
    geom_tile(height = 0.5) +
    scale_fill_manual(values = df$colour) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(ylim = c(0, 2)) +
    theme_void() +
    theme(legend.position = "none")
}
