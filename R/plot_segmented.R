#' plot_segmented
#'
#' Visualise the original and segmented images side by side with an optional divider and titles.
#'
#' @param original_img A \code{magick-image} object representing the original image.
#' @param segmented_img A \code{magick-image} object representing the segmented image.
#' @param divider Character. One of \code{"line"}, \code{"arrow"}, or \code{"none"}. Default is \code{"line"}.
#' @param show_title Logical. Whether to show image titles above the panels. Default is \code{TRUE}.
#'
#' @return A side-by-side visual plot of the two images using the \code{grid} graphics system.
#'
#' @importFrom grid grid.newpage pushViewport viewport grid.layout grid.raster grid.text grid.lines grid.arrows popViewport gpar
#' @importFrom grDevices as.raster
#' @export
plot_segmented <- function(original_img, segmented_img, divider = "line", show_title = TRUE) {
  # Validate divider input
  divider <- match.arg(divider, choices = c("line", "arrow", "none"))

  # Convert magick images to raster
  original_raster <- grDevices::as.raster(original_img)
  segmented_raster <- grDevices::as.raster(segmented_img)

  # Start new page with 2-column layout
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 2)))

  # Title vertical position
  title_y <- if (show_title) 0.95 else -9999

  # Plot original image
  grid::pushViewport(grid::viewport(layout.pos.col = 1))
  grid::grid.raster(original_raster)
  if (show_title) {
    grid::grid.text("Original Image", y = title_y, gp = grid::gpar(fontface = "bold"))
  }
  grid::popViewport()

  # Plot segmented image
  grid::pushViewport(grid::viewport(layout.pos.col = 2))
  grid::grid.raster(segmented_raster)
  if (show_title) {
    grid::grid.text("Segmented Image", y = title_y, gp = grid::gpar(fontface = "bold"))
  }
  grid::popViewport()

  # Add divider
  if (divider == "line") {
    grid::grid.lines(x = 0.5, y = c(0, 1), gp = grid::gpar(lwd = 2))
  } else if (divider == "arrow") {
    grid::grid.arrows(x = 0.5, y = c(0, 1), gp = grid::gpar(lwd = 2))
    #grid::grid.arrows(x0 = 0.45, y0 = 0.5, x1 = 0.55, y1 = 0.5, gp = grid::gpar(lwd = 2))
  }
}
