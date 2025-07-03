#' plot_segmented
#'
#' Display the original and segmented images side by side using grid graphics.
#'
#' @description
#' Visualise an image alongside its segmented version, with optional titles and a divider line between them.
#'
#' @param original_img A `magick-image` object representing the original image.
#' @param segmented_img A `magick-image` object representing the segmented image.
#' @param divider Character. Divider style between the images. Either `"line"` (default) or `"none"`.
#' @param show_title Logical. Show titles above each image? Default is `TRUE`.
#'
#' @return This function produces a visual side-by-side plot using the `grid` system. Nothing is returned.
#'
#' @examples
#' img_path <- system.file("extdata", "sample_img.png", package = "segmentR")
#' img <- read_image(img_path, width = 341, height = 512)
#' segmented <- segment_image(img, n = 4)
#' plot_segmented(img, segmented)
#' img_seg <- draw_border(segmented, edge_color = "red")  # Add border to segments
#' plot_segmented(img, img_seg)  # Plot with borders
#'
#' @importFrom grid grid.newpage pushViewport viewport grid.layout grid.raster grid.text grid.lines popViewport gpar
#' @importFrom grDevices as.raster
#' @export
plot_segmented <- function(original_img, segmented_img, divider = "line", show_title = TRUE) {
  # Validate divider input
  divider <- match.arg(divider, choices = c("line",  "none"))

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
  }
}
