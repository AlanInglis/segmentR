#' draw_border
#'
#' Add visible borders between colour segments in a segmented image.
#'
#' @description
#' Detects edges between segments in an RGB array and overlays a border using
#' a specified colour. Returns a `magick-image` object with borders drawn.
#'
#' @param segmented_array A numeric RGB array (`height × width × 3`) representing
#'   a segmented image.
#' @param edge_color Colour name or hex code for the borders (default: `"black"`).
#'
#' @return A `magick-image` object with segment borders highlighted.
#'
#' @examples
#' # Load and segment example image
#' img_path <- system.file("extdata", "sample_img.png", package = "segmentR")
#' img <- read_image(img_path, width = 341, height = 512)
#' seg <- segment_image(img, n = 6)
#'
#' # Add border
#' bordered <- draw_border(seg, edge_color = "red")
#' bordered
#'
#' @importFrom magick image_read
#' @importFrom grDevices col2rgb
#' @export
draw_border <- function(segmented_array, edge_color = "black") {
  # Convert the edge color from text to RGB values
  edge_rgb <- grDevices::col2rgb(edge_color) / 255

  # Function to detect edges
  detect_edges <- function(mat) {
    edges <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
    for (i in 1:(nrow(mat) - 1)) {
      for (j in 1:(ncol(mat) - 1)) {
        if (any(mat[i, j, ] != mat[i + 1, j, ]) || any(mat[i, j, ] != mat[i, j + 1, ])) {
          edges[i, j] <- 1
        }
      }
    }
    return(edges)
  }

  # Detect edges
  edges <- detect_edges(segmented_array)

  # Apply edges to the segmented image
  for (i in 1:3) {
    segmented_array[, , i][edges == 1] <- edge_rgb[i]  # Set edge pixels to the specified color
  }

  # Convert back to magick-image object
  segmented_img <- magick::image_read(segmented_array)

  return(segmented_img)
}
