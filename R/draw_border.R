#' Draw Border on Segmented Image
#'
#' @description
#' This function draws borders on a segmented image by detecting edges between different segments and allows the user to specify the border color.
#'
#' @details
#' The function takes a numeric array representing a segmented image and detects edges between segments. It then draws borders along these edges by setting the edge pixels to a specified color. The resulting image is converted back to a magick-image object and returned.
#'
#' @param segmented_array A numeric array representing the segmented image
#' @param edge_color A character string specifying the color of the edges (default is "black")
#' @return A magick-image object with borders drawn
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
