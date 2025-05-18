#' Segment Image Colors
#'
#' @description
#' This function segments an image into a specified number of colors using k-means clustering.
#'
#' @details
#' The function takes an image object and converts it into a matrix of RGB values.
#' It then applies k-means clustering to group the colours into the specified number of clusters.
#' Each pixel in the image is replaced with the color of its corresponding cluster center. T
#' he segmented image is returned as a numeric array with the same dimensions as the original image.
#' @param img An image object
#' @param n_colors Number of colors to segment into
#' @return A segmented image as a numeric array
#' @importFrom magick image_data
#' @importFrom stats kmeans
#' @export
segment_image <- function(img, n_colors = 5) {
  # Convert image to a matrix of RGB values
  img_array <- as.numeric(magick::image_data(img, channels = "RGB"))
  original_dim <- dim(img_array)
  dim(img_array) <- c(prod(original_dim[1:2]), 3)

  # Perform k-means clustering
  kmeans_result <- stats::kmeans(img_array, centers = n_colors)

  # Replace each pixel with its cluster center color
  segmented_array <- kmeans_result$centers[kmeans_result$cluster, ]

  # Ensure color values are in the correct range (0-255)
  segmented_array <- pmin(pmax(segmented_array, 0), 255)

  # Reshape the array to the original dimensions
  dim(segmented_array) <- original_dim

  return(segmented_array)
}
