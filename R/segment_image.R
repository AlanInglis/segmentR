#' segment_image
#'
#' Segment an image into a specified number of dominant colours using k-means clustering.
#'
#' @description
#' Converts an image into an RGB matrix and uses k-means to cluster pixels into `n_colors` groups.
#' Each pixel is recoloured using its cluster centroid. The result is returned as a numeric RGB array.
#'
#' @param img A \code{magick-image} object.
#' @param n_colors Integer ≥ 2. Number of colour clusters. Default is 5.
#'
#' @return A numeric array with the same dimensions as the input image,
#'         where each pixel is assigned the RGB value of its cluster centroid.
#'
#' @examples
#' img_path <- system.file("extdata", "sample_img.png", package = "segmentR")
#' img <- read_image(img_path, width = 300)
#' seg <- segment_image(img, n_colors = 2)
#' plot_segmented(img, seg)
#'
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
