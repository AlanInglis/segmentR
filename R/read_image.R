#' read_image
#'
#' Read and optionally resize or rotate an image using the \code{magick} package.
#'
#' @description
#' This function reads an image from a specified file path (or URL) and optionally resizes it to the given width/height,
#' and/or rotates it by the specified number of degrees.
#'
#' @details
#' Resizing maintains aspect ratio unless both dimensions are specified. Use \code{rotate} to rotate the image clockwise
#' (e.g., 90, 180, 270).
#'
#' @param img Character. Path or URL to the image file.
#' @param width Integer or NULL. New width in pixels. Default is NULL.
#' @param height Integer or NULL. New height in pixels. Default is NULL.
#' @param rotate Numeric or NULL. Degrees to rotate the image clockwise. Default is NULL.
#'
#' @return A \code{magick-image} object.
#'
#' @examples
#' \dontrun{
#' img <- read_image("https://upload.wikimedia.org/wikipedia/en/0/02/Homer_Simpson_2006.png", width = 300)
#' plot(img)
#' }
#'
#' @importFrom magick image_read image_scale image_rotate
#' @export
read_image <- function(img, width = NULL, height = NULL, rotate = NULL) {
  # Basic checks
  if (!is.character(img) || length(img) != 1) {
    stop("img must be a single character string.")
  }

  # Read image
  img <- magick::image_read(img)

  # Resize logic
  if (!is.null(width) && !is.null(height)) {
    img <- magick::image_scale(img, paste0(width, "x", height, "!"))
  } else if (!is.null(width)) {
    img <- magick::image_scale(img, paste0(width, "x"))
  } else if (!is.null(height)) {
    img <- magick::image_scale(img, paste0("x", height))
  }

  # Rotation
  if (!is.null(rotate)) {
    img <- magick::image_rotate(img, degrees = rotate)
  }

  return(img)
}
