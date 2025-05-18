#' read_image_from_url
#'
#' Read a PNG or JPG image from a URL and return the image data.
#'
#' @param path A character string representing the URL of the image file.
#'
#' @return An object containing the image data. If the image is a JPG, the object will be of class "array".
#' If the image is a PNG, the object will be of class "matrix".
#'
#' @examples
#' \dontrun{
#' img <- read_image_from_url("https://example.com/image.jpg")
#' }
#'
#' @importFrom jpeg readJPEG
#' @importFrom png readPNG
#'
#' @export
read_image_from_url <- function(path) {
  file_ext <- tolower(tools::file_ext(path))
  tf <- tempfile(fileext = paste0(".", file_ext))

  utils::download.file(path, tf, mode = "wb", quiet = TRUE)
  on.exit(unlink(tf), add = TRUE)

  switch(
    file_ext,
    jpg  = jpeg::readJPEG(tf),
    jpeg = jpeg::readJPEG(tf),
    png  = png::readPNG(tf),
    stop("Unsupported file format. Only PNG and JPG files are supported.")
  )
}
