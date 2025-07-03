#' img_to_palette
#'
#' Extract a simplified colour palette from an image using frequency and optional clustering.
#'
#' @description
#' Generates a colour palette by extracting the most common colours from an image.
#' Supports optional removal of near-white/black shades and grouping via k-means
#' clustering. Returns either raw hex values or perceptually averaged colours.
#'
#' @param img A `magick-image` object, or a character path/URL to an image file.
#' @param n Integer. Number of top colours to extract. Default is `NULL` (return all).
#' @param avg_cols Logical. Whether to average colours into clusters (`FALSE` by default).
#' @param exclude Logical. Whether to exclude near-black and near-white shades (`FALSE` by default).
#' @param n_clusters Integer or `NULL`. Number of clusters to use for averaging (if `avg_cols = TRUE`).
#' @param custom_exclude Optional character vector of hex colours to exclude manually.
#'
#' @return A character vector of hex codes:
#'   * If `avg_cols = TRUE`, returns averaged colours (`avg_color`)
#'   * Otherwise, returns the most frequent raw colours (`hex`)
#'
#' @examples
#' img_path <- system.file("extdata", "sample_img.png", package = "segmentR")
#' img <- read_image(img_path)
#' img_to_palette(img, n = 6)
#'
#' @seealso [get_top_col()], [exclude_shades()], [plot_palette()]
#' @importFrom magick image_read
#' @export
img_to_palette <- function(img,
                            n = NULL,
                            avg_cols = FALSE,
                            exclude = FALSE,
                            n_clusters = NULL,
                            custom_exclude = NULL) {
  # If input is a path or URL, read as magick image
  if (is.character(img)) img <- magick::image_read(img)

  colours <- suppressWarnings(
    get_top_col(
      img = img, # Pass the image object, not path
      n = n,
      avg_cols = avg_cols,
      exclude = exclude,
      n_clusters = n_clusters,
      custom_exclude = custom_exclude
    )
  )

  palette_hex <- if (avg_cols && "avg_color" %in% names(colours)) {
    colours$avg_color
  } else {
    colours$hex
  }

  print(plot_palette(palette_hex))
  return(palette_hex)
}
