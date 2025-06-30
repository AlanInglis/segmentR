#' img_to_palette
#'
#' Generate a colour palette from an image based on the most frequent colours.
#'
#' @param img magick image object *or* path/URL to the image.
#' @param n Integer. Number of top colours to extract. Default is NULL (all).
#' @param avg_cols Logical. Whether to average colours into clusters. Default is FALSE.
#' @param exclude Logical. Whether to exclude near-black/white shades. Default is FALSE.
#' @param n_clusters Integer or NULL. Number of clusters to use for averaging. If NULL, clustering is skipped.
#' @param custom_exclude Optional character vector of hex colours to exclude.
#' @importFrom magick image_read
#' @return A character vector of hex codes representing the palette.
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
