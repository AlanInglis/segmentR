#' get_top_col
#'
#' Extract the top colours from an image file (local or URL), with optional exclusion of black/white shades
#' and colour grouping via k-means clustering.
#'
#' @param path Character. File path or URL to a PNG or JPG image.
#' @param n Integer. Number of top colours to extract. Default is NULL (returns all).
#' @param exclude Logical. Exclude near-black and near-white colours. Default is TRUE.
#' @param sig Integer. Decimal places to round percentage values. Default is 4.
#' @param avg_cols Logical. Whether to average colours by k-means grouping. Default is TRUE.
#' @param n_clusters Integer or NULL. Number of clusters for grouping if \code{avg_cols = TRUE}. If NULL, clustering is skipped. Default is 5.
#' @param custom_exclude Optional character vector of hex colours to exclude.
#'
#' @return A data frame with hex codes, frequency, percentage, and optionally average colours.
#'
#' @export
get_top_col <- function(path,
                        n = NULL,
                        exclude = TRUE,
                        sig = 4,
                        avg_cols = TRUE,
                        n_clusters = 5,
                        custom_exclude = NULL) {

  if (grepl("^https?://", path)) {
    image <- read_image_from_url(path)
  } else {
    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("jpg", "jpeg")) {
      image <- jpeg::readJPEG(path)
    } else if (ext == "png") {
      image <- png::readPNG(path)
    } else {
      stop("Unsupported file format. Only PNG and JPG files are supported.")
    }
  }

  pic <- suppressWarnings(pixmap::pixmapRGB(image))
  rgb_hex <- grDevices::rgb(pic@red, pic@green, pic@blue)

  colour_table <- sort(table(rgb_hex), decreasing = TRUE)
  df_col <- data.frame(
    hex = names(colour_table),
    freq = as.integer(colour_table),
    stringsAsFactors = FALSE
  )
  df_col$col_percent <- round((df_col$freq / sum(df_col$freq)) * 100, sig)

  if (exclude) {
    df_col <- exclude_shades(df_col, custom_exclude = custom_exclude)
  }

  if (!is.null(n)) {
    if (n > nrow(df_col)) {
      stop("Requested top n colours exceeds number of available colours after processing.")
    }
    df_col <- df_col[1:n, ]
  }

  if (avg_cols && !is.null(n_clusters)) {
    df_col$group <- group_cols(df_col$hex, n_clusters = n_clusters)$group
    df_col <- avg_hex(df_col, hex_col = "hex", group_col = "group")
    df_col <- df_col[order(df_col$freq, decreasing = TRUE), ]
    df_col$group <- NULL
  }

  return(df_col)
}
