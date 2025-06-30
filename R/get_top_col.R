#' get_top_col
#'
#' Extract the top colours from an image file (local, URL, or magick image object),
#' with optional exclusion of black/white shades and colour grouping via k-means clustering.
#'
#' @param img Character (file path/URL) or magick image object or array.
#' @param n Integer. Number of top colours to extract. Default is NULL (returns all).
#' @param exclude Logical. Exclude near-black and near-white colours. Default is TRUE.
#' @param sig Integer. Decimal places to round percentage values. Default is 4.
#' @param avg_cols Logical. Whether to average colours by k-means grouping. Default is TRUE.
#' @param n_clusters Integer or NULL. Number of clusters for grouping if \code{avg_cols = TRUE}. If NULL, clustering is skipped. Default is 5.
#' @param custom_exclude Optional character vector of hex colours to exclude.
#' @importFrom pixmap pixmapRGB
#' @importFrom magick image_data
#' @return A data frame with hex codes, frequency, percentage, and optionally average colours.
#' @export

get_top_col <- function(img,
                        n = NULL,
                        exclude = TRUE,
                        sig = 4,
                        avg_cols = TRUE,
                        n_clusters = 5,
                        custom_exclude = NULL) {
  # Accept character (path/URL), magick image, array, or pixmap
  if (is.character(img)) {
    # Handle URLs or file paths
    if (grepl("^https?://", img)) {
      image <- read_image_from_url(img)
    } else {
      ext <- tolower(tools::file_ext(img))
      if (ext %in% c("jpg", "jpeg")) {
        image <- jpeg::readJPEG(img)
      } else if (ext == "png") {
        image <- png::readPNG(img)
      } else {
        stop("Unsupported file format. Only PNG and JPG files are supported.")
      }
    }
    pic <- suppressWarnings(pixmap::pixmapRGB(image))
  } else if (inherits(img, "magick-image")) {
    # Convert magick image to array in [0,1] if necessary
    arr <- magick::image_data(img, channels = "rgb")
    arr <- array(strtoi(arr, 16L) / 255, dim = dim(arr))
    arr <- aperm(arr, c(2, 3, 1)) # height × width × channels
    pic <- suppressWarnings(pixmap::pixmapRGB(arr))
  } else if (is.array(img) && length(dim(img)) == 3 && dim(img)[3] == 3) {
    # Already an RGB array in [0,1]
    pic <- suppressWarnings(pixmap::pixmapRGB(img))
  } else if (inherits(img, "pixmapRGB")) {
    # Already a pixmap
    pic <- img
  } else {
    stop("img must be a file path, URL, magick image, pixmapRGB, or RGB array")
  }

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
