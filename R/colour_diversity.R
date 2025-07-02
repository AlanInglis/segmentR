###############################################################################
## colour_diversity.R
## ---------------------------------------------------------------------------
## Compute three common diversity metrics for colour compositions:
##   * Shannon entropy (richness × evenness)
##   * Gini–Simpson index (probability of difference)
##   * Pielou’s evenness (entropy normalised by richness)
##
## Works on:
##   • magick images  (object or file/URL)
##   • RGB arrays     (height × width × 3, 0–255 or 0–1)
##   • factor/char matrices from classify_pixels_by_cluster()
##   • segmentR_raster objects
###############################################################################

#' colour_diversity
#'
#' Quantify colour diversity in an image or cluster map using one or more of:
#' * **Shannon entropy**  \eqn{H = -\sum p_i \ln p_i}
#' * **Gini–Simpson**    \eqn{G = 1 - \sum p_i^2}
#' * **Pielou evenness** \eqn{J = H / \ln(K)}
#'
#' where \eqn{p_i} is the proportion of pixels in colour *i* and *K* is the
#' number of distinct colours.
#'
#' @param x        A colour source: magick-image, file/URL, RGB array,
#'                 factor matrix, or `segmentR_raster`.
#' @param measure  One or more of `"shannon"`, `"gini"`, `"evenness"`.
#'                 Default returns all three.
#'
#' @return A named numeric vector containing the requested metric(s).
#'
#' @examples
#' \dontrun{
#' img <- read_image_from_url(
#'   "https://upload.wikimedia.org/wikipedia/en/0/02/Homer_Simpson_2006.png")
#' img <- white_balance_auto(img, "percentile")
#'
#' # Whole-image diversity
#' colour_diversity(img)
#'
#' # Diversity of a classified cluster map
#' pal <- img_to_palette(img, k = 15)
#' cl  <- classify_pixels_by_cluster(img, pal, return = "factor")
#' colour_diversity(cl, c("shannon", "evenness"))
#' }
#'
#' @importFrom magick image_read image_data
#' @export
colour_diversity <- function(x,
                             measure = c("shannon", "gini", "evenness")) {

  measure <- match.arg(measure, several.ok = TRUE)

  ## --------------------------------------------------------------------------
  ## 1. Extract vector of colour labels or IDs
  ## --------------------------------------------------------------------------
  if (inherits(x, "segmentR_raster")) {

    col_vec <- as.vector(x$data)

  } else if (inherits(x, "magick-image") || is.character(x)) {

    if (is.character(x)) x <- magick::image_read(x)

    raw <- magick::image_data(x, channels = "rgb")              # raw bytes
    arr <- array(as.integer(raw), dim = dim(raw))               # int 0–255
    arr <- aperm(arr, c(3, 2, 1))                               # H × W × 3
    col_vec <- as.vector(
      grDevices::rgb(arr[,,1], arr[,,2], arr[,,3], maxColorValue = 255)
    )

  } else if (is.array(x) && length(dim(x)) == 3 && dim(x)[3] == 3) {

    arr <- if (max(x) <= 1) round(x * 255) else x
    col_vec <- as.vector(
      grDevices::rgb(arr[,,1], arr[,,2], arr[,,3], maxColorValue = 255)
    )

  } else if (is.matrix(x)) {

    col_vec <- as.vector(x)

  } else {
    stop("Unsupported input type for `colour_diversity()`.", call. = FALSE)
  }

  ## Remove possible NAs (e.g., from logical masks)
  col_vec <- col_vec[!is.na(col_vec)]
  if (!length(col_vec))
    return(setNames(rep(NA_real_, length(measure)), measure))

  ## --------------------------------------------------------------------------
  ## 2. Colour proportions
  ## --------------------------------------------------------------------------
  p <- as.vector(prop.table(table(col_vec)))
  k <- length(p)

  ## --------------------------------------------------------------------------
  ## 3. Metrics
  ## --------------------------------------------------------------------------
  out <- numeric(length(measure)); names(out) <- measure

  if ("shannon"  %in% measure)
    out["shannon"]  <- -sum(p * log(p))

  if ("gini" %in% measure)
    out["gini"] <- 1 - sum(p^2)

  if ("evenness" %in% measure) {
    out["evenness"] <- if (k > 1) ( -sum(p * log(p)) / log(k) ) else 0
  }

  out
}
