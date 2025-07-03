#' colour_diversity
#'
#' Quantify colour diversity in an image or cluster map using common ecological indices.
#'
#' @description
#' Computes one or more diversity metrics based on the frequency of colours
#' in an image or cluster assignment. Supports:
#'
#' - **Shannon entropy**: \eqn{H = -\sum p_i \ln p_i}
#' - **Gini–Simpson index**: \eqn{G = 1 - \sum p_i^2}
#' - **Pielou evenness**: \eqn{J = H / \ln(K)}
#'
#' where \eqn{p_i} is the proportion of pixels in colour *i* and *K* is the
#' number of distinct colours. Supports both raw image data and cluster maps.
#'
#' @param x A colour source: `magick-image`, file/URL path, RGB array,
#'   factor matrix, or a `segmentR_raster` object.
#' @param measure Character vector specifying one or more metrics to compute.
#'   Options are `"shannon"`, `"gini"`, and `"evenness"`. Defaults to all three.
#'
#' @return A named numeric vector with the requested metric(s).
#'
#' @examples
#' # Load example image
#' img_path <- system.file("extdata", "sample_img.png", package = "segmentR")
#' img <- read_image(img_path, width = 341, height = 512)
#'
#' # Whole-image diversity
#' colour_diversity(img)
#'
#' # Diversity of clustered image
#' pal <- img_to_palette(img, n = 12)
#' cl  <- classify_pixels_by_cluster(img, pal, output = "factor")
#' colour_diversity(cl, c("shannon", "evenness"))
#'
#' @seealso [classify_pixels_by_cluster()], [segment_image()]
#' @importFrom magick image_read image_data
#' @importFrom grDevices rgb
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
