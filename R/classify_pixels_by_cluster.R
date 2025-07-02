#' classify_pixels_by_cluster
#'
#' Assign each pixel in an image to the **nearest** reference colour using
#' CIEDE2000 distance (Lab space by default).
#'
#' @section Return types:
#' * **"factor"** (default) – a `height × width` matrix whose elements are
#'   *factors* labelled by hex codes (one level per palette entry).
#' * **"mask"** – a logical matrix for a single cluster (`TRUE` where the pixel
#'   matches `cluster`, `FALSE` elsewhere).
#' * **"raster"** – a base-R S3 object of class **`segmentR_raster`** containing
#'   the integer cluster map (`data`) plus minimal metadata (`nrow`, `ncol`,
#'   `extent`, `crs`).  Helper methods `print.segmentR_raster()` and
#'   `plot.segmentR_raster()` are exported for convenience; advanced users can
#'   coerce `data` to a {terra} or {raster} object if needed.
#'
#' @param img      A *magick-image*, file/URL, or an RGB array
#'                 (`height × width × 3`, 0–255 or 0–1).
#' @param palette  Character vector of hex codes **or** data-frame with a
#'                 `hex` column (e.g. result of `merge_similar_colours()`).
#' @param space    Colour space for the ΔE calculation (default `"lab"`).
#' @param return   `"factor"`, `"mask"`, or `"raster"` (default `"factor"`).
#' @param cluster  Cluster identifier when `return = "mask"` – may be a hex
#'                 string or a numeric index.
#'
#' @return An object whose class depends on `return` (see *Return types* above).
#'
#' @examples
#' \dontrun{
#' img <- read_image_from_url(
#'   "https://upload.wikimedia.org/wikipedia/en/0/02/Homer_Simpson_2006.png")
#' img <- white_balance_auto(img, "percentile")
#'
#' pal_raw   <- img_to_palette(img, k = 20)
#' pal_clean <- merge_similar_colours(pal_raw, deltaE = 4)
#' pal_hex   <- unique(pal_clean$avg_color)
#'
#' ## Factor matrix
#' cl_mat <- classify_pixels_by_cluster(img, pal_hex)
#' table(cl_mat)
#'
#' ## Logical mask of the dominant colour
#' mask <- classify_pixels_by_cluster(img, pal_hex,
#'                                    return  = "mask",
#'                                    cluster = pal_hex[1])
#' image(mask)
#'
#' ## Lightweight raster
#' rast <- classify_pixels_by_cluster(img, pal_hex, return = "raster")
#' print(rast)
#' plot(rast)  # uses plot.segmentR_raster()
#' }
#'
#' @importFrom magick image_read image_data
#' @importFrom farver decode_colour convert_colour compare_colour
#' @export
classify_pixels_by_cluster <- function(img,
                                       palette,
                                       space   = "lab",
                                       return  = c("factor", "mask", "raster"),
                                       cluster = NULL) {

  return <- match.arg(return)

  if (!requireNamespace("farver", quietly = TRUE))
    stop("Package 'farver' is required.", call. = FALSE)

  ## ---- normalise palette -------------------------------------------------------
  if (is.character(palette))
    palette <- data.frame(hex = toupper(palette), stringsAsFactors = FALSE)
  if (!"hex" %in% names(palette))
    stop("`palette` must be a hex vector or a data-frame with a `hex` column.",
         call. = FALSE)

  pal_hex <- toupper(palette$hex)
  pal_lab <- farver::decode_colour(pal_hex, to = space)            # n × 3

  ## ---- read / coerce image to integer RGB cube -------------------------------
  if (is.character(img)) img <- magick::image_read(img)

  if (inherits(img, "magick-image")) {
    raw <- magick::image_data(img, channels = "rgb")
    arr <- array(strtoi(raw, 16L), dim = dim(raw))                 # 3 × W × H
    arr <- aperm(arr, c(3, 2, 1))                                  # H × W × 3
  } else if (is.array(img) && length(dim(img)) == 3 && dim(img)[3] == 3) {
    arr <- img
    if (max(arr) <= 1) arr <- round(arr * 255)
  } else {
    stop("`img` must be a magick image, path/URL, or RGB array.", call. = FALSE)
  }

  h <- dim(arr)[1]; w <- dim(arr)[2]
  pix_rgb <- matrix(arr, ncol = 3, byrow = FALSE)                  # (H*W) × 3
  pix_lab <- farver::convert_colour(pix_rgb, from = "rgb", to = space)

  ## ---- ΔE matrix & nearest palette index --------------------------------------
  dmat    <- farver::compare_colour(pix_lab, pal_lab,
                                    from_space = space,
                                    to_space   = space,
                                    method     = "CIE2000")
  nearest <- max.col(-dmat)                                        # minimise distance
  cl_mat  <- matrix(nearest, nrow = h, ncol = w, byrow = FALSE)

  ## ---- assemble requested output ----------------------------------------------
  if (return == "factor") {
    fac <- factor(cl_mat, levels = seq_along(pal_hex), labels = pal_hex)
    dim(fac) <- c(h, w)                                            # keep matrix shape
    return(fac)
  }

  if (return == "mask") {
    if (is.null(cluster))
      stop("Supply `cluster` when return = 'mask'.", call. = FALSE)
    if (is.character(cluster))
      cluster <- match(toupper(cluster), pal_hex)
    return(cl_mat == as.integer(cluster))
  }

  ## return == "raster"  (base-R lightweight object) -----------------------------
  rast <- list(
    data   = cl_mat,                           # integer matrix
    nrow   = h,
    ncol   = w,
    extent = c(xmin = 0, xmax = w, ymin = 0, ymax = h),
    crs    = NA_character_
  )
  class(rast) <- "segmentR_raster"
  rast
}

################################################################################
## Helper methods for the lightweight raster ----------------------------------
################################################################################

#' @export
print.segmentR_raster <- function(x, ...) {
  cat("segmentR_raster\n",
      " rows   :", x$nrow,
      "\n  cols   :", x$ncol,
      "\n  extent :", paste(x$extent, collapse = " "),
      "\n  CRS    :", ifelse(is.na(x$crs), "NA", x$crs),
      "\n")
}

#' @export
plot.segmentR_raster <- function(x,
                                 col = hcl.colors(max(x$data)),
                                 axes = FALSE, ...) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mar = c(4, 4, 1, 1))
  # flip y-axis for conventional raster orientation
  img_mat <- t(apply(x$data, 2, rev))
  image(img_mat, col = col, axes = axes, asp = 1, ...)
  box()
}
