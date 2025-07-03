#' classify_pixels_by_cluster
#'
#' Assign each pixel in an image to the closest reference colour using
#' CIEDE2000 distance in Lab colour space.
#'
#' @description
#' This function assigns pixels to colour clusters based on perceptual distance
#' from a reference palette. It supports multiple output types: a factor matrix
#' of hex labels, a binary mask for a single cluster, or a custom lightweight
#' raster object.
#'
#' @section Return types:
#' * **"factor"** (default) – a `height × width` matrix of factor levels, one per hex code
#' * **"mask"** – a logical matrix identifying pixels in a specified cluster
#' * **"raster"** – a `segmentR_raster` S3 object (integer matrix + metadata) with custom print/plot methods
#'
#' @param img A `magick-image` object, image path/URL, or an RGB array (`height × width × 3`)
#' @param palette Character vector of hex codes or a data frame with a `hex` column
#' @param space Character. Colour space to use for comparison (default: `"lab"`)
#' @param output Type of output: `"factor"`, `"mask"`, or `"raster"` (default: `"factor"`)
#' @param cluster When `output = "mask"`, the cluster to extract (hex code or index)
#'
#' @return
#' Depends on `output`:
#' * `"factor"` – matrix of factor hex codes
#' * `"mask"` – logical matrix
#' * `"raster"` – object of class `segmentR_raster`
#'
#' @examples
#' # Load example image from package
#' img_path <- system.file("extdata", "sample_img.png", package = "segmentR")
#' img <- read_image(img_path, width = 341, height = 512)
#'
#' # Extract colour palette
#' pal <- img_to_palette(img, n = 20, exclude = FALSE)
#'
#' # Factor matrix of cluster assignments
#' cl_mat <- classify_pixels_by_cluster(img, palette = pal)
#' table(cl_mat)
#' plot_palette(unique(as.vector(cl_mat)))
#'
#' # Logical mask for the most dominant colour
#' mask <- classify_pixels_by_cluster(img,
#'                                    palette = pal,
#'                                    output = "mask",
#'                                    cluster = pal[1])
#' image(t(apply(mask, 2, rev)),  # rotate for display
#'       col = c("black", "white"),
#'       axes = FALSE,
#'       asp = 1)
#'
#' # Lightweight raster with metadata
#' rast <- classify_pixels_by_cluster(img, palette = pal, output = "raster")
#' print(rast)
#' plot(rast, col = pal)
#'
#' @importFrom magick image_read image_data
#' @importFrom farver decode_colour convert_colour compare_colour
#' @export
classify_pixels_by_cluster <- function(img,
                                       palette,
                                       space   = "lab",
                                       output  = c("factor", "mask", "raster"),
                                       cluster = NULL) {

  output <- match.arg(output)

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
  if (output == "factor") {
    fac <- factor(cl_mat, levels = seq_along(pal_hex), labels = pal_hex)
    dim(fac) <- c(h, w)                                            # keep matrix shape
    return(fac)
  }

  if (output == "mask") {
    if (is.null(cluster))
      stop("Supply `cluster` when output = 'mask'.", call. = FALSE)
    if (is.character(cluster))
      cluster <- match(toupper(cluster), pal_hex)
    return(cl_mat == as.integer(cluster))
  }

  ## output == "raster"  (base-R lightweight object) -----------------------------
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
