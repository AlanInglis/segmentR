#' white_balance_auto
#'
#' Automatic grey-world or percentile white-balance correction.
#'
#' @param img      magick image object *or* path/URL.
#' @param method   "greyworld" (default) or "percentile".
#' @param lower,upper  Quantiles for percentile stretch.
#' @return         magick image object.
#' @export
white_balance_auto <- function(img,
                               method = c("greyworld", "percentile"),
                               lower  = 0.01,
                               upper  = 0.99) {


  # Ensure magick is loaded
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("The 'magick' package is required.")
  }

  method <- match.arg(method)

  if (is.character(img)) img <- magick::image_read(img)

  ## 3 × H × W array of two-digit hex
  raw_hex <- magick::image_data(img, channels = "rgb")
  arr     <- array(strtoi(raw_hex, 16L), dim = dim(raw_hex))   # integer

  clip255 <- function(x) {
    x <- round(x)
    x[x < 0] <- 0
    x[x > 255] <- 255
    as.integer(x)
  }

  if (method == "greyworld") {
    means      <- vapply(1:3, function(ch) mean(arr[ch, , ]), numeric(1))
    scalers    <- mean(means) / means
    for (ch in 1:3)
      arr[ch, , ] <- clip255(arr[ch, , ] * scalers[ch])
  } else { # percentile stretch
    for (ch in 1:3) {
      v  <- as.vector(arr[ch, , ])
      lo <- as.numeric(quantile(v, lower,  names = FALSE))
      hi <- as.numeric(quantile(v, upper,  names = FALSE))
      if (hi == lo) hi <- lo + 1
      arr[ch, , ] <- clip255((arr[ch, , ] - lo) / (hi - lo) * 255)
    }
  }

  ## reshape to H × W × 3 (base R's aperm)
  arr_out <- aperm(arr, c(2, 3, 1))            # height × width × channels
  dimnames(arr_out) <- NULL

  # Write to a temporary PNG file using base R's writeBin and magick's image_write
  # Make a magick image from the integer array
  # The only way with base R + magick: convert arr_out to magick image via a temporary PNG

  # 1. Create a magick image from the array for writing
  #    - arr_out: (height, width, 3), values 0-255
  #    - convert to magick image using magick::image_read()
  #    - But magick::image_read() does not accept arrays, so write to a temp PNG via magick::image_write

  # Convert arr_out to a raster object (base R)
  height <- dim(arr_out)[1]
  width  <- dim(arr_out)[2]
  m <- matrix(rgb(
    arr_out[,,1]/255,
    arr_out[,,2]/255,
    arr_out[,,3]/255
  ), nrow = height, ncol = width)

  ras <- as.raster(t(m)[, height:1])

  # flip to match original image orientation
  ras <- ras[,c(ncol(ras):1),drop = FALSE]

  # Convert raster -> magick image
  img_tmp <- magick::image_read(ras)

  # Done!
  img_tmp
}
