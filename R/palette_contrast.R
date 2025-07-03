#' palette_contrast
#'
#' Compute WCAG 2.2 contrast ratios for colours in a palette.
#'
#' @description
#' Calculates contrast ratios between each colour and a background (white, black, or both)
#' according to the WCAG 2.2 formula:
#' \deqn{C = (L_{bright} + 0.05) / (L_{dark} + 0.05)}
#' where \eqn{L} is the relative luminance computed from linearised sRGB values.
#' Optionally, the function can return the full pairwise contrast matrix.
#'
#' @param pal A character vector of hex colour codes or a data frame with a `hex` column.
#' @param against Background colour(s) to test: `"white"`, `"black"`, or both (default).
#' @param pairwise Logical. If `TRUE`, also return a symmetric matrix of contrast ratios
#'   between every pair of colours in the palette.
#'
#' @return
#' If `pairwise = FALSE` (default), returns a data frame with:
#' - `hex`: the input colour
#' - `contrast_white`: contrast vs. white (if requested)
#' - `contrast_black`: contrast vs. black (if requested)
#' - `min_contrast`: the lower of the two
#'
#' If `pairwise = TRUE`, returns a list with:
#' - `summary`: the data frame above
#' - `matrix`: a symmetric matrix of contrast ratios between all colour pairs
#'
#' @examples
#' pal <- c("#FFFFFF", "#FED707", "#55ADCD", "#000000")
#' palette_contrast(pal)
#'
#' # Pairwise contrast matrix
#' pc <- palette_contrast(pal, pairwise = TRUE)
#' pc$matrix
#'
#' @seealso [img_to_palette()], [merge_similar_colours()]
#' @importFrom grDevices col2rgb
#' @export
palette_contrast <- function(pal,
                             against  = c("white", "black"),
                             pairwise = FALSE) {

  against <- match.arg(against, c("white", "black"), several.ok = TRUE)

  ## ---- normalise palette ------------------------------------------------------
  if (is.data.frame(pal)) {
    if (!"hex" %in% names(pal))
      stop("`pal` data-frame must contain a `hex` column.", call. = FALSE)
    pal <- pal$hex
  }
  pal <- toupper(pal)

  ## ---- helper: hex -> relative luminance --------------------------------------
  hex_to_L <- function(hex) {
    rgb <- grDevices::col2rgb(hex) / 255
    lin <- ifelse(rgb <= 0.03928,
                  rgb / 12.92,
                  ((rgb + 0.055) / 1.055) ^ 2.4)
    0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ]
  }

  L <- hex_to_L(pal)
  names(L) <- pal

  white_L <- 1
  black_L <- 0

  cr <- function(L1, L2) {
    bright <- pmax(L1, L2)
    dark   <- pmin(L1, L2)
    (bright + 0.05) / (dark + 0.05)
  }

  ## ---- contrasts vs white / black --------------------------------------------
  out <- data.frame(hex = pal, stringsAsFactors = FALSE)

  if ("white" %in% against)
    out$contrast_white <- cr(L, white_L)
  if ("black" %in% against)
    out$contrast_black <- cr(L, black_L)

  if (all(c("white", "black") %in% against))
    out$min_contrast <- pmin(out$contrast_white, out$contrast_black)

  ## ---- optional pair-wise matrix ---------------------------------------------
  if (pairwise) {
    mat <- outer(L, L, FUN = cr)
    dimnames(mat) <- list(pal, pal)
    return(list(summary = out, matrix = mat))
  }

  out
}
