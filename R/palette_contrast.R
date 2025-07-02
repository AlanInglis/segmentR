################################################################################
## palette_contrast.R
## ---------------------------------------------------------------------------
## WCAG 2.2 contrast ratios for a palette
## * Against fixed backgrounds (white and/or black)
## * Optional full pair-wise matrix
##
## No dependencies beyond base R.
################################################################################

#' palette_contrast
#'
#' Compute WCAG 2.2 contrast ratios for every colour in a palette.
#'
#' @details
#' The function follows the WCAG 2.2 formula:
#' * linearise sRGB → relative luminance \(L\);
#' * contrast ratio \(C = (L_bright+0.05) / (L_dark+0.05)\).
#' Values range from **1** (identical colours) to **21** (black v. white).
#'
#' Two use–cases are covered:
#'
#' 1. **Fixed background check** – choose `against = "white"`, `"black"`,
#'    or both (default).  Returns a data-frame with a row per palette entry and
#'    columns `contrast_white`, `contrast_black`, and `min_contrast`.
#' 2. **Pair-wise matrix** – set `pairwise = TRUE` to receive the full
#'    \(K * K\) symmetric matrix of contrasts between palette colours.
#'
#' @param pal       Character vector of hex colours **or** data-frame with a
#'                  `hex` column (e.g. output of `merge_similar_colours()`).
#' @param against   Background(s) to test: `"white"`, `"black"`, or both.
#' @param pairwise  Logical.  If `TRUE`, also return the full contrast matrix.
#'
#' @return If `pairwise = FALSE` (default) a data-frame with columns
#'   `hex`, `contrast_white`, `contrast_black`, `min_contrast`.
#'   If `pairwise = TRUE` a list with two elements:
#'   * `summary` – the data-frame above;
#'   * `matrix`  – numeric contrast matrix (rownames/colnames = palette hex).
#'
#' @examples
#' \dontrun{
#' pal <- c("#FFFFFF", "#FED707", "#55ADCD", "#000000")
#' palette_contrast(pal)
#'
#' ## Pair-wise view
#' pc <- palette_contrast(pal, pairwise = TRUE)
#' pc$matrix
#' }
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
