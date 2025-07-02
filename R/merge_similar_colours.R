#' merge_similar_colours
#'
#' Collapse perceptually similar colours in an extracted palette.
#'
#' @description
#' Accepts either
#' * a **data frame** with columns
#'   – `hex`: valid hexadecimal colour strings
#'   – `freq`: integer pixel counts or weights, **or**
#' * a **character vector** of hex strings (duplicates allowed).
#' This function computes the pair-wise perceptual distances
#' (CIEDE2000) between all colours in the specified colour space
#' (Lab by default).
#' Single-linkage hierarchical clustering is then applied and the tree is cut
#' at height `deltaE`.
#' All colours that fall within the same cluster are merged; their
#' representative colour is the mean hex value returned by
#' [`avg_hex()`], and their frequencies are summed.
#'
#' @param pal Either a data frame as above or a character/factor vector of
#'   hex colours.
#' @param deltaE Numeric ≥ 0. The maximum CIEDE2000 distance for two colours
#'   to be considered the same. Values between 2 and 6 are common; default 5.
#' @param space Character. Colour space supplied to **farver** for the
#'   distance calculation (default `"lab"`).  Any space recognised by
#'   `farver::decode_colour()` is valid.
#'
#' @return A data frame with columns
#'   *`group`* – cluster label
#'   *`avg_color`* – representative hex colour per group
#'   *`freq`* – total pixel count per group,
#'   sorted by descending `freq`.
#'
#' @details
#' *Perceptual* merging avoids the arbitrary choice of *k* required by
#' k-means approaches and is deterministic (no random seeds).
#'
#' @seealso
#' [`img_to_palette()`], [`get_top_col()`], [`avg_hex()`]
#'
#' @examples
#' \dontrun{
#' img  <- segmentR::read_image_from_url("https://upload.wikimedia.org/wikipedia/en/0/02/Homer_Simpson_2006.png")
#' pal  <- segmentR::img_to_palette(img, n = 20)          # initial 20-colour palette
#' pal2 <- merge_similar_colours(pal, deltaE = 4)
#' print(pal2)
#' plot_palette(pal2$avg_color)
#' }
#'
#' @importFrom farver decode_colour compare_colour
#' @importFrom stats   hclust cutree as.dist
#' @export
merge_similar_colours <- function(pal,
                                   deltaE = 5,
                                   space  = "lab") {
  if (!requireNamespace("farver", quietly = TRUE))
    stop("Package 'farver' is required but not installed.", call. = FALSE)

  # ------------------------------------------------------------------ #
  # 0. Input normalisation: accept data frame or vector                 #
  # ------------------------------------------------------------------ #
  if (is.character(pal) || is.factor(pal)) {
    # vector → data frame with frequency counts
    pal <- aggregate(freq ~ hex,
                     data = data.frame(hex = as.character(pal),
                                       freq = 1L,
                                       stringsAsFactors = FALSE),
                     FUN = sum)
  }

  stopifnot(all(c("hex", "freq") %in% names(pal)),
            is.numeric(deltaE), length(deltaE) == 1, deltaE >= 0)

  # ------------------------------------------------------------------ #
  # 1. Hex → numeric coordinates in chosen space                       #
  # ------------------------------------------------------------------ #
  cols_num <- farver::decode_colour(pal$hex, to = space)

  # ------------------------------------------------------------------ #
  # 2. Full pair-wise ΔE matrix                                        #
  # ------------------------------------------------------------------ #
  dmat <- farver::compare_colour(
    cols_num, cols_num,
    from_space = space,
    to_space   = space,
    method     = "CIE2000"
  )

  # ------------------------------------------------------------------ #
  # 3. Single-link clustering, cut at height = deltaE                  #
  # ------------------------------------------------------------------ #
  hc           <- stats::hclust(as.dist(dmat), method = "single")
  pal$group    <- stats::cutree(hc, h = deltaE)

  # ------------------------------------------------------------------ #
  # 4. Aggregate with existing helper                                  #
  # ------------------------------------------------------------------ #
  merged <- avg_hex(pal, hex_col = "hex", group_col = "group")
  merged[order(merged$freq, decreasing = TRUE), ]
}
