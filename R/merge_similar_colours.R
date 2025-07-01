#' merge_similar_colours
#'
#' Collapse perceptually similar colours in an extracted palette.
#'
#' @description
#' Given a data frame of hex colours and their pixel **frequency** counts
#' (such as the output of [`img_to_palette()`] or [`get_top_col()`]),
#' this function computes the pair-wise perceptual distances
#' (CIEDE2000) between all colours in the specified colour space
#' (Lab by default).
#' Single-linkage hierarchical clustering is then applied and the tree is cut
#' at height `deltaE`.
#' All colours that fall within the same cluster are merged; their
#' representative colour is the mean hex value returned by
#' [`avg_hex()`], and their frequencies are summed.
#'
#' @param pal_df A data frame with **at least** two columns
#'   *`hex`* – valid hexadecimal colour strings
#'   *`freq`* – integer pixel counts or weights.
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
#' img  <- read_image("path/to/image.png")
#' pal  <- img_to_palette(img, k = 20)          # initial 20-colour palette
#' pal2 <- merge_similar_colours(pal, deltaE = 4)
#' print(pal2)
#' plot_palette(pal2$avg_color, pal2$freq)      # quick visual check
#' }
#'
#' @importFrom farver decode_colour compare_colour
#' @importFrom stats   hclust cutree as.dist
#' @export
merge_similar_colours <- function(pal_df,
                                  deltaE = 5,
                                  space  = "lab") {

  if (!requireNamespace("farver", quietly = TRUE))
    stop("Package 'farver' is required but not installed.", call. = FALSE)
  stopifnot(all(c("hex", "freq") %in% names(pal_df)),
            is.numeric(deltaE), length(deltaE) == 1, deltaE >= 0)

  # 1. Hex -> numeric coords in chosen space
  cols_num <- farver::decode_colour(pal_df$hex, to = space)

  # 2. Full pair-wise ΔE matrix
  dmat <- farver::compare_colour(
    cols_num, cols_num,                     # <-- both matrices
    from_space = space,
    to_space   = space,
    method     = "CIE2000")

  # 3. Single-link clustering, cut at height = deltaE
  hc               <- stats::hclust(as.dist(dmat), method = "single")
  pal_df$group     <- stats::cutree(hc, h = deltaE)

  # 4. Aggregate with existing helper
  merged <- avg_hex(pal_df, hex_col = "hex", group_col = "group")
  merged[order(merged$freq, decreasing = TRUE), ]
}
