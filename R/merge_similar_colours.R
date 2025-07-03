#' merge_similar_colours
#'
#' Collapse perceptually similar colours in a palette using CIEDE2000 distance.
#'
#' @description
#' Groups colours that are perceptually close in a specified colour space and merges them.
#' Accepts either:
#' - a **data frame** with columns:
#'   - `hex`: hexadecimal colour codes
#'   - `freq`: pixel counts or weights
#' - or a **character vector** of hex codes (duplicates allowed).
#'
#' Colours are clustered using single-linkage hierarchical clustering with distances
#' computed in the chosen space (default: Lab). Clusters are merged by computing
#' a weighted average colour using [`avg_hex()`].
#'
#' @param pal A data frame or character vector of hex colours. If a data frame, it must contain
#'   `hex` and `freq` columns.
#' @param deltaE Numeric ≥ 0. The maximum perceptual distance (ΔE2000) allowed within a cluster.
#'   Typical values are between 2 and 6. Default is 5.
#' @param space Character. Colour space used for distance calculation (default: `"lab"`).
#'   Passed to `farver::decode_colour()`.
#'
#' @return A data frame with:
#'   * `group`: cluster label
#'   * `avg_color`: mean colour per group (hex)
#'   * `freq`: total pixel count per group
#'   Rows are sorted by descending frequency.
#'
#' @examples
#' # Basic example with a hex vector
#' hex_vec <- c("#FF0000", "#FE0000", "#00FF00", "#0000FF", "#00FE00")
#' merge_similar_colours(hex_vec, deltaE = 3)
#'
#' # Using an internal image
#' img_path <- system.file("extdata", "sample_img.png", package = "segmentR")
#' img <- read_image(img_path)
#' pal <- img_to_palette(img, n = 15, avg_cols = FALSE)
#' merged <- merge_similar_colours(pal, deltaE = 4)
#' plot_palette(merged$avg_color)
#'
#' @details
#' This perceptual merging avoids the arbitrary choice of *k* in k-means and
#' is deterministic (no random seed needed).
#'
#' @seealso [avg_hex()], [img_to_palette()], [get_top_col()]
#'
#' @importFrom farver decode_colour compare_colour
#' @importFrom stats hclust cutree as.dist
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
