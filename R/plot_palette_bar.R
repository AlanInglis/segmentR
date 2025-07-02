#' plot_palette_bar
#'
#' Quick palette visualisation in two styles:
#' * **Swatch strip** (`type = "swatch"`) – equal-width colour tiles.
#'   `counts` only affects whether labels show percentages.
#' * **Bar plot** (`type = "bar"`) – bars whose height equals the pixel
#'   count (`counts = TRUE`) or a constant (`counts = FALSE`).
#'
#' @param pal   Hex vector or data-frame with `hex` and optional `freq`.
#' @param counts Logical.  For *bar* controls bar height; for *swatch*
#'               controls whether percentages appear in the labels.
#' @param type  `"swatch"` (default) or `"bar"`.
#' @param orientation `"horizontal"` (default) or `"vertical"`.
#' @param show_labels Logical; draw text labels?  Default `TRUE`.
#'
#' @return ggplot object (printed invisibly).
#'
#' @examples
#' \dontrun{
#' pal <- c("#FFFFFF", rep("#FED707", 3), "#55ADCD", "#000000")
#' plot_palette_bar(pal)                          # swatch strip
#' plot_palette_bar(pal, type = "bar")            # frequency bars
#' }
#'
#' @import ggplot2
#' @export
plot_palette_bar <- function(pal,
                             counts      = TRUE,
                             type        = c("swatch", "bar"),
                             orientation = c("horizontal", "vertical"),
                             show_labels = TRUE) {

  type        <- match.arg(type)
  orientation <- match.arg(orientation)

  ## ---- tidy palette --------------------------------------------------------
  if (is.data.frame(pal)) {
    stopifnot("hex" %in% names(pal))
    df <- pal[, c("hex", if ("freq" %in% names(pal)) "freq" else NULL)]
  } else {
    df <- data.frame(hex = toupper(pal), stringsAsFactors = FALSE)
  }
  if (!"freq" %in% names(df)) df$freq <- 1

  df <- aggregate(freq ~ hex, df, sum)                 # collapse dups
  df$hex <- toupper(df$hex)
  df$pct <- df$freq / sum(df$freq) * 100
  df <- df[order(-df$freq), ]
  df$pos <- seq_len(nrow(df))

  ## ---- plot object ---------------------------------------------------------
  if (type == "bar") {

    # order bars from most- to least-frequent
    df <- df[order(-df$freq), ]
    df$hex_f <- factor(df$hex, levels = rev(df$hex))  # keeps order after flip

    gg <- ggplot2::ggplot(df,
                          ggplot2::aes(x = hex_f,
                                       y = if (counts) freq else 1,
                                       fill = hex)) +
      ggplot2::geom_bar(stat = "identity", width = 0.8) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_identity() +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::labs(x = "Colour", y = if (counts) "Count" else "Colour") +
      ggplot2::theme(axis.text.x  = ggplot2::element_text(angle = 45,
                                                          hjust = 1),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor   = ggplot2::element_blank(),
                     legend.position    = "none")

    if (orientation == "vertical")
      gg <- ggplot2::ggplot(df,
                                    ggplot2::aes(x = hex_f,
                                                 y = if (counts) freq else 1,
                                                 fill = hex)) +
      ggplot2::geom_bar(stat = "identity", width = 0.8) +
      ggplot2::scale_fill_identity() +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::labs(x = "Colour", y = if (counts) "Count" else "Colour") +
      ggplot2::theme(axis.text.x  = ggplot2::element_text(angle = 45,
                                                          hjust = 1),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor   = ggplot2::element_blank(),
                     legend.position    = "none")

    if (show_labels) {
      lab <- if (counts)
        sprintf("%.0f", df$freq) else ''
      gg <- gg + ggplot2::geom_text(
        ggplot2::aes(label = lab),
        hjust = -0.1, vjust = 0.5, size = 3)
    }

    print(gg)
    return(invisible(gg))
  }

  ## ---- swatch strip (equal segments) ---------------------------------------
  gg <- ggplot2::ggplot(df,
                        ggplot2::aes(x = pos, y = 1, fill = hex)) +
    ggplot2::geom_col(width = 1, colour = "grey40") +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void()

  if (orientation == "vertical")
    gg <- gg + ggplot2::coord_flip()

  if (show_labels) {
    lab <- if (counts)
      sprintf("%s\n%.1f%%", df$hex, df$pct) else df$hex

    gg <- gg + ggplot2::geom_text(
      ggplot2::aes(label = lab),
      y = 0.5,
      angle = if (orientation == "horizontal") 90 else 0,
      size = 3,
      fontface = "bold",
      vjust = 0.5, hjust = 0.5)
  }

  print(gg)
  invisible(gg)
}
