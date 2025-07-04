#' plot_contrast_heatmap
#'
#' Display, for each palette colour, its contrast ratio against white and
#' black backgrounds, together with a swatch of the colour itself.
#'
#' @description
#' The function accepts a data frame returned by
#' [`palette_contrast()`] and produces a heat-map that helps assess which
#' colours meet WCAG recommendations on light or dark layouts.
#' A left-hand swatch column shows the actual colour, while the adjoining
#' tiles give numeric contrast ratios on white and black panels.
#'
#' @param contrast A data frame containing the columns
#'   \code{hex}, \code{contrast_white}, and \code{contrast_black} – typically
#'   the output of \code{palette_contrast()}.
#'
#' @return A \pkg{ggplot2} object. Print it to display or add layers with the
#'   usual \code{+} syntax.
#'
#' @examples
#' img_path <- system.file("extdata", "sample_img2.png", package = "segmentR")
#' img <- read_image(img_path, width = 341, height = 512)
#'
#' ## extract a palette, then compute contrast ratios
#' set.seed(123)
#' pal_raw <- get_top_col(img, n = 40, avg_cols = FALSE)
#' pal_raw <- get_top_col(
#'   img, n = 10, avg_cols = FALSE,
#'   custom_exclude = pal_raw$hex[1:40]
#' )
#' contrast <- palette_contrast(pal_raw$hex)
#'
#' ## plot the heat-map
#' plot_contrast_heatmap(contrast)
#'
#' @import ggplot2
#' @export

plot_contrast_heatmap <- function(contrast) {
  stopifnot(all(c("hex", "contrast_white", "contrast_black") %in% names(contrast)))

  ## ---- tidy --------------------------------------------------------------
  tmp <- contrast[, c("hex", "contrast_white", "contrast_black")]

  contrast_long <- rbind(
    data.frame(hex = tmp$hex,
               background = "White",
               ratio = tmp$contrast_white,
               stringsAsFactors = FALSE),
    data.frame(hex = tmp$hex,
               background = "Black",
               ratio = tmp$contrast_black,
               stringsAsFactors = FALSE)
  )

  ## ---- order rows --------------------------------------------------------
  order_hex <- with(tmp, hex[order(pmin(contrast_white, contrast_black))])
  contrast_long$hex <- factor(contrast_long$hex, levels = rev(order_hex))

  ## ---- colour-swatch column ---------------------------------------------
  swatch_df <- data.frame(
    hex        = factor(order_hex, levels = rev(order_hex)),
    background = factor("Colour", levels = c("Colour", "White", "Black")),
    ratio      = NA_real_
  )

  contrast_long$background <- factor(
    contrast_long$background,
    levels = c("Colour", "White", "Black")
  )

  ## ---- label colour ------------------------------------------------------
  rng  <- range(contrast_long$ratio, na.rm = TRUE)
  mid  <- mean(rng)
  contrast_long$label_col <- ifelse(contrast_long$ratio > mid, "white", "black")

  ## ---- plot --------------------------------------------------------------
  ggplot() +
    # heat-map tiles (white / black columns)
    geom_tile(
      data   = contrast_long,
      aes(x = background, y = hex, fill = ratio),
      colour = "grey80"
    ) +
    # numeric labels with adaptive colour
    geom_text(
      data  = contrast_long,
      aes(x = background, y = hex,
          label = sprintf("%.1f", ratio),
          colour = label_col),
      size = 3, show.legend = FALSE
    ) +
    # solid colour swatches (leftmost column)
    geom_tile(
      data  = swatch_df,
      aes(x = background, y = hex),
      fill  = as.character(swatch_df$hex),
      colour = NA
    ) +
    scale_fill_gradient(
      low = "white", high = "black",
      name = "Contrast", na.value = NA
    ) +
    scale_colour_identity() +
    labs(
      x = "Background",
      y = "Palette colour",
      title = "Contrast ratios against white and black"
    ) +
    theme_minimal() +
    theme(
      panel.grid  = element_blank(),
      axis.text.x = element_text(face = "bold")
    )
}
