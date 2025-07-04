% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_border.R
\name{draw_border}
\alias{draw_border}
\title{draw_border}
\usage{
draw_border(segmented_array, edge_color = "black")
}
\arguments{
\item{segmented_array}{A numeric RGB array (\verb{height × width × 3}) representing
a segmented image.}

\item{edge_color}{Colour name or hex code for the borders (default: \code{"black"}).}
}
\value{
A \code{magick-image} object with segment borders highlighted.
}
\description{
Detects edges between segments in an RGB array and overlays a border using
a specified colour. Returns a \code{magick-image} object with borders drawn.
}
\details{
Add visible borders between colour segments in a segmented image.
}
\examples{
# Load and segment example image
img_path <- system.file("extdata", "sample_img.png", package = "segmentR")
img <- read_image(img_path, width = 341, height = 512)
seg <- segment_image(img, n = 6)

# Add border
bordered <- draw_border(seg, edge_color = "red")
bordered

}
