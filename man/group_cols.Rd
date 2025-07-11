% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_cols.R
\name{group_cols}
\alias{group_cols}
\title{group_cols}
\usage{
group_cols(hex_colors, n_clusters = 5)
}
\arguments{
\item{hex_colors}{A character vector of hex colour values (e.g., \code{"#FF0000"}).}

\item{n_clusters}{Integer. Number of clusters to form (default = 5).}
}
\value{
A data frame with:
\itemize{
\item \code{hex_color}: the original hex code
\item \code{group}: the assigned cluster label (integer)
}
}
\description{
Converts hex colours to RGB and applies k-means clustering to group visually similar
colours. Returns a data frame mapping each colour to a cluster label.
}
\details{
Cluster a set of hex colours into perceptually similar groups using k-means in RGB space.
}
\examples{
hex_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF")
group_cols(hex_colors, n_clusters = 2)

}
\seealso{
\code{\link[=avg_hex]{avg_hex()}}, \code{\link[=get_top_col]{get_top_col()}}
}
