#' find_optimal_clusters
#'
#' Estimate the optimal number of colour clusters in an image using the elbow or silhouette method.
#'
#' @description
#' Converts an image to RGB space and applies k-means clustering with varying `k` values
#' to compute either the within-cluster sum of squares (elbow method) or the average silhouette score.
#' Returns the estimated optimal `k` and a diagnostic plot.
#'
#' @param img A `magick-image` object.
#' @param max_clusters Integer. The maximum number of clusters to test (default: 10).
#' @param method `"elbow"` (default) or `"silhouette"` – clustering criterion.
#'
#' @return A list with:
#'   * `optimal_clusters`: estimated best number of clusters
#'   * `plot`: a `ggplot2` object visualising the method result
#'
#' @examples
#' img_path <- system.file("extdata", "sample_img.png", package = "segmentR")
#' img <- read_image(img_path)
#'
#' res <- find_optimal_clusters(img, max_clusters = 8, method = "elbow")
#' print(res$plot)
#' res$optimal_clusters
#'
#'
#' @importFrom magick image_data
#' @importFrom cluster silhouette
#' @import ggplot2
#' @export
find_optimal_clusters <- function(img, max_clusters = 10, method = "elbow") {

  # Convert image to a matrix of RGB values
  img_array <- as.numeric(magick::image_data(img, channels = "RGB"))
  dim(img_array) <- c(prod(dim(img_array)[1:2]), 3)

  # Sample data if the image is too large
  if (nrow(img_array) > 10000) {
    set.seed(123)  # for reproducibility
    sample_indices <- sample(1:nrow(img_array), 10000)
    img_array <- img_array[sample_indices, ]
  }

  # Function to calculate total within-cluster sum of square
  wss <- function(k) {
    kmeans(img_array, k, nstart = 10, iter.max = 30)$tot.withinss
  }

  # Calculate WSS for different k
  wss_values <- sapply(1:max_clusters, wss)

  if (method == "elbow") {
    # Elbow method
    diff_wss <- diff(wss_values)
    elbow_point <- which.min(abs(diff_wss - mean(diff_wss))) + 1

    # Create elbow plot
    plot_data <- data.frame(k = 1:max_clusters, wss = wss_values)
    p <- ggplot2::ggplot(plot_data, aes(x = k, y = wss)) +
      geom_line() +
      geom_point() +
      geom_vline(xintercept = elbow_point, linetype = "dashed", color = "red") +
      labs(x = "Number of Clusters (k)", y = "Total Within-Cluster Sum of Squares",
           title = "Elbow Method for Optimal k") +
      theme_bw()

    return(list(optimal_clusters = elbow_point, plot = p))

  } else if (method == "silhouette") {
    # Silhouette method
    silhouette_scores <- numeric(max_clusters - 1)
    for (k in 2:max_clusters) {
      km <- kmeans(img_array, centers = k, nstart = 10)
      ss <- silhouette(km$cluster, dist(img_array))
      silhouette_scores[k-1] <- mean(ss[, 3])
    }
    optimal_k <- which.max(silhouette_scores) + 1

    # Create silhouette plot
    plot_data <- data.frame(k = 2:max_clusters, score = silhouette_scores)
    p <- ggplot(plot_data, aes(x = k, y = score)) +
      geom_line() +
      geom_point() +
      geom_vline(xintercept = optimal_k, linetype = "dashed", color = "red") +
      labs(x = "Number of Clusters (k)", y = "Average Silhouette Score",
           title = "Silhouette Method for Optimal k") +
      theme_bw()

    return(list(optimal_clusters = optimal_k, plot = p))
  } else {
    stop("Invalid method. Choose 'elbow' or 'silhouette'.")
  }
}


#' find_optimal_clusters_combined
#'
#' Estimate optimal clusters using both elbow and silhouette methods.
#'
#' @description
#' Calls `find_optimal_clusters()` with both `"elbow"` and `"silhouette"` methods and
#' returns their suggested cluster counts, as well as the average.
#'
#' @param img A `magick-image` object.
#' @param max_clusters Integer. The maximum number of clusters to test (default: 10).
#'
#' @return A list with:
#'   * `elbow_k`: number of clusters suggested by elbow method
#'   * `silhouette_k`: number of clusters suggested by silhouette method
#'   * `avg_k`: average of the two methods
#'
#' @examples
#' img_path <- system.file("extdata", "sample_img.png", package = "segmentR")
#' img <- read_image(img_path)
#' find_optimal_clusters_combined(img)
#'
#'
#' @export
find_optimal_clusters_combined <- function(img, max_clusters = 10) {
  elbow_result <- find_optimal_clusters(img, max_clusters, method = "elbow")
  silhouette_result <- find_optimal_clusters(img, max_clusters, method = "silhouette")

  elbow_k <- elbow_result$optimal_clusters
  silhouette_k <- silhouette_result$optimal_clusters

  # Average of the two methods
  avg_k <- round((elbow_k + silhouette_k) / 2)



  cat("Elbow method suggests:", elbow_k, "clusters\n")
  cat("Silhouette method suggests:", silhouette_k, "clusters\n")
  cat("Average:", avg_k, "clusters\n")

  return(list(elbow_k = elbow_k, silhouette_k = silhouette_k, avg_k = avg_k))
}

