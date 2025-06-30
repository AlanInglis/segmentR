#' Find Optimal Number of Clusters
#'
#' @description
#' This function finds the optimal number of clusters for segmenting an
#' image using either the elbow method or the silhouette method.
#'
#' @details
#' The function converts an image to a matrix of RGB values and calculates the
#' total within-cluster sum of squares (WSS) for different numbers of clusters.
#' Depending on the chosen method ("elbow" or "silhouette"), it determines the
#' optimal number of clusters. The elbow method identifies the point where the
#' WSS starts to decrease less sharply, while the silhouette method calculates
#' average silhouette scores for different cluster numbers.
#' @param img An image object
#' @param max_clusters Maximum number of clusters to consider
#' @param method Method to use: "elbow" or "silhouette"
#' @return A list containing the optimal number of clusters and a plot
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


#' Find Optimal Number of Clusters
#'
#' @param img An image object
#' @param max_clusters Maximum number of clusters to consider
#' @return A list containing the optimal number of clusters and a plot
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

