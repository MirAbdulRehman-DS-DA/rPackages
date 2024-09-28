#' Dijkstra's Algorithm
#'
#' This function finds the shortest path from a starting node to all other nodes
#' in a graph using Dijkstra's algorithm.
#'
#' @param graph A data frame representing the graph with columns: v1 (from node), v2 (to node), w (weight)
#' @param init_node The starting node
#' @return A vector of shortest distances from the starting node to all other nodes
#' @examples
#' wiki_graph <- data.frame(v1 = c(1, 1, 1), v2 = c(2, 3, 6), w = c(7, 9, 14))
#' dijkstra(wiki_graph, 1)
#' @export

dijkstra <- function(graph, init_node) {
  nodes <- unique(c(graph$v1, graph$v2))
  n <- length(nodes)

  # Initialize distances
  distances <- rep(Inf, n)
  distances[init_node] <- 0

  # Track visited nodes
  visited <- rep(FALSE, n)

  while (any(!visited)) {
    # Find the unvisited node with the smallest distance
    current_node <- which.min(ifelse(visited, Inf, distances))

    # If smallest distance is Inf, break the loop (no more reachable nodes)
    if (distances[current_node] == Inf) {
      break
    }

    visited[current_node] <- TRUE

    # Get neighbors of the current node
    neighbors <- graph[graph$v1 == current_node & !visited[graph$v2], ]

    for (i in seq_len(nrow(neighbors))) {
      neighbor_node <- neighbors$v2[i]

      # Correctly index neighbor_node in the distances vector
      neighbor_index <- match(neighbor_node, nodes)
      new_dist <- distances[current_node] + neighbors$w[i]

      # Debugging print statements
      print(paste("Current node:", current_node))
      print(paste("Neighbor node:", neighbor_node))
      print(paste("Current distance:", distances[current_node]))
      print(paste("New distance:", new_dist))
      print(paste("Current distance for neighbor:", distances[neighbor_index]))

      # Update distance if new_dist is smaller
      if (!is.na(new_dist) && new_dist < distances[neighbor_index]) {
        distances[neighbor_index] <- new_dist
      }
    }
  }

  return(distances)
}
