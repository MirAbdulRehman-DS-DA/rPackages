test_that("Dijkstra's algorithm works correctly", {
  # Sample graph to test
  wiki_graph <- data.frame(
    v1 = c(1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 6, 6),
    v2 = c(2, 3, 6, 3, 4, 4, 6, 5, 6, 6, 5, 4),
    w = c(7, 9, 14, 10, 15, 11, 2, 6, 9, 10, 9, 2)
  )

  # Test from node 1
  result <- dijkstra(wiki_graph, 1)
  expect_equal(result[2], 7)    # Distance to node 2 should be 7
  expect_equal(result[3], 9)    # Distance to node 3 should be 9
  expect_equal(result[6], 11)   # Distance to node 6 should be 11

  # Test from node 6
  result <- dijkstra(wiki_graph, 6)
  expect_equal(result[4], 2)    # Distance to node 4 should be 2
  expect_equal(result[5], 8)    # Distance to node 5 should be 8 (updated from 9)
})
