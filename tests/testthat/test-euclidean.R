test_that("Euclidean algorithm works correctly", {
  # Test simple GCDs
  expect_equal(euclidean(123612, 13892347912), 4)   # Known GCD
  expect_equal(euclidean(100, 1000), 100)           # Known GCD
  expect_equal(euclidean(48, 18), 6)                # Known GCD

  # Test when both inputs are the same
  expect_equal(euclidean(5, 5), 5)

  # Test edge cases
  expect_equal(euclidean(0, 0), 0)    # GCD of 0 and 0 is 0
  expect_error(euclidean("a", "b"))   # Error for non-numeric inputs
})
