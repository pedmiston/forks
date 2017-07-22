context("Creating all pairwise edges")

test_that("edges can be created from formulas", {
  formulas <- c(
    "y ~ x",
    "y ~ x + a"
  )
  
  edges <- expand_formulas(formulas)
  
  expected <- data_frame(
    from = c("y ~ x"),
    to = c("y ~ x + a")
  )
  
  expect_equal(edges[,c("from", "to")], expected)
})

test_that("only models differing by 1 variable are returned as edges", {
  formulas <- c(
    "y ~ x",
    "y ~ x + a",
    "y ~ x + a + b"
  )
  
  edges <- expand_formulas(formulas)
  
  expected <- data_frame(
    from = c("y ~ x", "y ~ x + a"),
    to = c("y ~ x + a", "y ~ x + a + b")
  )
  
  expect_equal(edges[,c("from", "to")], expected)
})

context("Generating random trees")

test_that("simple tree returns the right number of edges", {
  formulas <- generate_formulas("y ~ x", c("a", "b", "c"))
  tree_edges <- get_tree_edges(formulas)
  expect_equal(nrow(tree_edges), 7)
})
