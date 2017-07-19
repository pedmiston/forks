context("Generating formulas")

test_that("basic formulas can be generated", {
  formulas <- generate_formulas("y ~ x", c("a", "b"))
  expect_equal(formulas, c("y ~ x", "y ~ x + a", "y ~ x + b", "y ~ x + a + b"))
})


context("Comparing formulas")

test_that("comparing models returns difference and size", {
  compared <- compare_formulas("y ~ x", "y ~ x + a")
  
  expected <- data_frame(
    from = "y ~ x",
    to = "y ~ x + a",
    difference = "a",
    n_different = as.integer(1)
  )
  
  expect_equal(compared, expected)
})


context("Creating edges")

test_that("edges can be created from formulas", {
  formulas <- c(
    "y ~ x",
    "y ~ x + a"
  )
  
  edges <- get_deviations(formulas)
  
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
  
  edges <- get_deviations(formulas)
  
  expected <- data_frame(
    from = c("y ~ x", "y ~ x + a"),
    to = c("y ~ x + a", "y ~ x + a + b")
  )
  
  expect_equal(edges[,c("from", "to")], expected)
})