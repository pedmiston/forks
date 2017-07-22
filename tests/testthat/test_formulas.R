context("Generating formulas")

test_that("formulas can be generated from vector of covariates", {
  formulas <- expand_base_formula("y ~ x", c("a", "b"))
  expect_equal(formulas, c("y ~ x", "y ~ x + a", "y ~ x + b", "y ~ x + a + b"))
})


context("Formula manipulation")

test_that("splitting formulas is vectorized", {
  expect_equal(split_formula_args("y ~ x"), c("x"))
  expect_equal(split_formula_args(c("y ~ x", "y ~ x + a")),
               list(c("x"), c("x", "a")))
})


context("Comparing formulas")

test_that("comparing formulas returns positive differences and size", {
  compared <- compare_formulas("y ~ x", "y ~ x + a")
  
  expected <- data_frame(
    from = "y ~ x",
    to = "y ~ x + a",
    difference = "+ a",
    n_different = as.integer(1)
  )
  
  expect_equal(compared, expected)
})

test_that("comparing formulas returns negative differences and size", {
  compared <- compare_formulas("y ~ x + a", "y ~ x")
  
  expected <- data_frame(
    from = "y ~ x + a",
    to = "y ~ x",
    difference = "- a",
    n_different = as.integer(1)
  )
  
  expect_equal(compared, expected)
})

test_that("comparing formulas returns both positive and negative differences", {
  compared <- compare_formulas("y ~ x + a", "y ~ x + b")
  
  expected <- data_frame(
    from = "y ~ x + a",
    to = "y ~ x + b",
    difference = "- a + b",
    n_different = as.integer(2)
  )
  
  expect_equal(compared, expected)
})
