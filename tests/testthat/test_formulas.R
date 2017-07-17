context("Generating formulas")

test_that("basic formulas can be generated", {
  formulas <- generate_formulas("y ~ x", c("a", "b"))
  expect_equal(formulas, c("y ~ x", "y ~ x + a", "y ~ x + b", "y ~ x + a + b"))
})