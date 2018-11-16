context("Helper Functions")
library(shinyMisc)

test_df <- data.frame(
  a = letters[1:10],
  b = factor(letters[11:20]),
  c = 1:10,
  d = runif(10),
  e = rep(c(TRUE, FALSE), 5),
  f = LETTERS[1:10]
)
test_df$f <- as.character(test_df$f)

test_that("factors_in_data returns expected results", {
  expect_that(factors_in_data(test_df), equals(c('a', 'b')))
  expect_that(factors_in_data(test_df[, letters[3:6] ]), equals(NULL))
})

test_that("continuous_variables_in_data returns expected results", {
  expect_that(continuous_variables_in_data(test_df), equals(c('c', 'd')))
  expect_that(continuous_variables_in_data(test_df[, c('a', 'b', 'e', 'f') ]), equals(NULL))
})
