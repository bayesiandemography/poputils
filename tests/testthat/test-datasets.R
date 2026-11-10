test_that("booth_standard_single sums to booth_standard", {
  expect_identical(booth_standard_single$age, as.character(10:49))
  expect_equal(sum(booth_standard_single$value), 1)
  expect_true(all(booth_standard_single$value > 0))
  value_five <- as.vector(tapply(booth_standard_single$value, rep(1:8, each = 5), sum))
  expect_equal(value_five, booth_standard$value, tolerance = 1e-12)
})
