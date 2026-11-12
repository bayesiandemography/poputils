test_that(".make_seeds() gives one named seed per name", {
  seeds <- .make_seeds(c("a", "b", "c"))
  expect_identical(names(seeds), c("a", "b", "c"))
  expect_true(is.integer(seeds))
  expect_identical(length(.make_seeds(character())), 0L)
  set.seed(0)
  s1 <- .make_seeds("a")
  set.seed(0)
  s2 <- .make_seeds("a")
  expect_identical(s1, s2)
})

test_that(".make_seeds() checks names", {
  expect_error(.make_seeds(1), "character vector")
  expect_error(.make_seeds(NA_character_), "character vector")
})

test_that(".with_seed() gives the same draws with the same seed", {
  expect_identical(.with_seed(10, stats::rnorm(3)), .with_seed(10, stats::rnorm(3)))
  expect_false(identical(.with_seed(10, stats::rnorm(3)), .with_seed(11, stats::rnorm(3))))
})

test_that(".with_seed() leaves the caller's random numbers unaffected", {
  set.seed(1)
  expected <- stats::runif(2)
  set.seed(1)
  ignore <- .with_seed(99, stats::rnorm(5))
  expect_identical(stats::runif(2), expected)
})

test_that(".with_seed() restores the state after an error", {
  set.seed(1)
  expected <- stats::runif(2)
  set.seed(1)
  expect_error(.with_seed(99, {stats::rnorm(5); stop("oops")}), "oops")
  expect_identical(stats::runif(2), expected)
})

test_that(".with_seed() leaves no random number state if there was none", {
  env <- globalenv()
  has_old <- exists(".Random.seed", envir = env, inherits = FALSE)
  if (has_old) {
    old <- get(".Random.seed", envir = env, inherits = FALSE)
    on.exit(assign(".Random.seed", old, envir = env))
  }
  if (has_old)
    rm(".Random.seed", envir = env)
  ignore <- .with_seed(5, stats::rnorm(1))
  expect_false(exists(".Random.seed", envir = env, inherits = FALSE))
})

test_that(".with_seed() checks seed", {
  expect_error(.with_seed(1.5, 1), "whole number")
  expect_error(.with_seed(c(1, 2), 1), "whole number")
  expect_error(.with_seed(NA, 1), "whole number")
})
