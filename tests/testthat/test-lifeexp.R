
test_that("'lifeexp' gives answer with valid combination of inputs, and throws error with invalid", {
    ## tests of whether the answer given is the right answer are found
    ## in the tests for the helper functions
    mx <- matrix(c(0.02, 0.01, 0.5), nrow = 1)
    expect_true(lifeexp(mx, age = "lt", method = "mid") > 0)
    expect_true(lifeexp(mx, age = "lt", method = "CD-Female") > 0)
    expect_true(lifeexp(mx, age = "lt", method = "CD-Male") > 0)
    expect_error(lifeexp(mx, age = "lt", method = "HMD-Female"),
                 "'age' is \"lt\" but method is \"HMD-Female")
    expect_error(lifeexp(mx, age = "lt", method = "HMD-Male"),
                 "'age' is \"lt\" but method is \"HMD-Male")
    expect_true(lifeexp(mx, age = "lt", method = "const") > 0)
    expect_true(lifeexp(mx, age = "single", method = "mid") > 0)
    expect_true(lifeexp(mx, age = "single", method = "CD-Female") > 0)
    expect_true(lifeexp(mx, age = "single", method = "CD-Male") > 0)
    expect_true(lifeexp(mx, age = "single", method = "HMD-Female") > 0)
    expect_true(lifeexp(mx, age = "single", method = "HMD-Male") > 0)
    expect_true(lifeexp(mx, age = "single", method = "const") > 0)
    expect_true(lifeexp(mx, age = "five", method = "mid") > 0)
    expect_error(lifeexp(mx, age = "five", method = "CD-Female"),
                 "'age' is \"five\" but method is \"CD-Female")
    expect_error(lifeexp(mx, age = "five", method = "CD-Male"),
                 "'age' is \"five\" but method is \"CD-Male")
    expect_error(lifeexp(mx, age = "five", method = "HMD-Female"),
                 "'age' is \"five\" but method is \"HMD-Female")
    expect_error(lifeexp(mx, age = "five", method = "HMD-Male"),
                 "'age' is \"five\" but method is \"HMD-Male")
    expect_true(lifeexp(mx, age = "five", method = "const") > 0)
})

test_that("'lifeexp' gives correct error messages when age is 'lt' and ncol < 3", {
    mx <- matrix(numeric(), nrow = 1)
    expect_error(lifeexp(mx = mx, age = "lt", method = "mid"))
})
                 
