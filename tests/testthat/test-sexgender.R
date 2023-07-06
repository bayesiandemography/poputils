
test_that("'reformat_sex' works with valid inputs - factor, length > 0", {
    x <- c("fem", "MA", "  boys  ", NA, "F")
    ans_obtained <- reformat_sex(x)
    ans_expected <- factor(c("Female", "Male", "Male", NA, "Female"),
                           levels = c("Female", "Male", NA),
                           exclude = character())
    expect_identical(ans_obtained, ans_expected)
    x <- NA
    ans_obtained <- reformat_sex(x)
    ans_expected <- factor(NA,
                           levels = c("Female", "Male", NA),
                           exclude = character())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works with valid inputs - character, length > 0", {
    x <- c("fem", "MA", "  boys  ", NA, "F")
    ans_obtained <- reformat_sex(x, factor = FALSE)
    ans_expected <- c("Female", "Male", "Male", NA, "Female")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works with valid inputs - factor, length == 0", {
    ans_obtained <- reformat_sex(character())
    ans_expected <- factor(character(), levels = c("Female", "Male"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works with valid inputs - character, length == 0", {
    ans_obtained <- reformat_sex(character(), factor = FALSE)
    ans_expected <- character()
    expect_identical(ans_obtained, ans_expected)
})


    
