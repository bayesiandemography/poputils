
#' Calculate life expectancy from a matrix of 
#' mortality rates
#'
#' Calculate life expectancy.
#'
#' \describe{
#'   \item{\code{"mid"}}{ On average, people die half way
#'         through each age interval; equivalently, the
#'         life table function \code{lx} is a straight
#'         line within each age interval. }
#'   \item{\code{"CD-Female"}, \code{"CD-Male"}}{ As for
#'         \code{"mid"}, except that the average age at
#'         which infants die (and, if \code{age} is
#'         \code{"lt"}, the average age at which
#'         children aged 1-4 die), is determined by
#'         formulas developed by Coale and Demeny, and
#'         reported in Preston et al (2001). }
#'   \item{\code{"HMD-Female"}, \code{"HMD-Male"}}{ The
#'         approach used in the Human Mortality Database.
#'         As for \code{"mid"}, except that the average
#'         age at which infants die is determined by
#'         formulas developed by , and reported in Preston
#'         et al (2001). }
#'   \item{\code{"const"}}{ Mortality rates are constant
#'         within each age interval; equivalently, the
#'         life table function \code{lx} is an exponential
#'         curve within each age interval. }
#' }
#' 
#' 
#' |                      | \code{"lt"} | \code{"single"} | \code{"five"} |
#' |:---------------------|-------------|-----------------|---------------|
#' | \code{"mid"}         | X           | X               | X             |
#' | \code{"CD-Female"}   | X           | X               |               |
#' | \code{"CD-Male"}     | X           | X               |               |
#' | \code{"HMD-Female"}  |             | X               |               |
#' | \code{"HMD-Male"}    |             | X               |               |
#' | \code{"const"}       | X           | X               | X             |
#'
#'
#' The last age group is always the open age group.
#'
#' If \code{age} is \code{"lt"}, then \code{mx} must have
#' at least three columns (ie at least three age groups).
#'
#' @param mx Mortality rates. A matrix of non-negative values.
#' Must have at least one column.
#' @param age Type of age groups used. Choices are
#' \code{"lt"}, \code{"single"}, \code{"five"}.
#' @param method Name of method for converting mortality
#' rates to probabilies of dying. Choices are \code{"mid"},
#' \code{"CD-Female"}, \code{"CD-Male"},
#' \code{"HMD-Female"}, \code{"HMD-Male"},
#' and \code{"const"}.
#'
#' @return A numeric vector with length \code{nrow(mx)}.
#'
#' @examples
#' mx <- matrix(c(0.010, 0.002, 0.070, 0.200,
#'                0.011, 0.003, 0.072, 0.210),
#'              nrow = 2)
#'
#' lifeexp(mx, age = "lt", method = "mid")
#' lifeexp(mx, age = "lt", method = "CD-Female")
#' lifeexp(mx, age = "single", method = "CD-Female")
#' lifeexp(mx, age = "single", method = "const")
#' @md
#' @export
lifeexp <- function(mx,
                    age,
                    method) {
    choices_age <- c("lt",
                     "single",
                     "five")
    choices_method <- c("mid",
                        "CD-Female", "CD-Male",
                        "HMD-Female", "HMD-Male",
                        "const")
    checkmate::assert_choice(age, choices = choices_age)
    checkmate::assert_choice(method, choices = choices_method)
    checkmate::assert_matrix(mx, min.cols = 1L)
    checkmate::assert_numeric(mx)
    if (method == "lt") {
        if (ncol(mx) == 1L)
            stop("'age' is \"lt\" but 'mx' only has one column (ie only one age group)",
                 call. = FALSE)
        if (ncol(mx) == 2L)
            stop("'age' is \"lt\" but 'mx' only has two columns (ie only two age groups)",
                 call. = FALSE)
    }
    if (nrow(mx) == 0L)
        return(numeric())
    index_method <- match(method, choices_method)
    if (age == "lt") {
        if (method %in% c("mid", "CD-Female", "CD-Male"))
            lifeexp_ax_lt(mx = mx, index_method = index_method)
        else if (method == "const")
            lifeexp_const_lt(mx = mx)
        else
            stop("'age' is \"lt\" but method is \"", method, "\"", call. = FALSE)
    }
    else if (age == "single") {
        if (method == "const")
            lifeexp_const_single(mx)
        else
            lifeexp_ax_single(mx = mx, index_method = index_method)
    }
    else if (age == "five") {
        if (method == "mid")
            lifeexp_ax_five(mx = mx)
        else if (method == "const")
            lifeexp_const_five(mx = mx)
        else
            stop("'age' is \"five\" but method is \"", method, "\"", call. = FALSE)
    }
    else
        stop("unexpected value for 'age' : ", age, "\"", call. = FALSE)
}
        


        
