
## ## HAS_TESTS
## #' Calculate life expectancy based on
## #' 'ax' and 5-year age groups
## #'
## #' Calculate life expectancy using an
## #' ax-based method, with 5-year age groups.
## #' In practice, the only ax-based method for calculating
## #' life expectancy that works when all age groups
## #' are 5-year is the "mid" method, ie the one
## #' where  ax is 2.5 for all age groups except
## #' the open age group.
## #'
## #' Wih this method, the probability of dying, qx,
## #' can exceed 1. \code{lifeexp_ax_five}
## #' adjusts the probability
## #' downwards, with a warning.
## #'
## #' @param mx A matrix of mortality rates,
## #' using 5-year age groups. 
## #'
## #' @return A vector of life expectancies,
## #' with length equal to nrow(mx).
## #'
## #' @noRd
## lifeexp_ax_five <- function(mx) {
##     n_val <- nrow(mx)
##     n_age <- ncol(mx)
##     ans <- rep(0, times = n_val)
##     n_above_1 <- 0L
##     for (i_val in seq_len(n_val)) {
##         lifeexp <- 0
##         l_prev <- 1
##         if (n_age >= 2L) {
##             for (i_age in seq.int(from = 1L, to = n_age - 1L)) {
##                 m_curr <- mx[i_val, i_age]
##                 q_curr <- 5 * m_curr / (1 + 2.5 * m_curr) ## assumes width is 5 and ax is 2.5
##                 if (q_curr > 1) {
##                     q_curr <- 1
##                     n_above_1 <- n_above_1 + 1L
##                 }
##                 l_curr <- (1 - q_curr) * l_prev
##                 L_curr <- 2.5 * (l_prev + l_curr) ## assumes width is 5 and ax is 2.5
##                 lifeexp <- lifeexp + L_curr
##                 l_prev <- l_curr
##             }
##         }
##         m_curr <- mx[i_val, n_age]
##         L_curr <- l_prev / m_curr
##         lifeexp <- lifeexp + L_curr
##         ans[i_val] <- lifeexp
##     }
##     if (n_above_1 > 0L)
##         warning("estimated probability of dying 'qx' exceeded 1.0 in ",
##                 n_above_1, " cell(s) : adjusted downwards to 1.0",
##                 call. = FALSE)
##     ans
## }


## ## HAS_TESTS
## #' Calculate life expectancy based on
## #' 'ax' and abridged life table age groups
## #'
## #' Calculate life expectancy using ax-based
## #' methods with abridged life table age groups, ie
## #' 0, 1-4, 5-9, ...., A+. The minimum number
## #' of age groups is 3.
## #'
## #' Argument \code{index_method} can be 1 (= "mid")
## #' 2 (= "CD-Female") or 3 (= "CD-Male"). The
## #' value for \code{index_method} affects
## #' the way that a0 and a1 are calculated.
## #' 'ax' equals 2.5 in all other age groups, apart from
## #' A+.
## #'
## #' With this method, the probability of dying,
## #' qx, can exceed 1.
## #' \code{lifeexp_ax_lt} adjusts the estimated
## #' probability downwards, with a warning.
## #'
## #' @param mx A matrix of mortality rates,
## #' using life table age groups.
## #' @param index_method Index number for method
## #' for calculating 'a0' and 'a1'.
## #'
## #' @return A vector of life expectancies,
## #' with length equal to nrow(mx).
## #'
## #' @noRd
## lifeexp_ax_lt <- function(mx, index_method) {
##     n_val <- nrow(mx)
##     n_age <- ncol(mx)
##     ans <- rep(0, times = n_val)
##     n_above_1 <- 0L
##     for (i_val in seq_len(n_val)) {
##         m0 <- mx[i_val, 1L]
##         m1 <- mx[i_val, 2L]
##         if (index_method == 1L) { ## mid
##             a0 <- 0.5
##             a1 <- 2
##         }
##         else if (index_method == 2L) { ## CD-Female
##             if (m0 >= 0.107) {
##                 a0 <- 0.35
##                 a1 <- 1.361
##             }
##             else {
##                 a0 <- 0.053 + 2.8 * m0
##                 a1 <- 1.522 - 1.518 * m0
##             }
##         }
##         else if (index_method == 3L) { ## CD-Male
##             if (m0 >= 0.107) {
##                 a0 <- 0.33
##                 a1 <- 1.352
##             }
##             else {
##                 a0 <- 0.045 + 2.684 * m0
##                 a1 <- 1.651 - 2.816 * m0
##             }
##         }
##         else
##             stop("unexpected value for 'index_method' : ", index_method, call. = FALSE)
##         q0 <- m0 / (1 + (1 - a0) * m0)
##         q1 <- 4 * m1 / (1 + (4 - a1) * m1)
##         l0 <- 1
##         l1 <- 1 - q0
##         l5 <- (1 - q1) * l1
##         L0 <- l1 + a0 * (l0 - l1)
##         L1 <- 4 * l5 + a1 * (l1 - l5)
##         lifeexp <- L0 + L1
##         l_prev <- l5
##         if (n_age >= 4L) {
##             for (i_age in seq.int(from = 3L, to = n_age - 1L)) {
##                 m_curr <- mx[i_val, i_age]
##                 q_curr <- 5 * m_curr / (1 + 2.5 * m_curr) ## assumes width is 5 and ax is 2.5
##                 if (q_curr > 1) {
##                     q_curr <- 1
##                     n_above_1 <- n_above_1 + 1L
##                 }
##                 l_curr <- (1 - q_curr) * l_prev
##                 L_curr <- 2.5 * (l_prev + l_curr) ## assumes width is 5 and ax is 2.5
##                 lifeexp <- lifeexp + L_curr
##                 l_prev <- l_curr
##             }
##         }
##         m_curr <- mx[i_val, n_age]
##         L_curr <- l_prev / m_curr
##         lifeexp <- lifeexp + L_curr
##         ans[i_val] <- lifeexp
##     }
##     if (n_above_1 > 0L)
##         warning("estimated probability of dying 'qx' exceeded 1.0 in ",
##                 n_above_1, " cell(s) : adjusted downwards to 1.0",
##                 call. = FALSE)
##     ans
## }
        

## HAS_TESTS
#' Calculate life expectancy based on 'ax',
#' and one-year age groups
#' 
#' Calculate life expectancy using ax-based
#' methods with one year age groups, ie
#' 0, 1, 2, ...., A+.
#'
#' Argument \code{index_method} can be one of 1 (= "mid"),
#' 2 (= "CD-Female"), 3 (= "CD-Male"),
#' 4 (= "HMD-Female"), or  5 (= "HMD-Male").
#' \code{index_method} affects
#' the way that a0 is are calculated.
#' 'ax' equals 0.5 in all other age groups, apart from
#' A+.
#' 
#' With this method, the probability of dying,
#' qx, can exceed 1.
#' \code{lifeexp_ax_lt} adjusts the estimated
#' probability downwards, with a warning.
#'
#' @param mx A matrix of mortality rates,
#' single-year age groups.
#' @param index_method Index number for
#' method for calculating 'a0'.
#'
#' @return A vector of life expectancies,
#' with length equal to nrow(mx).
#'
#' @noRd
lifeexp_ax_single <- function(mx, index_method) {
    n_val <- nrow(mx)
    n_age <- ncol(mx)
    ans <- rep(0, times = n_val)
    n_above_1 <- 0L
    for (i_val in seq_len(n_val)) {
        m0 <- mx[i_val, 1L]
        if (index_method == 1L ) ## mid
            a0 <- 0.5
        else if (index_method == 2L) { ## CD-Female
            if (m0 >= 0.107)
                a0 <- 0.35
            else
                a0 <- 0.053 + 2.8 * m0
        }
        else if (index_method == 3L) { ## CD-Male
            if (m0 >= 0.107)
                a0 <- 0.33
            else
                a0 <- 0.045 + 2.684 * m0
        }
        else if (index_method == 4) { ## HMD-Female
            if (m0 >= 0.06891)
                a0 <- 0.31411
            else if (m0 >= 0.01724)
                a0 <- 0.04667 + 3.88089 * m0
            else
                a0 <- 0.14903 - 2.05527 * m0
        }
        else if (index_method == 5L) { ## HMD-Male
            if (m0 >= 0.08307)
                a0 <- 0.29915
            else if (m0 >= 0.023)
                a0 <- 0.02832 + 3.26021 * m0
            else
                a0 <- 0.14929 - 1.99545 * m0
        }
        else
            stop("unexpected value for 'index_method' : ", index_method, call. = FALSE)
        lifeexp <- 0
        l0 <- 1
        if (n_age >= 2L) {
            q0 <- m0 / (1 + (1 - a0) * m0)
            l1 <- (1 - q0) * l0
            L0 <- l1 + a0 * (l0 - l1)
            lifeexp <- lifeexp + L0
            l_prev <- l1
            if (n_age >= 3L) {
                for (i_age in seq.int(from = 2L, to = n_age - 1L)) {
                    m_curr <- mx[i_val, i_age]
                    q_curr <- m_curr / (1 + 0.5 * m_curr)
                    if (q_curr > 1) {
                        q_curr <- 1
                        n_above_1 <- n_above_1 + 1L
                    }
                    l_curr <- (1 - q_curr) * l_prev
                    L_curr <- 0.5 * (l_prev + l_curr)
                    lifeexp <- lifeexp + L_curr
                    l_prev <- l_curr
                }
            }
        }
        else
            l_prev <- l0
        m_curr <- mx[i_val, n_age]
        L_curr <- l_prev / m_curr
        lifeexp <- lifeexp + L_curr
        ans[i_val] <- lifeexp
    }
    if (n_above_1 > 0L)
        warning("estimated probability of dying 'qx' exceeded 1.0 in ",
                n_above_1, " cell(s) : adjusted downwards to 1.0",
                call. = FALSE)
    ans
}


## ## HAS_TESTS
## #' Calculate life expectancy based on constant
## #' mortality rates and 5-year age groups
## #'
## #' Calculate life expectancy using the assumption
## #' of constant mortality rates within each
## #' age group, and with 5-year age groups.
## #'
## #' @param mx A matrix of mortality rates,
## #' using 5-year age groups.
## #'
## #' @return A vector of life expectancies,
## #' with length equal to nrow(mx).
## #'
## #' @noRd
## lifeexp_const_five <- function(mx) {
##     n_val <- nrow(mx)
##     n_age <- ncol(mx)
##     ans <- rep(0, times = n_val)
##     for (i_val in seq_len(n_val)) {
##         lifeexp <- 0
##         l_prev <- 1
##         if (n_age >= 2L) {
##             for (i_age in seq_len(n_age - 1L)) {
##                 m_curr <- mx[i_val, i_age]
##                 p_curr <- exp(-5 * m_curr)
##                 l_curr <- p_curr * l_prev
##                 L_curr <- (l_prev - l_curr) / m_curr
##                 lifeexp <- lifeexp + L_curr
##                 l_prev <- l_curr
##             }
##         }
##         m_curr <- mx[i_val, n_age]
##         L_curr <- l_prev / m_curr
##         lifeexp <- lifeexp + L_curr
##         ans[i_val] <- lifeexp
##     }
##     ans
## }


## ## HAS_TESTS
## #' Calculate life expectancy based on constant
## #' mortality rates and life table age groups
## #'
## #' Calculate life expectancy using the assumption
## #' of constant mortality rates within each
## #' age group, and with abridged life table age groups, ie
## #' 0, 1-4, 5-9, ...., A+. The minimum number
## #' of age groups is 3.
## #'
## #' @param mx A matrix of mortality rates,
## #' using life table age groups.
## #'
## #' @return A vector of life expectancies,
## #' with length equal to nrow(mx).
## #'
## #' @noRd
## lifeexp_const_lt <- function(mx) {
##     n_val <- nrow(mx)
##     n_age <- ncol(mx)
##     ans <- rep(0, times = n_val)
##     for (i_val in seq_len(n_val)) {
##         m0 <- mx[i_val, 1L]
##         m1 <- mx[i_val, 2L]
##         p0 <- exp(-1 * m0)
##         p1 <- exp(-4 * m1)
##         l0 <- 1
##         l1 <- p0 * l0
##         l5 <- p1 * l1
##         L0 <- (l0 - l1) / m0
##         L1 <- (l1 - l5) / m1
##         lifeexp <- L0 + L1
##         l_prev <- l5
##         if (n_age >= 4L) {
##             for (i_age in seq.int(from = 3L, to = n_age - 1L)) {
##                 m_curr <- mx[i_val, i_age]
##                 p_curr <- exp(-5 * m_curr)
##                 l_curr <- p_curr * l_prev
##                 L_curr <- (l_prev - l_curr) / m_curr
##                 lifeexp <- lifeexp + L_curr
##                 l_prev <- l_curr
##             }
##         }
##         m_curr <- mx[i_val, n_age]
##         L_curr <- l_prev / m_curr
##         lifeexp <- lifeexp + L_curr
##         ans[i_val] <- lifeexp
##     }
##     ans
## }


## ## HAS_TESTS
## #' Calculate life expectancy based on constant
## #' mortality rates and 1-year age groups
## #'
## #' Calculate life expectancy using the assumption
## #' of constant mortality rates within each
## #' age group, and with 1-year age groups.
## #'
## #' @param mx A matrix of mortality rates,
## #' using 1-year age groups.
## #'
## #' @return A vector of life expectancies,
## #' with length equal to nrow(mx).
## #'
## #' @noRd
## lifeexp_const_single <- function(mx) {
##     n_val <- nrow(mx)
##     n_age <- ncol(mx)
##     ans <- rep(0, times = n_val)
##     for (i_val in seq_len(n_val)) {
##         lifeexp <- 0
##         l_prev <- 1
##         if (n_age >= 2L) {
##             for (i_age in seq_len(n_age - 1L)) {
##                 m_curr <- mx[i_val, i_age]
##                 p_curr <- exp(-1 * m_curr)
##                 l_curr <- p_curr * l_prev
##                 L_curr <- (l_prev - l_curr) / m_curr
##                 lifeexp <- lifeexp + L_curr
##                 l_prev <- l_curr
##             }
##         }
##         m_curr <- mx[i_val, n_age]
##         L_curr <- l_prev / m_curr
##         lifeexp <- lifeexp + L_curr
##         ans[i_val] <- lifeexp
##     }
##     ans
## }



