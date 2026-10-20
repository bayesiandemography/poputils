## Helper for creating test data ---------------------------------------------

make_data_decrement <- function(n_draw = NULL) {
  age <- c("0", "1-4", paste(seq(5, 80, 5), seq(9, 84, 5), sep = "-"), "85+")
  n_age <- length(age)
  mx_all <- 0.00005 * exp(0.09 * (seq_len(n_age) * 5 - 5))
  mx_all[1:2] <- c(0.005, 0.0004)
  causes <- c("a", "b", "c")
  share_b <- seq(0.1, 0.6, length.out = n_age)
  share <- cbind(a = 0.3, b = share_b, c = 0.7 - share_b)
  ans <- data.frame(age = rep(age, times = 3),
                    cause = rep(causes, each = n_age),
                    mx = as.double(mx_all * share),
                    popn = rep(seq(1000, 100, length.out = n_age), times = 3))
  if (!is.null(n_draw)) {
    set.seed(0)
    m <- ans$mx * matrix(runif(nrow(ans) * n_draw, 0.8, 1.2), ncol = n_draw)
    ans <- tibble::as_tibble(ans)
    ans$mx <- rvec::rvec(m)
  }
  ans
}
