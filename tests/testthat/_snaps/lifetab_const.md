# constant-hazard functions check their inputs

    Code
      .mx_to_lifetab_const("a", width = c(1, Inf))
    Condition
      Error in `.mx_to_lifetab_const()`:
      ! `mx` must be numeric.

---

    Code
      .mx_to_lifetab_const(0.1, width = Inf)
    Condition
      Error in `.mx_to_lifetab_const()`:
      ! `mx` must have at least two age intervals.

---

    Code
      .mx_to_lifetab_const(c(0.1, 0.2), width = c(1, 2, Inf))
    Condition
      Error in `.mx_to_lifetab_const()`:
      ! `width` must be a numeric vector with one element per age interval (2).

---

    Code
      .mx_to_lifetab_const(c(0.1, 0.2), width = c(1, 5))
    Condition
      Error in `.mx_to_lifetab_const()`:
      ! Last element of `width` must be `Inf` (the open interval).

---

    Code
      .lx_to_mx_const(c(1, 0.5), width = c(0, Inf))
    Condition
      Error in `.lx_to_mx_const()`:
      ! Widths of closed intervals must be finite and positive.

---

    Code
      .brass_logit_lx(c(1, 0.5), alpha = c(0, 1), beta = c(1, 1, 1))
    Condition
      Error in `.brass_logit_lx()`:
      ! `beta` must be a numeric vector with length 1 or the length of `alpha`.

---

    Code
      .brass_logit_lx(cbind(c(1, 0.5), c(1, 0.6)), alpha = c(0, 1, 2))
    Condition
      Error in `.brass_logit_lx()`:
      ! `lx_std` must have one column or one column per element of `alpha`.

