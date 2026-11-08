# count_assert() throws error listing failed conditions

    Code
      count_assert(popn, whole = TRUE)
    Condition
      Error in `count_assert()`:
      ! `popn` fails checks `nonneg` and `whole`.
      x `nonneg`: Example: popn[2] is -3.
      x `whole`: Example: popn[3] is 2.5.

# prob_assert() throws error when values outside [0, 1]

    Code
      prob_assert(qx)
    Condition
      Error in `prob_assert()`:
      ! `qx` fails checks `nonneg` and `le_one`.
      x `nonneg`: Example: qx[3] is -0.1.
      x `le_one`: Example: qx[2] is 1.3.

