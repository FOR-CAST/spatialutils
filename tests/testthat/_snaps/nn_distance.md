# `exclude` of the wrong length is rejected

    Code
      nn_distance(v, v, exclude = 1L)
    Condition
      Error in `nn_distance()`:
      ! `exclude` must have one element per feature of `x`

