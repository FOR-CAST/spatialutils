# all-slivers warns and returns the input unchanged (pipe-safe)

    Code
      res <- eliminate_slivers(fx, threshold = 1e+06)
    Condition
      Warning in `eliminate_slivers()`:
      All features are slivers (area <= threshold); returning `x` unchanged.

# a geographic CRS warns

    Code
      out <- eliminate_slivers(fx, threshold = 6)
    Condition
      Warning in `eliminate_slivers()`:
      `x` has a geographic CRS; sliver areas and border lengths are not metric. Project to an equal-area / projected CRS before eliminating slivers.

# non-polygon input is rejected

    Code
      eliminate_slivers(pts, threshold = 1)
    Condition
      Error in `eliminate_slivers()`:
      ! `x` must contain polygon geometries, not points.

