# expanse_planar warns for a geographic CRS

    Code
      x <- expanse_planar(mk_poly(0, 1, 0, 1, crs = "EPSG:4326"))
    Condition
      Warning in `expanse_planar()`:
      `x` has a geographic CRS; planar area is not meaningful and `terra` returns geodesic area regardless. Project to a projected CRS first.

# dissolve_by rejects unknown columns

    Code
      dissolve_by(v, "nope")
    Condition
      Error in `dissolve_by()`:
      ! column(s) not found in `x`: nope

