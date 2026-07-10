# prep_landbase errors informatively on an unknown status column

    Code
      prep_landbase(src, aoi, status_col = "nope")
    Condition
      Error:
      ! `status_col` "nope" not found in the source layer; available columns: status.

