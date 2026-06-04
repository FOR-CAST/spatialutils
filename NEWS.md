# spatialutils (development version)

* initial package version:
  - `eliminate_slivers` (alias `desliver`) to remove sliver polygons by merging
    each into its longest-shared-border neighbour (ArcGIS `Eliminate`, LENGTH option);
  - `intersect_clean` to perform intersections and cleanup crumbs;
  - `st_erase` to erase features;
  - `st_perimeter` to remove holes;
  - `st_union_analysis` to perform unions by attribute;
