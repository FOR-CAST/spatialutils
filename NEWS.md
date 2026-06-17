# spatialutils 0.0.0.9008

* `calc_centroid` to get the coordinates of the centroid of a polygon;
* `create_bbox` to create a bounding box of a study area;
* `extract_coords` to convert sf coordinates to a numeric vector;
* `prep_agg_lonlat_raster` to prepare an aggregated lon/lat raster for climate-grid retrieval;
* `save_gpkg` to save geospatial vector data to a GeoPackage;
* `spatial_join_intersects` to perform a spatial join based on intersection;

# spatialutils (development version)

* initial package version:
  - `eliminate_slivers` (alias `desliver`) to remove sliver polygons by merging
    each into its longest-shared-border neighbour (ArcGIS `Eliminate`, LENGTH option);
  - `intersect_clean` to perform intersections and cleanup crumbs;
  - `st_erase` to erase features;
  - `st_perimeter` to remove holes;
  - `st_union_analysis` to perform unions by attribute;
