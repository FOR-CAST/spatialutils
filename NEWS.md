# spatialutils 0.0.0.9013

* `dissolve_by()` to dissolve a vector layer on one or more attributes, keeping `NA` as a group of
  its own. `terra::aggregate(by = )` cannot dissolve on several columns when any of them contains
  `NA`: it returns a `SpatVector` whose attribute table has fewer rows than it has geometries, which
  then errors on the next access rather than at the aggregate call.
* `drop_values()` to strip every attribute from a `SpatVector` while keeping its geometries --
  `x[, character(0)]` is an error in `terra`, and `x$col <- NULL` has to be repeated per column.
* `eliminate_slivers()` now thresholds on **planar** area in the layer's own projection, matching
  `sf::st_area()` and ArcGIS `Shape_Area`. It previously used the `terra::expanse()` default, which
  reprojects to lon/lat and returns geodesic area -- 2.8% larger in Canada Atlas Lambert at BC
  latitudes, enough to move features across the threshold. Equal-area projections (e.g. BC Albers)
  are unaffected.
* `expanse_planar()` for planar area in a layer's own projection, as `sf::st_area()` and ArcGIS
  `Shape_Area` report it, guarding against the geodesic `terra::expanse()` default.
* `nn_distance()` for the distance from every feature to its nearest neighbour, via one indexed
  candidate query per round over an escalating search radius rather than a per-feature
  `sf::st_nearest_feature()` call. About 14x faster on a 70,000-polygon layer (1.72 s to 0.12 s per
  polygon), and exact.

# spatialutils 0.0.0.9012

* `repair_geoms` to fix invalid geometries efficiently: validate once and run
  `terra::makeValid()` on only the invalid subset, then recombine with the already-valid
  majority (dropping any that stay invalid or empty). Avoids running `makeValid()` over a whole
  layer when only a small fraction of geometries are invalid, e.g. national fire perimeters
  where ~767 of ~41k NFDB polygons are invalid. Now used internally by `prep_landbase`.

# spatialutils 0.0.0.9011

* `read_vector_aoi` to read a vector source with the spatial filter pushed down to the GDAL/OGR
  read, so only features overlapping an area of interest (and only the requested columns) are
  materialised in R -- for working with enormous polygon coverages.
* `intersect_relate` for a `terra`-native intersection that drops non-intersecting features first,
  so only candidate geometries reach the GEOS overlay.
* `prep_landbase` to prepare active/passive landbase-status polygons (read + column/spatial
  prefilter + repair + dissolve by status + clip), generalising the per-FMA landbase processing
  formerly repeated across the `LandWeb_preamble` study-area helpers. The dissolve defaults to a
  fast rasterise-and-polygonise (`dissolve = "raster"`, snapped to `dissolve_res`); an exact but
  much slower vector union (`dissolve = "vector"`) and `"none"` are also available. On a real
  ~420k-polygon landbase this cut the read + repair + dissolve from ~12 h (naive `sf` union) to
  roughly a minute.

# spatialutils 0.0.0.9010

* `prep_vector` to crop, mask, and reproject a vector to a study area -- a lean, `terra`-based
  replacement for the crop/mask/reproject that `reproducible::prepInputs()` did internally.

# spatialutils 0.0.0.9009

* Relicensed from GPL (>= 3) to Apache License (>= 2).

# spatialutils 0.0.0.9008

* `calc_centroid` to get the coordinates of the centroid of a polygon;
* `create_bbox` to create a bounding box of a study area;
* `extract_coords` to convert sf coordinates to a numeric vector;
* `prep_agg_lonlat_raster` to prepare an aggregated lon/lat raster for climate-grid retrieval;
* `save_gpkg` to save geospatial vector data to a GeoPackage;
* `spatial_join_intersects` to perform a spatial join based on intersection;

# spatialutils 0.0.0.9007

* initial package version:
  - `eliminate_slivers` (alias `desliver`) to remove sliver polygons by merging
    each into its longest-shared-border neighbour (ArcGIS `Eliminate`, LENGTH option);
  - `intersect_clean` to perform intersections and cleanup crumbs;
  - `st_erase` to erase features;
  - `st_perimeter` to remove holes;
  - `st_union_analysis` to perform unions by attribute;
