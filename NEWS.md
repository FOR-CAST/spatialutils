# spatialutils (development version)

* `erase_polygons()` and `sym_difference()` are now S3 generics with `sf` and `SpatVector` methods,
  so an `sf` caller is no longer forced through terra and back. That round-trip was not merely
  wasteful (~18% on a 3,393-polygon layer): `erase_polygons()` goes through sf SPECIFICALLY because
  `terra::erase()` can return one more attribute row than it has geometries, so routing sf callers
  sf -> SpatVector -> sf -> SpatVector -> sf pushed them through the exact conversion the design
  exists to avoid, twice. Each method now returns the class it was given. No existing caller
  changes: every use in this package and its known callers passes `SpatVector`s.
* `erase_polygons_tiled()` does the difference tile by tile. The cost of a difference is superlinear
  in the complexity of the layer being subtracted, so when that layer is one large dissolved
  multipolygon every feature pays for its full complexity. Measured on a 40,301-polygon layer against
  a single 1.88 Mha multipolygon: 11,157 s -> 201.8 s over 56 tiles (55.3x), identical output. On a
  larger study area the same change took a pipeline target from 27.6 h to 35 min (46.7x), again
  bit-identical -- 32,305 features and 513,509.0693 ha either way. Not always a win: tiling adds a
  crop per tile, so measure before adopting it.
* `sym_difference()` and `sym_difference_tiled()` give the symmetric difference of two polygon
  layers, built from two `erase_polygons()` calls so they inherit its guards. Against
  `terra::symdif()` on real data: 8.05x and 19.33x respectively, with identical area. The result
  carries geometry and a `source` column rather than attributes, because the two halves come from
  layers with different columns and `rbind()` on `SpatVector`s NA-fills mismatches silently --
  reporting "this attribute is NA here" where the truth is "this attribute does not exist on this
  side".
* Tiling `intersect_relate()` was tried and REJECTED: correct, but 0.83x -- 20% slower. It already
  drops the features that cannot possibly intersect before calling `terra::intersect()`, so tiling
  adds a crop per tile on top of pre-filtering that is already happening. Recorded so the idea is
  not retried.

* `erase_polygons()` takes the difference of two `SpatVector` polygon layers, keeping `x`'s
  attributes. `terra::erase()` can return a `SpatVector` carrying one more attribute row than it has
  geometries -- it drops a geometry whose difference comes out empty without dropping the matching
  attribute row -- and nothing complains until something else reads the attributes, a long way from
  the cause (<https://github.com/rspatial/terra/issues/2179>). Going through `sf` cannot
  desynchronise, because the attributes are columns of the same data frame as the geometry.
* `intersect_clean()` no longer fails when the intersection contains a bare `LINESTRING`. Two
  polygons that share only an edge intersect in a line, with no `GEOMETRYCOLLECTION` anywhere in the
  result, so the guard testing only for collections never fired and `st_cast()` then aborted with
  "`x` must contain polygon geometries, not lines".
* `intersect_clean()` no longer fails when the intersection is empty -- layers that only touch, or
  do not meet at all. It returned an empty `sf` object through `do.call(rbind, list())`, which is
  `NULL`, and failed on the next line instead.
* `keep_polygons()` keeps only the non-empty polygonal features of an `sf` object, extracting them
  from any `GEOMETRYCOLLECTION` first. Testing for `GEOMETRYCOLLECTION` alone is not enough: an
  overlay can leave a bare `LINESTRING` sitting directly in the geometry column, and `terra::vect()`
  then drops that geometry while keeping its attribute row.
* `overlay_left_join()` tags one polygon layer with another's attributes as an overlay, preserving
  the left layer's footprint exactly -- no area invented, none lost, and none counted twice.
  `sf::st_join()` is not an overlay: it keeps whole geometries from `x` and emits one copy per
  feature of `y` they touch.

* `intersect_clean()` now **merges** each sliver into the neighbouring polygon it shares the longest
  border with, instead of discarding it with `smoothr::drop_crumbs()`. Dropping meant the result no
  longer covered the same footprint as the intersection and the area in the dropped fragments simply
  vanished; merging is what the ArcGIS `Eliminate` tool does, and is why `eliminate_slivers()` was
  written (#1). `smoothr` is no longer a dependency.
* `read_vector_aoi()` now returns valid geometry. Source layers contain invalid features -- a single
  self-intersecting polygon in the BC CEF Forest Disturbance layer was enough to abort a pipeline
  branch with "TopologyException: side location conflict" -- and every GEOS overlay a caller reaches
  for next (`crop()`, `intersect()`, `erase()`) throws on them. Repairing in the caller is not
  sufficient, because the throw happens in whichever overlay runs first, which may be before the
  caller's own repair. A clean layer pays one `terra::is.valid()` pass and nothing else.
* `read_vector_aoi()` no longer fails on a layer whose geometry type is "Unknown (any)" -- what
  `sf::st_write()` produces for mixed POLYGON/MULTIPOLYGON, and very common in GeoPackages. terra's
  *proxy* reader rejects such a layer outright, which aborted the read; a failed proxy now falls
  back to the ordinary filtered read, which handles it.

* `nn_distance()` gains an `exclude` argument, giving the row of `y` that each feature of `x` must
  not match. This is what lets a *chunk* of a layer be measured against the whole layer -- and
  therefore what lets the chunks run in parallel -- since otherwise every feature of the subset
  finds itself at distance 0. Defaults to the previous self-exclusion behaviour.

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
