## Write `n` unit squares (status column `status`) to a shapefile in a temporary directory, then cut
## its .dbf to `keep` complete records: the state a .dbf is left in when a writer stops at the
## dBASE 2 GiB size limit. The header's record count still says `n`, as it did for the CLS S17 and
## S20 landbases. Returns the .shp path.
local_truncated_shapefile <- function(n = 3L, keep = n, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  shp <- file.path(dir, "landbase.shp")

  v <- do.call(
    rbind,
    lapply(seq_len(n) - 1L, function(i) terra::as.polygons(terra::ext(2 * i, 2 * i + 1, 0, 1)))
  )
  terra::crs(v) <- "EPSG:3857"
  v$status <- rep_len(c("Active", "Passive"), n)
  terra::writeVector(v, shp)

  if (keep < n) {
    dbf <- sub("\\.shp$", ".dbf", shp)
    bytes <- readBin(dbf, "raw", file.size(dbf))
    hdrLen <- readBin(bytes[9:10], "integer", size = 2L, signed = FALSE, endian = "little")
    recLen <- readBin(bytes[11:12], "integer", size = 2L, signed = FALSE, endian = "little")
    writeBin(bytes[seq_len(hdrLen + keep * recLen)], dbf)
  }

  shp
}
