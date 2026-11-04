## A shapefile's attribute table (.dbf) cannot exceed 2 GiB, and a writer that hits the limit can
## leave a .dbf holding fewer records than the .shp holds shapes, with a header that still claims
## all of them. GDAL stops reading at the first shape without a record and only warns ("fread(...)
## failed on DBF file"), so the read returns a plausible-looking subset. The CLS S17 landbase lost
## 13% of its shapes this way. These helpers make that an error.

## Shapes indexed by the .shx beside `shp`, and complete records in its .dbf. Both come from the
## file headers and sizes, so the cost does not grow with the file. NULL unless `shp` is a local
## .shp with both sidecar files.
shapefile_counts <- function(shp) {
  if (!is.character(shp) || length(shp) != 1L || !grepl("\\.shp$", shp, ignore.case = TRUE)) {
    return(NULL)
  }
  base <- sub("\\.shp$", "", shp, ignore.case = TRUE)
  sidecar <- function(ext) {
    f <- paste0(base, c(tolower(ext), toupper(ext)))
    f <- f[file.exists(f)]
    if (length(f)) f[[1L]] else NA_character_
  }
  shx <- sidecar(".shx")
  dbf <- sidecar(".dbf")
  if (is.na(shx) || is.na(dbf)) {
    return(NULL)
  }

  ## .shx: a 100-byte header, then 8 bytes per shape; bytes 25-28 hold the file length in 16-bit
  ## words, big-endian (the count GDAL uses)
  shxHdr <- readBin(shx, "raw", 100L)
  dbfHdr <- readBin(dbf, "raw", 32L)
  if (length(shxHdr) < 100L || length(dbfHdr) < 32L) {
    return(NULL)
  }
  words <- readBin(shxHdr[25:28], "integer", size = 4L, endian = "big")
  shapes <- (as.numeric(words) * 2 - 100) / 8

  ## .dbf: bytes 5-8 the record count (unsigned, little-endian), 9-10 the header length and 11-12
  ## the record length; a cut file holds fewer complete records than its header says
  nHdr <- readBin(dbfHdr[5:8], "integer", size = 4L, endian = "little")
  nHdr <- if (nHdr < 0) nHdr + 2^32 else as.numeric(nHdr)
  hdrLen <- readBin(dbfHdr[9:10], "integer", size = 2L, signed = FALSE, endian = "little")
  recLen <- readBin(dbfHdr[11:12], "integer", size = 2L, signed = FALSE, endian = "little")
  if (recLen == 0L) {
    return(NULL)
  }
  records <- min(nHdr, floor((file.size(dbf) - hdrLen) / recLen))

  c(shapes = shapes, records = records)
}

## Stop if the shapefile `src` has fewer complete attribute records than shapes. A no-op for any
## other source, or a shapefile whose sidecar files cannot be read.
stop_if_dbf_truncated <- function(src) {
  n <- shapefile_counts(src)
  if (!is.null(n) && n[["records"]] < n[["shapes"]]) {
    fmt <- function(x) format(x, big.mark = ",", scientific = FALSE)
    stop(
      src,
      ": the attribute table (.dbf) holds ",
      fmt(n[["records"]]),
      " complete records for ",
      fmt(n[["shapes"]]),
      " shapes. GDAL would stop at the first shape without a record and drop the remaining ",
      fmt(n[["shapes"]] - n[["records"]]),
      " with only a warning. A .dbf cut at the 2 GiB dBASE size limit looks like this; ",
      "re-export the layer as a GeoPackage or File Geodatabase.",
      call. = FALSE
    )
  }
  invisible(src)
}

## Evaluate a read of `src`, turning GDAL's warning that it could not read an attribute record into
## an error. This covers sources `stop_if_dbf_truncated()` cannot inspect (an archive read through
## /vsizip/, say), where the warning is the only sign that features were dropped.
with_dbf_guard <- function(expr, src) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("failed on DBF file", conditionMessage(w), fixed = TRUE)) {
      stop(
        paste(src, collapse = " "),
        ": GDAL could not read an attribute record (",
        trimws(conditionMessage(w)),
        "), so every feature after it would be dropped. The .dbf may be cut at the 2 GiB ",
        "dBASE size limit; re-export the layer as a GeoPackage or File Geodatabase.",
        call. = FALSE
      )
    }
  })
}
