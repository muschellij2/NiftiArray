#' @importFrom S4Vectors isTRUEorFALSE
#' @importFrom HDF5Array HDF5RealizationSink
#' @importFrom DelayedArray BLOCK_write_to_sink
writeHDF5Array2 = function (x, filepath = NULL, name = NULL, chunkdim = NULL, level = NULL,
                            verbose = FALSE, dim = NULL, dimnames = NULL) {
  if (!isTRUEorFALSE(verbose)) {
    stop("'verbose' must be TRUE or FALSE")
  }
  if (is.null(dim)) {
    dim = dim(x)
  }
  if (is.null(dimnames)) {
    dimnames = dimnames(x)
  }
  sink <- HDF5RealizationSink(dim, dimnames, type(x),
                              filepath = filepath, name = name, chunkdim = chunkdim,
                              level = level)
  if (verbose) {
    old_verbose <- DelayedArray:::set_verbose_block_processing(verbose)
    on.exit(DelayedArray:::set_verbose_block_processing(old_verbose))
  }
  BLOCK_write_to_sink(x, sink)
  as(sink, "HDF5Array")
}
