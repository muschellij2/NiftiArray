#' @export
writeNiftiArray <- function(x, filepath=NULL,
                            name = "image",
                            header_name = "hdr",
                            chunkdim=NULL,
                            level=NULL,
                            verbose=FALSE)
{

  hdr = niftiHeader(x)
  aa = attributes(hdr)
  aa$class = NULL
  class(hdr) = "list"
  attributes(hdr) = aa
  x = array(x, dim = dim(x))
  HDF5Array::writeHDF5Array(x = x,
                                  filepath = filepath, name = name,
                                  chunkdim = chunkdim, level = level,
                                  verbose = verbose)
  rhdf5::h5write(hdr, file = filepath, name = header_name)

  # sink <- HDF5RealizationSink(dim(x), dimnames(x), type(x),
  #                             filepath=filepath, name=name,
  #                             chunkdim=chunkdim, level=level)
  # BLOCK_write_to_sink(x, sink)
  # as(sink, "HDF5Array")
  # as(sink, "NiftiArray")
  NiftiArray(filepath, name = name, header_name = header_name)
}
