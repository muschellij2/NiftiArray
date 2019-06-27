#' Write NiftiArray Object
#'
#' @param x
#' @param filepath
#' @param name
#' @param header_name
#' @param chunkdim
#' @param level
#' @param verbose
#'
#' @export
writeNiftiArray <- function(x, filepath=tempfile(fileext = ".h5"),
                            name = "image",
                            header_name = "hdr",
                            chunkdim=NULL,
                            level=NULL,
                            verbose=FALSE)
{

  # for filepath for .nii.gz
  if (is.character(x)) {
    fe = tools::file_ext(x)
    if (fe == "gz") {
      fe = tools::file_ext(sub("[.]gz$", "", x))
    }
    if (fe == "nii") {
      x = RNifti::readNifti(x)
    }
  }

  hdr = nifti_header(x)
  aa = attributes(hdr)
  aa$class = NULL
  class(hdr) = "list"
  attributes(hdr) = aa
  if (!is(x, "DelayedArray")) {
    x = array(x, dim = dim(x))
  }
  HDF5Array::writeHDF5Array(x = x,
                            filepath = filepath, name = name,
                            chunkdim = chunkdim, level = level,
                            verbose = verbose)
  # write header
  rhdf5::h5write(hdr, file = filepath, name = header_name)

  NiftiArray(filepath, name = name, header_name = header_name)
}
