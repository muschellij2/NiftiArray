#' Write NiftiArray Object
#'
#' @param x a `niftiImage` object or file path to `NIfTI` file
#' @param filepath The path (as a single character string) to the HDF5
#'  file.
#' @param name The name of the image in the HDF5 file.
#' @param header_name The name of the header in the HDF5 file.
#' @param chunkdim The dimensions of the chunks to use for
#' writing the data to disk. Passed to [HDF5Array::writeHDF5Array].
#' @param level 	The compression level to use for writing the data to disk.
#' Passed to [HDF5Array::writeHDF5Array].
#' @param verbose Display progress. Passed to [HDF5Array::writeHDF5Array].
#'
#' @export
#' @examples
#' nii_fname = system.file("extdata", "example.nii.gz", package = "RNifti")
#' res = writeNiftiArray(nii_fname)
writeNiftiArray <- function(
  x, filepath=tempfile(fileext = ".h5"),
  name = "image",
  header_name = "hdr",
  chunkdim=NULL,
  level=NULL,
  verbose=FALSE)
{

  # for filepath for .nii.gz
  if (is.character(x)) {
    fe = tools::file_ext(x)
    fe = tolower(fe)
    if (fe == "gz") {
      fe = tools::file_ext(sub("[.]gz$", "", x))
      fe = tolower(fe)
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
