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
#' @param header list of header information to override call of
#' @param overwrite FALSE by default and an in the event that an HDF5 file already exists for `filepath` input then do not overwrite it.
#' If set to TRUE then the "image" and "hdr" objects at this file location will overwrite.
#' [nifti_header]
#'
#' @export
#' @importFrom HDF5Array writeHDF5Array
#' @importFrom rhdf5 h5closeAll h5delete h5write
#' @examples
#' nii_fname = system.file("extdata", "example.nii.gz", package = "RNifti")
#' res = writeNiftiArray(nii_fname)
writeNiftiArray <- function(
  x, filepath=tempfile(fileext = ".h5"),
  name = "image",
  header_name = "hdr",
  chunkdim=NULL,
  level=NULL,
  verbose=FALSE,
  header = NULL,
  overwrite = FALSE)
{
  # Check if filepath exists as h5 already
  # If it does delete it
  if(file.exists(filepath) & overwrite == TRUE){
    # Could keep the file but remove the contents/groups in the h5 with
    rhdf5::h5delete(file = filepath, name = "image")
    rhdf5::h5delete(file = filepath, name = "hdr")
  } else if(file.exists(filepath) & overwrite == FALSE){
    stop('The HDF5 filepath given already exists. Please delete file or set overwrite = TRUE.')
  }
  # Function will error if the file exists

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

  if (!is.null(header)) {
    hdr = header
  } else {
    hdr = nifti_header(x)
  }
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

  # Close all open HDF5 handles in the environment
  rhdf5::h5closeAll()

  NiftiArray(filepath, name = name, header_name = header_name)
}
