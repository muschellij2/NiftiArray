#' @title Write NiftiArray Object
#'
#' @description A function for writing NIfTI objects on disk to a specifically formatted HDF5 file.
#' The HDF5 file will include a group for the NIfTI image or array and the NIfTI header.
#'
#' @param x A `niftiImage` object loaded into R or a file path to a `NIfTI` file.
#' @param filepath The file path (single character string) of where to save the HDF5
#' file converted from the NIfTI input as `x`. By default creates a temporary file on disk.
#' @param name The name of the group for the NIfTI image in the HDF5 file. Default is set to "image".
#' Unless you have to other "image" groups in the HDF5 file there is no need to change default settings.
#' @param header_name The name of the group for the NIfTI header in the HDF5 file. Default is set to "header".
#' Unless you have to other "header" groups in the HDF5 file there is no need to change default settings.
#' @param chunkdim The dimensions of the chunks to use for
#' writing the data to disk.
#' Passed to [HDF5Array::writeHDF5Array].
#' @param level The compression level,
#' passed to [HDF5Array::writeHDF5Array].
#' @param verbose Display progress,
#' passed to [HDF5Array::writeHDF5Array].
#' @param header List of header information;
#' overrides call of [nifti_header].
#' @param overwrite `FALSE` by default. In the
#' event that an HDF5 file already exists for `filepath`
#' input then do not overwrite it.
#' If set to `TRUE` then the `name` and `header_name` group objects in the HDF5 file
#' location will overwrite.
#'
#' @return A `NiftiArray` object.
#' @export
#' @importFrom RNifti readNifti
#' @importFrom HDF5Array writeHDF5Array
#' @importFrom rhdf5 h5closeAll h5delete h5write
#' @examples
#' nii_fname = system.file("extdata", "example.nii.gz", package = "RNifti")
#' res = writeNiftiArray(nii_fname)
#' filepath = tempfile(fileext = ".h5")
#' res = writeNiftiArray(nii_fname, filepath = filepath)
#' testthat::expect_error(
#'    writeNiftiArray(nii_fname, filepath = filepath),
#'    regexp = "already exist",
#' )
#' res = writeNiftiArray(nii_fname, filepath = filepath, overwrite = TRUE)
#' img = RNifti::readNifti(nii_fname)
#' writeNiftiArray(c(img), header = nifti_header(img))
writeNiftiArray <- function(x, filepath = tempfile(fileext = ".h5"),
  name = "image", header_name = "hdr", chunkdim = NULL, level = NULL,
  verbose = FALSE, header = NULL, overwrite = FALSE){
  # Check if filepath exists as h5 already
  # If it does delete it, will error if the file exists and no overwrite
  if (file.exists(filepath)) {
    if (overwrite) {
      # Could keep the file but remove the contents/groups in the h5 with
      rhdf5::h5delete(file = filepath, name = name)
      rhdf5::h5delete(file = filepath, name = header_name)
    } else {
      stop(paste0("The HDF5 filepath given already exists. ",
                  "Please delete file or set overwrite = TRUE."))
    }
  }
  run_gc = FALSE
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
      run_gc = TRUE
    }
  }
  if (!is.null(header)) {
    hdr = header
  } else {
    hdr = nifti_header(x)
  }
  if (is.vector(x)) {
    x = matrix(x, ncol = 1)
  }
  if (!is(x, "DelayedArray")) {
    x = array(x, dim = dim(x))
  }
  HDF5Array::writeHDF5Array(x = x, filepath = filepath, name = name,
                            chunkdim = chunkdim, level = level,
                            verbose = verbose)
  if (run_gc) {
    rm(x); gc()
  }
  if (!is.null(hdr)) {
    aa = attributes(hdr)
    aa$class = NULL
    class(hdr) = "list"
    attributes(hdr) = aa
    rhdf5::h5write(hdr, file = filepath, name = header_name) # write header
    rhdf5::h5closeAll() # Close all open HDF5 handles in the environment
  }
  NiftiArray(filepath, name = name, header_name = header_name)
}
