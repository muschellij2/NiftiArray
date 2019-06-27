### =========================================================================
### NiftiArraySeed objects
### -------------------------------------------------------------------------
#' @importClassesFrom HDF5Array HDF5ArraySeed
#' @aliases DelayedArray,NiftiArraySeed-method
#' @export
setClass("NiftiArraySeed",
         contains = "HDF5ArraySeed",
         slots = c(
           sizeof_hdr = "integer",
           dim_info = "integer",
           dim_ = "integer",
           intent_p1 = "numeric",
           intent_p2 = "numeric",
           intent_p3 = "numeric",
           intent_code = "integer",
           datatype = "integer",
           bitpix = "integer",
           slice_start = "integer",
           pixdim = "numeric",
           vox_offset = "numeric",
           scl_slope = "numeric",
           scl_inter = "numeric",
           slice_end = "integer",
           slice_code = "integer",
           xyzt_units = "integer",
           cal_max = "numeric",
           cal_min = "numeric",
           slice_duration = "numeric",
           toffset = "numeric",
           descrip = "character",
           aux_file = "character",
           qform_code = "integer",
           sform_code = "integer",
           quatern_b = "numeric",
           quatern_c = "numeric",
           quatern_d = "numeric",
           qoffset_x = "numeric",
           qoffset_y = "numeric",
           qoffset_z = "numeric",
           srow_x = "numeric",
           srow_y = "numeric",
           srow_z = "numeric",
           intent_name = "character",
           magic = "character"
         )
)



#' Seed for NiftiArray Class
#'
#' @param filepath The path (as a single character string) to the HDF5
#'  file where the dataset is located.
#' @param name The name of the image in the HDF5 file.
#' @param header_name The name of the header in the HDF5 file.
#' @param type `NA` or the R atomic type, passed to
#' [HDF5Array::HDF5Array()]
#'
#' @return A `NiftiArraySeed` object
#' @export
#' @importFrom HDF5Array HDF5ArraySeed
#' @importFrom rhdf5 h5read
#' @importFrom S4Vectors new2
#' @examples
#' nii_fname = system.file("extdata", "example.nii.gz", package = "RNifti")
#' res = NiftiArraySeed(nii_fname)
NiftiArraySeed <- function(filepath,
                           name = "image",
                           header_name = "hdr",
                           type=NA)
{
  # for filepath for .nii.gz
  fe = tools::file_ext(filepath)
  if (fe == "gz") {
    fe = tools::file_ext(sub("[.]gz$", "", filepath))
  }
  if (fe == "nii") {
    x = RNifti::readNifti(filepath)
    filepath = tempfile(fileext = ".h5")
    writeNiftiArray(x, filepath = filepath,
                    name = name,
                    header_name = header_name)
    rm(x); gc()
  }

  seed = HDF5Array::HDF5ArraySeed(filepath, name = name, type = type)
  args = list(
    filepath = seed@filepath,
    name = seed@name,
    dim = seed@dim,
    first_val = seed@first_val,
    chunkdim = seed@chunkdim
  )
  hdr = rhdf5::h5read(filepath, name = header_name)
  hdr = lapply(hdr, as.vector)
  hdr$dim_ = hdr$dim
  hdr$dim = NULL
  args = c(args, hdr)
  args = c("NiftiArraySeed", args )
  do.call(S4Vectors::new2, args = args)
}
