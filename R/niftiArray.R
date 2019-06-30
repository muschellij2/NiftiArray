### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

#' @importMethodsFrom DelayedArray DelayedArray
setMethod("DelayedArray", "NiftiArraySeed",
          function(seed) DelayedArray::new_DelayedArray(seed, Class = "NiftiArray")
)


#' NIfTI images as DelayedArray objects, using HDF5Array
#'
#' The NiftiArray class is a [DelayedArray::DelayedArray] subclass
#' represented in HDF5, with the header.
#' All the operations available for [HDF5Array::HDF5Array]
#' objects work on NiftiArray objects.
#'
#'
#' @param filepath The path (as a single character string) to the HDF5
#'  file where the dataset is located.
#' @param name The name of the image in the HDF5 file.
#' @param header_name The name of the header in the HDF5 file.
#' @param type `NA` or the R atomic type, passed to
#' [HDF5Array::HDF5Array()].
#' @param header list of header information to override call of
#' [nifti_header]
#'
#' @return A `NiftiArray` object
#' @export
#'
#' @aliases class:NiftiArray
#' @aliases NiftiArray-class
#' @aliases NiftiArray
#'
#' @importFrom DelayedArray DelayedArray
#' @import methods
#' @examples
#' nii_fname = system.file("extdata", "example.nii.gz", package = "RNifti")
#' res = NiftiArray(nii_fname)
#' res2 = NiftiArray(slot(slot(res, "seed"), "filepath"))
#' res2 = NiftiArray(slot(res, "seed"))
NiftiArray <- function(filepath, name = "image", header_name = "hdr",
                       type = NA, header = NULL)
{
  if (is(filepath, "NiftiArraySeed")) {
    seed <- filepath
  } else {
    seed <- NiftiArraySeed(filepath, name = name,
                           header_name = header_name,
                           type = type,
                           header = header)
  }
  DelayedArray::DelayedArray(seed)
}



