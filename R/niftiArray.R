### - - - - - - - -
### Constructor
###

#' @importMethodsFrom DelayedArray DelayedArray
setMethod(
    "DelayedArray", "NiftiArraySeed",
    function(seed) {
        DelayedArray::new_DelayedArray(
            seed, Class = "NiftiArray")
    }
)


#' @title Realize NIfTI images as DelayedArray objects, using HDF5Array
#'
#' @description NiftiArray, a [HDF5Array::HDF5Array] with a header.
#'
#' @note All the operations available for
#' [HDF5Array::HDF5Array] objects work on
#' NiftiArray objects.
#'
#'
#' @param filepath The path (as a single character string) to the NIfTI or HDF5
#'  file where the dataset is located. If a path to the NIfTI is provided we call
#'  [RNifti::readNifti()] and [NiftiArray::writeNiftiArray()] to convert to the HDF5
#'  and more memory and time are used. If a path to a HDF5 file the data is simply loaded
#'  into R as an object of class [NiftiArray]. A path to the HDF5 file is more memory and time efficient.
#' @param name The name of the group for the NIfTI image in the HDF5 file. Default is set to "image".
#' Unless you have to other "image" groups in the HDF5 file there is no need to change default settings.
#' @param header_name The name of the group for the NIfTI header in the HDF5 file. Default is set to "header".
#' Unless you have to other "header" groups in the HDF5 file there is no need to change default settings.
#' @param type `NA` or the R atomic type, passed to
#' [HDF5Array::HDF5Array()] corresponding to the type of the HDF5 dataset. Default is set to `NA`.
#' Unless you want different types of HDF5 storage files there is no need to change default settings.
#' @param header List of NIfTI header information to override call of
#' [nifti_header].
#' @param extendible Should a single empty dimension be added to the array?
#' Currently necessary for easy reshaping.
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
#' nii_fname = system.file("extdata",
#' "example.nii.gz", package = "RNifti")
#' res = NiftiArray(nii_fname)
#' res2 = NiftiArray(slot(slot(res, "seed"), "filepath"))
#' res2 = NiftiArray(slot(res, "seed"))
NiftiArray <- function(
    filepath, name = "image",
    header_name = "hdr", type = NA, header = NULL,
    extendible = FALSE){
    if (is(filepath, "NiftiArraySeed")) {
        seed <- filepath
    } else {
        seed <- NiftiArraySeed(
            filepath, name = name,
            header_name = header_name, type = type,
            header = header, extendible = extendible)
    }
    DelayedArray::DelayedArray(seed)
}



