#' Write NiftiArray Object
#'
#' @param x a character or list of objects to be called with
#' [writeNiftiArray]
#' @param ... additional arguments to pass to [NiftiArray]
#'
#' @return A list of class `NiftiArrayList`, which is a
#' list if `NiftiArray` objects
#' @export
#' @examples
#' nii_fname = system.file("extdata",
#' "example.nii.gz", package = "RNifti")
#' nii_fname = rep(nii_fname, 3)
#' res = NiftiArrayList(nii_fname)
#' testthat::expect_is(res, "NiftiArrayList")
#' mat = as(res, "NiftiMatrix")
#' arr = as(res, "NiftiArray")
#' h5 = unlist(DelayedArray::seedApply(res, slot, "filepath"))
#' res2 = NiftiArrayList(h5)
#' testthat::expect_is(res2, "NiftiArrayList")
#' mat = as(res2, "NiftiMatrix")
#' arr = as(res2, "NiftiArray")
NiftiArrayList <- function(x, ...) {
    res = lapply(x, function(xx) {
      # writeNiftiArray(xx, ...)
      NiftiArray(xx, ...)
    })
    class(res) = "NiftiArrayList"
    res
}
