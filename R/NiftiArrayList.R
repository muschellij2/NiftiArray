#' Write NiftiArray Object
#'
#' @param x a character or list of objects to be called with
#' [writeNiftiArray]
#' @param ... additional arguments to pass to [NiftiArray]
#' @param verbose show progress bars if `pbapply` package installed.
#'
#' @return A list of class `NiftiArrayList`, which is a
#' list if `NiftiArray` objects
#' @export
#' @examples
#' nii_fname = system.file("extdata",
#' "example.nii.gz", package = "RNifti")
#' nii_fname = rep(nii_fname, 3)
#' res = NiftiArrayList(nii_fname)
#' if (requireNamespace("pbapply", quietly = TRUE)) {
#'    res = NiftiArrayList(nii_fname, verbose = TRUE)
#' } else {
#'    testthat::expect_warning({
#'    res = NiftiArrayList(nii_fname, verbose = TRUE)
#'    })
#' }
#' testthat::expect_is(res, "NiftiArrayList")
#' mat = as(res, "NiftiMatrix")
#' arr = as(res, "NiftiArray")
#' h5 = unlist(DelayedArray::seedApply(res, slot, "filepath"))
#' res2 = NiftiArrayList(h5)
#' testthat::expect_is(res2, "NiftiArrayList")
#' mat = as(res2, "NiftiMatrix")
#' arr = as(res2, "NiftiArray")
NiftiArrayList <- function(x, ..., verbose = FALSE) {
  applier = lapply
  if (verbose) {
    if (requireNamespace("pbapply", quietly = TRUE)) {
      applier = pbapply::pblapply
    } else {
      warning(paste0("verbose is set to TRUE for ",
                     "NiftiArrayList, but pbapply not ",
                     "installed, defaulting to lapply"))
    }
  }
  res = applier(x, function(xx) {
    # writeNiftiArray(xx, ...)
    NiftiArray(xx, ...)
  })
  class(res) = "NiftiArrayList"
  attr(res, "verbose") = verbose
  res
}
