testthat::context("Operations of NiftiMatrix")

nii_fname = system.file("extdata", "example.nii.gz", package = "RNifti")
fnames = rep(nii_fname, 5)
res = lapply(fnames, writeNiftiArray)
class(res) = "NiftiArrayList"


testthat::test_that("Operations and DelayedArray give header", {

  mat = as(res, "NiftiMatrix")
  testthat::expect_is(mat, "NiftiMatrix")
  testthat::expect_equal(DelayedArray::matrixClass(mat), "NiftiMatrix")
})
