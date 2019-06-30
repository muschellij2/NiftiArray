testthat::context("Operations of NiftiArrayList")

nii_fname = system.file("extdata", "example.nii.gz", package = "RNifti")
n_images = 5L
fnames = rep(nii_fname, n_images)
res = NiftiArrayList(fnames)


testthat::test_that("Converting NiftiArrayList to NiftiMatrix", {

  mat = as(res, "NiftiMatrix")
  testthat::expect_is(mat, "NiftiMatrix")
  testthat::expect_equal(DelayedArray::matrixClass(mat), "NiftiMatrix")

})



testthat::test_that("Converting NiftiArrayList to NiftiArray", {

  mat = as(res, "NiftiArray")
  testthat::expect_is(mat, "NiftiArray")
  testthat::expect_equal(DelayedArray::matrixClass(mat), "NiftiMatrix")

  dmat = dim(mat)[4]
  testthat::expect_equal(dmat, n_images)

})
