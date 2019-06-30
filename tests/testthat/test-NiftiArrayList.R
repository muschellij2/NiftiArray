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


testthat::test_that("nifti_header NiftiArrayList", {

  hdr = nifti_header(res)
  hdr2 = nifti_header(res[[length(res)]])
  testthat::expect_equal(hdr, hdr2)
})



testthat::test_that("Converting NiftiArrayList to NiftiArray", {

  mat = as(res, "NiftiArray")
  testthat::expect_is(mat, "NiftiArray")
  testthat::expect_equal(DelayedArray::matrixClass(mat), "NiftiMatrix")

  dmat = dim(mat)[4]
  testthat::expect_equal(dmat, n_images)

})



testthat::test_that("Converting NiftiArrayList to NiftiArray", {

  mat = as(res, "NiftiMatrix")
  vec = DelayedMatrixStats::rowMedians(mat)
  writeNiftiArray(vec, header = nifti_header(res))

  res_mat = as(vec, "NiftiMatrix")
  res_mat = writeNiftiArray(res_mat, header = nifti_header(res))
  class(res_mat)
  res_arr = as(res_mat, "NiftiArray")

  arr = array(vec, dim = dim(res[[1]]) )
  hdr = nifti_header(res)
  res_arr = writeNiftiArray(arr, header = hdr)
  nifti_header(res_arr)

})
