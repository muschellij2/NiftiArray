testthat::context("Operations of NiftiMatrix")

nii_fname = system.file("extdata", "example.nii.gz", package = "RNifti")
h5_fname = tempfile(fileext = ".h5")
img = RNifti::readNifti(nii_fname)
img_hdr = nifti_header(img)

check_array = function(x) {
  testthat::expect_is(x, "NiftiArray")
  testthat::expect_is(x, "HDF5Array")
  testthat::expect_is(x, "DelayedArray")
}



testthat::test_that("Operations and DelayedArray give header", {

  res = writeNiftiArray(img)
  mat = as(res, "NiftiMatrix")
  testthat::expect_is(mat, "NiftiMatrix")
  testthat::expect_equal(DelayedArray::matrixClass(mat), "NiftiMatrix")
  check_array(mat)

  mat = acbind(mat, mat, mat, mat)
  testthat::expect_is(mat, "DelayedMatrix")
  testthat::expect_equal(DelayedArray::matrixClass(mat), "DelayedMatrix")

  vec_result = DelayedMatrixStats::rowMedians(mat)
  result = matrix(vec_result, ncol = 1)
  result = writeNiftiArray(result, header = nifti_header(res))
  res_arr = as(result, "NiftiArray")

  as(res_arr, "niftiImage")

})





