testthat::context("Operations of NiftiMatrix")

nii_fname = system.file("extdata",
                        "example.nii.gz", package = "RNifti")
h5_fname = tempfile(fileext = ".h5")
img = RNifti::readNifti(nii_fname)
img_hdr = nifti_header(img)

nii_4d = system.file("extdata",
                     "example_4d.nii.gz",
                     package = "RNifti")


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

  hdf5mat = HDF5Array::HDF5Array(res@seed@filepath, "image")
  testthat::expect_is(as(hdf5mat, "NiftiMatrix"), "NiftiMatrix")
  testthat::expect_warning(as(hdf5mat, "NiftiMatrix"))
  testthat::expect_equal(DelayedArray::matrixClass(mat), "NiftiMatrix")
  check_array(mat)

  mat = DelayedArray::acbind(mat, mat, mat, mat)
  testthat::expect_is(mat, "DelayedMatrix")
  testthat::expect_equal(DelayedArray::matrixClass(mat), "DelayedMatrix")

  vec_result = DelayedMatrixStats::rowMedians(mat)
  result = matrix(vec_result, ncol = 1)
  result = writeNiftiArray(result, header = nifti_header(res))
  res_arr = as(result, "NiftiArray")

  as(res_arr, "niftiImage")

})

testthat::test_that("4D NiftiMatrix Example", {

  res = writeNiftiArray(nii_4d)
  nd = length(dim(res))
  mat = as(res, "NiftiMatrix")
  testthat::expect_equal(ncol(mat), dim(res)[4])
  testthat::expect_is(mat, "NiftiMatrix")
  testthat::expect_equal(DelayedArray::matrixClass(mat), "NiftiMatrix")
  check_array(mat)

  back_res = as(mat, "NiftiArray")
  testthat::expect_equal(dim(back_res), dim(res))
  check_array(back_res)

  as(back_res, "niftiImage")

})

