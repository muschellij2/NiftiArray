testthat::context("Operations of NiftiArray")

nii_fname = system.file("extdata", "example.nii.gz",
                        package = "RNifti")
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
  res2 = writeNiftiArray(img)

  sum_img = res + res2
  testthat::expect_is(sum_img, "DelayedArray")

  testthat::expect_is(as(sum_img, "NiftiArray"), "NiftiArray")

  hdr = nifti_header(sum_img)
  testthat::expect_is(hdr, "niftiHeader")
  writeNiftiArray(sum_img)

  rr = HDF5Array::HDF5Array(res2@seed@filepath, "image")
  image = res + rr
  hdr = nifti_header(image)
  testthat::expect_is(hdr, "niftiHeader")

  rr = HDF5Array::HDF5Array(res2@seed@filepath, "image")
  image = rr + rr
  testthat::expect_error(nifti_header(image), "No seeds")
  testthat::expect_is(hdr, "niftiHeader")

  testthat::expect_is(as(rr, "NiftiArray"), "NiftiArray")
  testthat::expect_warning(as(rr, "NiftiArray"), "No header")

})

testthat::test_that("Conversion to NiftiArray", {

  run_mat = matrix(rnorm(100), nrow = 10)
  testthat::expect_is(as(run_mat, "NiftiArray"), "NiftiArray")
  testthat::expect_is(as(as(run_mat, "NiftiArray"), "NiftiMatrix"),
                      "NiftiMatrix")
  run_mat = as(run_mat, "NiftiArray")
  testthat::expect_is(as(run_mat, "NiftiMatrix"), "NiftiMatrix")

  testthat::expect_is(as(rnorm(100), "NiftiMatrix"), "NiftiMatrix")

})

testthat::test_that("Conversion to NiftiArray", {

  arr = writeNiftiArray(nii_fname)
  out_img = as(as(arr, "NiftiMatrix"), "niftiImage")
  testthat::expect_is(out_img, "niftiImage")

  testthat::expect_equal(dim(out_img), dim(img))

})


testthat::test_that("Operations and DelayedArray give header", {

  res = writeNiftiArray(img)
  res2 = writeNiftiArray(img)
  dim(res) = c(dim(res), 1)
  dim(res2) = c(dim(res2), 1)
  big_res <- DelayedArray::aperm(
    DelayedArray::arbind(
      DelayedArray::aperm(res, 4:1),
      DelayedArray::aperm(res2, 4:1)),
    4:1)

})
