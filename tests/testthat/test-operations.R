testthat::context("Operations of NiftiArray")

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
  res2 = writeNiftiArray(img)

  sum_img = res + res2
  testthat::expect_is(sum_img, "DelayedArray")

  hdr = nifti_header(sum_img)
  testthat::expect_is(hdr, "niftiHeader")
  writeNiftiArray(sum_img)

})



testthat::test_that("Operations and DelayedArray give header", {

  res = writeNiftiArray(img)
  res2 = writeNiftiArray(img)
  dim(res) = c(dim(res), 1)
  dim(res2) = c(dim(res2), 1)
  big_res <- aperm(arbind(aperm(res, 4:1), aperm(res2, 4:1)), 4:1)

})
