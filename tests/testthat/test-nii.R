testthat::context("Trying to make NiftiArray objects")

nii_fname = system.file("extdata", "example.nii.gz", package = "RNifti")
h5_fname = tempfile(fileext = ".h5")
img = RNifti::readNifti(nii_fname)

check_array = function(x) {
  testthat::expect_is(x, "NiftiArray")
  testthat::expect_is(x, "HDF5Array")
  testthat::expect_is(x, "DelayedArray")
}
testthat::test_that("Writing an Array", {
  res = writeNiftiArray(img)
  check_array(res)
})

testthat::test_that("Writing and Reading an Array", {

  res = writeNiftiArray(img, filepath = h5_fname)
  testthat::expect_true(file.exists(h5_fname))
  check_array(res)

  hdr = nifti_header(res)
  testthat::expect_is(hdr, "niftiHeader")
  rm(res)


  pd = pixdim(hdr)
  testthat::expect_true(all(pd == 2.5))
  testthat::expect_equal(hdr$qoffset_x, 122.033897399902)

  res = NiftiArray(h5_fname)
  check_array(res)
})

testthat::test_that("Writing and Reading just nifti file on disk", {

  res = writeNiftiArray(nii_fname)
  check_array(res)

  hdr = nifti_header(res)
  testthat::expect_is(hdr, "niftiHeader")
  rm(res)

  res = NiftiArray(nii_fname)
  check_array(res)
})



