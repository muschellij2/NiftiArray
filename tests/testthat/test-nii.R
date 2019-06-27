testthat::context("Trying to make NiftiArray objects")
nii_fname = system.file("extdata", "example.nii.gz", package = "RNifti")
h5_fname = tempfile(fileext = ".h5")
img = RNifti::readNifti(nii_fname)

testthat::test_that("Writing an Array", {
  writeNiftiArray(img)
  name = "image"
  header_name = "hdr"
  res = NiftiArray(filepath)
  writeNiftiArray(img)

  all.equal(nifti_header(img), nifti_header(res))
  niftiHeader(res)
  pixdim(res)

})
