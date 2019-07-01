testthat::context("Operations of NiftiArray")

nii_fname = system.file("extdata", "example.nii.gz",
                        package = "RNifti")
h5_fname = tempfile(fileext = ".h5")
img = RNifti::readNifti(nii_fname)
img_hdr = nifti_header(img)
arr_list = NiftiArrayList(rep(nii_fname, 5))

check_array = function(x) {
  testthat::expect_is(x, "NiftiArray")
  testthat::expect_is(x, "HDF5Array")
  testthat::expect_is(x, "DelayedArray")
}


testthat::test_that("Operations NiftiArrayList give header", {

  res = Reduce("+", arr_list)
  testthat::expect_is(res, "DelayedArray")
  testthat::expect_is(as(res, "NiftiArray"), "NiftiArray")

  hdr = nifti_header(res)
  testthat::expect_is(hdr, "niftiHeader")
  writeNiftiArray(res)

  res = res / 4
  testthat::expect_is(res, "DelayedArray")
  check_array(as(res, "NiftiArray"))

  res = log(res + 1)
  testthat::expect_is(res, "DelayedArray")
  check_array(as(res, "NiftiArray"))

  sub = res[1:30, 1:30, 1:30]
  testthat::expect_is(sub, "DelayedArray")
  check_array(as(sub, "NiftiArray"))

  sub = DelayedArray::aperm(res, c(3, 1, 2))
  testthat::expect_is(sub, "DelayedArray")
  check_array(as(sub, "NiftiArray"))

  res[1:30, 1:30, 1:30] = 1
  testthat::expect_is(res, "DelayedArray")
  check_array(as(res, "NiftiArray"))

})


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
