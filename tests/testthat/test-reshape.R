nii_fname = system.file("extdata",
                        "example.nii.gz", package = "RNifti")
res = NiftiArray::NiftiArray(nii_fname)

testthat::test_that(desc = "Reshaping works in 3D", {

    dim(res) = c(dim(res), 1)
    res = writeNiftiArray(res)
    from = res
    A <- ReshapedNiftiArray(
        filepath = from@seed@filepath,
        name = from@seed@name,
        dim = c(prod(dim(from)[1:3]), 1))
    testthat::expect_is(nifti_header(A), "niftiHeader")
    aa = DelayedArray::acbind(A, A, A, A, A)
    hdr = nifti_header(aa)
    testthat::expect_is(hdr, "niftiHeader")
    testthat::expect_equal(hdr$qoffset_x, 122.033897399902)

    med = DelayedMatrixStats::rowMedians(aa)
})

nii_fname = system.file("extdata",
                        "example_4d.nii.gz", package = "RNifti")
from = NiftiArray::NiftiArray(nii_fname)

testthat::test_that(desc = "Reshaping works in 4D", {


    A <- ReshapedNiftiArray(filepath = from@seed@filepath,
                            name = from@seed@name,
                            dim=c(prod(dim(from)[1:3]), dim(from)[4]))
    testthat::expect_equal(dim(A), c(552960L, 3L))
    aa = DelayedArray::acbind(A, A, A, A, A)
    hdr = nifti_header(aa)
    testthat::expect_is(hdr, "niftiHeader")
    testthat::expect_equal(hdr$qoffset_x, 122.033897399902)
    med = DelayedMatrixStats::rowMedians(A)

})
