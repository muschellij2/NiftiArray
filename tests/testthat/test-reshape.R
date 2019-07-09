testthat::test_that(desc = "Reshaping works in 3D", {
    nii_fname = system.file("extdata",
                        "example.nii.gz", package = "RNifti")
res = NiftiArray::NiftiArray(nii_fname)
dim(res) = c(dim(res), 1)
res = writeNiftiArray(res)
from = res
A <- ReshapedNiftiArray(
    filepath = from@seed@filepath,
    name = from@seed@name,
    dim=c(prod(dim(from)[1:3]), 1))
nifti_header(A)
aa = DelayedArray::acbind(A, A, A, A, A)
nifti_header(aa)

med = DelayedMatrixStats::rowMedians(aa)
})

testthat::test_that(desc = "Reshaping works in 4D", {

    nii_fname = system.file("extdata",
                            "example_4d.nii.gz", package = "RNifti")
    from = NiftiArray::NiftiArray(nii_fname)

    A <- ReshapedNiftiArray(filepath = from@seed@filepath,
                            name = from@seed@name,
                            dim=c(prod(dim(from)[1:3]), dim(from)[4]))
    med = DelayedMatrixStats::rowMedians(A)

})
