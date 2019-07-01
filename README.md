
<!-- README.md is generated from README.Rmd. Please edit that file -->

The `NiftiArray` package is under development please check back later\!

# NiftiArray

<!-- badges: start -->

muschellij2 badges: [![Build
Status](https://travis-ci.com/muschellij2/NiftiArray.svg?branch=master)](https://travis-ci.com/muschellij2/NiftiArray)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/muschellij2/NiftiArray?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/NiftiArray)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/NiftiArray)](https://cran.r-project.org/package=NiftiArray)
[![Codecov test
coverage](https://codecov.io/gh/muschellij2/NiftiArray/branch/master/graph/badge.svg)](https://codecov.io/gh/muschellij2/NiftiArray?branch=master)
[![Coverage
Status](https://img.shields.io/coveralls/muschellij2/NiftiArray.svg)](https://coveralls.io/r/muschellij2/NiftiArray?branch=master)

avalcarcel9 badges: [![Build
Status](https://travis-ci.org/avalcarcel9/NiftiArray.svg?branch=master)](https://travis-ci.org/avalcarcel9/NiftiArray)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/avalcarcel9/NiftiArray?branch=master&svg=true)](https://ci.appveyor.com/project/avalcarcel9/NiftiArray)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/NiftiArray)](https://cran.r-project.org/package=NiftiArray)
[![Coverage
Status](https://img.shields.io/coveralls/avalcarcel9/NiftiArray.svg)](https://coveralls.io/r/avalcarcel9/NiftiArray?branch=master)
<!-- badges: end -->

The goal of `NiftiArray` is to allow for memory efficient fast random
access of NIfTI objects. `NiftiArray` is an R package that allows for
convenient and memory-efficient containers for on-disk representation of
NIfTI objects. We allow for `DelayedArray` extensions and support all
operations supported by DelayedArray objects. These operations can be
either delayed or block-processed.

## Installation

You can install the development version of `NiftiArray` from
[GitHub](https://github.com/) with:

``` r
# install.packages('remotes')
remotes::install_github("muschellij2/NiftiArray")
```

We are working to get a stable version on
[Neuroconductor](www.neuroconductor.org).

## Example

![](https://media.giphy.com/media/1ken0zzzL79NPy3QZj/giphy.gif)

Here we use the example image from `RNifti`. We use the
`writeNiftiArray` function to create a `NiftiArray` object:

``` r
library(NiftiArray)
nii_fname = system.file("extdata", "example.nii.gz", package = "RNifti")
res = writeNiftiArray(nii_fname)
class(res)
#> [1] "NiftiArray"
#> attr(,"package")
#> [1] "NiftiArray"
dim(res)
#> [1] 96 96 60
res
#> <96 x 96 x 60> NiftiArray object of type "integer":
#> ,,1
#>        [,1]  [,2]  [,3]  [,4] ... [,93] [,94] [,95] [,96]
#>  [1,]     0     0     0     0   .     0     0     0     0
#>  [2,]     0     0     0     0   .     0     0     0     0
#>   ...     .     .     .     .   .     .     .     .     .
#> [95,]     0     0     0     0   .     0     0     0     0
#> [96,]     0     0     0     0   .     0     0     0     0
#> 
#> ...
#> 
#> ,,60
#>        [,1]  [,2]  [,3]  [,4] ... [,93] [,94] [,95] [,96]
#>  [1,]     0     0     0     0   .     0     0     0     0
#>  [2,]     0     0     0     0   .     0     0     0     0
#>   ...     .     .     .     .   .     .     .     .     .
#> [95,]     0     0     0     0   .     0     0     0     0
#> [96,]     0     0     0     0   .     0     0     0     0
```

We can see the file on disk that was written out:

``` r
res@seed@filepath
#> [1] "/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/RtmpyVro02/file794dc397671.h5"
```

We see that the object is a low-memory `DelayedArray`:

``` r
object.size(res)
#> 8688 bytes
```

You can also simply use the `NiftiArray` function of the NIfTI filename
to create the array:

``` r
res = NiftiArray(nii_fname)
```

We see the header information is encoded in the `seed` slot of the
object, which can be accessed using the `nifti_header` function:

``` r
nifti_header(res)
#> NIfTI-1 header
#>     sizeof_hdr: 348
#>       dim_info: 0
#>            dim: 3  96  96  60  1  1  1  1
#>      intent_p1: 0
#>      intent_p2: 0
#>      intent_p3: 0
#>    intent_code: 0
#>       datatype: 8
#>         bitpix: 32
#>    slice_start: 0
#>         pixdim: -1.0  2.5  2.5  2.5  0.0  0.0  0.0  0.0
#>     vox_offset: 352
#>      scl_slope: 0
#>      scl_inter: 0
#>      slice_end: 0
#>     slice_code: 0
#>     xyzt_units: 10
#>        cal_max: 2503
#>        cal_min: 0
#> slice_duration: 0
#>        toffset: 0
#>        descrip: TractoR NIfTI writer v3.0.0
#>       aux_file: 
#>     qform_code: 2
#>     sform_code: 2
#>      quatern_b: 0
#>      quatern_c: 1
#>      quatern_d: 0
#>      qoffset_x: 122.0339
#>      qoffset_y: -95.18523
#>      qoffset_z: -55.03814
#>         srow_x: -2.5000  0.0000  0.0000  122.0339
#>         srow_y: 0.00000  2.50000  0.00000  -95.18523
#>         srow_z: 0.00000  0.00000  2.50000  -55.03814
#>    intent_name: 
#>          magic: n+1
```

### Creating a Matrix

``` r
mat = as(res, "NiftiMatrix")
mat
#> <552960 x 1> NiftiMatrix object of type "integer":
#>           [,1]
#>      [1,]    0
#>      [2,]    0
#>      [3,]    0
#>      [4,]    0
#>      [5,]    0
#>       ...    .
#> [552956,]    0
#> [552957,]    0
#> [552958,]    0
#> [552959,]    0
#> [552960,]    0
```

Now that the image is a matrix, we can bind the columns together,

``` r
mat = DelayedArray::acbind(mat, mat, mat, mat)
testthat::expect_is(mat, "DelayedMatrix")
object.size(mat)
#> 33672 bytes
```

Now that we have the data in a `DelayedMatrix` class, we can use the
package `DelayedMatrixStats` package that calls the `matrixStats`
package for quick operations:

``` r
vec_result = DelayedMatrixStats::rowMedians(mat)
head(vec_result)
#> [1] 0 0 0 0 0 0
```

Turning the output back into a `NiftiArray`, we have to pass in the
`header` argument, passing in the correct header information. We can
either create the `NiftiArray` output by creating a matrix, then the
`NiftiMatrix`, then the `NiftiArray`.

``` r
res_mat = matrix(vec_result, ncol = 1)
res_mat = as(res_mat, "NiftiMatrix")
hdr = nifti_header(res)
res_mat = writeNiftiArray(res_mat, header = hdr)
class(res_mat)
#> [1] "NiftiMatrix"
#> attr(,"package")
#> [1] "NiftiArray"
res_arr = as(res_mat, "NiftiArray")
```

Or we can create an array and then making the `NiftiArray`:

``` r
arr = array(vec_result, dim = dim(res) )
hdr = nifti_header(res)
res_arr = writeNiftiArray(arr, header = hdr)
res_arr
#> <96 x 96 x 60> NiftiArray object of type "double":
#> ,,1
#>        [,1]  [,2]  [,3] ... [,95] [,96]
#>  [1,]     0     0     0   .     0     0
#>  [2,]     0     0     0   .     0     0
#>   ...     .     .     .   .     .     .
#> [95,]     0     0     0   .     0     0
#> [96,]     0     0     0   .     0     0
#> 
#> ...
#> 
#> ,,60
#>        [,1]  [,2]  [,3] ... [,95] [,96]
#>  [1,]     0     0     0   .     0     0
#>  [2,]     0     0     0   .     0     0
#>   ...     .     .     .   .     .     .
#> [95,]     0     0     0   .     0     0
#> [96,]     0     0     0   .     0     0
nifti_header(res_arr)
#> NIfTI-1 header
#>     sizeof_hdr: 348
#>       dim_info: 0
#>            dim: 3  96  96  60  1  1  1  1
#>      intent_p1: 0
#>      intent_p2: 0
#>      intent_p3: 0
#>    intent_code: 0
#>       datatype: 8
#>         bitpix: 32
#>    slice_start: 0
#>         pixdim: -1.0  2.5  2.5  2.5  0.0  0.0  0.0  0.0
#>     vox_offset: 352
#>      scl_slope: 0
#>      scl_inter: 0
#>      slice_end: 0
#>     slice_code: 0
#>     xyzt_units: 10
#>        cal_max: 2503
#>        cal_min: 0
#> slice_duration: 0
#>        toffset: 0
#>        descrip: TractoR NIfTI writer v3.0.0
#>       aux_file: 
#>     qform_code: 2
#>     sform_code: 2
#>      quatern_b: 0
#>      quatern_c: 1
#>      quatern_d: 0
#>      qoffset_x: 122.0339
#>      qoffset_y: -95.18523
#>      qoffset_z: -55.03814
#>         srow_x: -2.5000  0.0000  0.0000  122.0339
#>         srow_y: 0.00000  2.50000  0.00000  -95.18523
#>         srow_z: 0.00000  0.00000  2.50000  -55.03814
#>    intent_name: 
#>          magic: n+1
```

### Converting back to niftiImage

We can return a `niftiImage` from the `NiftiArray` object, as follows:

``` r
nii = as(res_arr, "niftiImage")
nii
#> Image array of mode "double" (4.2 Mb)
#> - 96 x 96 x 60 voxels
#> - 2.5 x 2.5 x 2.5 mm per voxel
```
