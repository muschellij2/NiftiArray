setClass("ReshapedNiftiArraySeed",
         contains = c("ReshapedHDF5ArraySeed"),
         slots = c(
           sizeof_hdr = "integer",
           dim_info = "integer",
           dim_ = "integer",
           intent_p1 = "numeric",
           intent_p2 = "numeric",
           intent_p3 = "numeric",
           intent_code = "integer",
           datatype = "integer",
           bitpix = "integer",
           slice_start = "integer",
           pixdim = "numeric",
           vox_offset = "numeric",
           scl_slope = "numeric",
           scl_inter = "numeric",
           slice_end = "integer",
           slice_code = "integer",
           xyzt_units = "integer",
           cal_max = "numeric",
           cal_min = "numeric",
           slice_duration = "numeric",
           toffset = "numeric",
           descrip = "character",
           aux_file = "character",
           qform_code = "integer",
           sform_code = "integer",
           quatern_b = "numeric",
           quatern_c = "numeric",
           quatern_d = "numeric",
           qoffset_x = "numeric",
           qoffset_y = "numeric",
           qoffset_z = "numeric",
           srow_x = "numeric",
           srow_y = "numeric",
           srow_z = "numeric",
           intent_name = "character",
           magic = "character"
         )
)

setClass("ReshapedNiftiArray",
         contains = "ReshapedHDF5Array"
)


#' Virtually reshaped Nifti images as DelayedArray objects
#'
#' @inheritParams NiftiArraySeed
#' @param dim A vector of dimensions that describes the virtual
#' reshaping i.e. the reshaping that is virtually applied upfront
#' to the HDF5 dataset when the ReshapedHDF5Array object gets constructed.
#' See [HDF5Array::ReshapedHDF5Array].
#' @return An object of class `ReshapedNiftiArraySeed`
#' @export
#' @examples
#' nii_fname = system.file("extdata",
#'                         "example.nii.gz", package = "RNifti")
#' res = NiftiArray::NiftiArray(nii_fname)
#' dim(res) = c(dim(res), 1)
#' res = writeNiftiArray(res)
#' A <- ReshapedNiftiArray(
#'   filepath = res@seed@filepath,
#'   name = res@seed@name,
#'   dim=c(prod(dim(res)[1:3]), 1))
ReshapedNiftiArraySeed <- function(filepath, name = "image",
                                   header_name = "hdr", dim,
                                   type = NA, header = NULL){


  seed <- NiftiArraySeed(filepath, name = name,
                         header_name = header_name,
                         header = header, type = type)
  hdr = nifti_header(seed)
  reshaped_dim <- as.integer(dim)
  collapse_along <- HDF5Array:::find_dims_to_collapse(reshaped_dim, seed@dim)
  if (is.null(seed@chunkdim)) {
    reshaped_chunkdim <- NULL
  } else {
    reshaped_chunkdim <-  HDF5Array:::collapse_dims(seed@chunkdim,
                                                    collapse_along)
    reshaped_chunkdim <- as.integer(reshaped_chunkdim)
  }
  args = list(reshaped_chunkdim = reshaped_chunkdim,
              reshaped_dim = reshaped_dim)
  .niftiArraySeed_from_HDF5ArraySeed(seed = seed,
                                     header_name = header_name,
                                     header = hdr,
                                     args =  args,
                                     seed_type = "ReshapedNiftiArraySeed")

  # new2("ReshapedNiftiArraySeed", seed,
  #      reshaped_dim = reshaped_dim,
  #      reshaped_chunkdim = reshaped_chunkdim)
}




### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###
setMethod("DelayedArray", "ReshapedNiftiArraySeed",
          function(seed) DelayedArray::new_DelayedArray(
            seed,
            Class = "ReshapedNiftiArray")
)


#' @export
#' @rdname ReshapedNiftiArraySeed
ReshapedNiftiArray <- function(
  filepath, name = "image",
  header_name = "hdr", dim,
  type = NA, header = NULL){

  if (is(filepath, "NiftiArraySeed")) {
    seed <- filepath
  } else {
    seed <- ReshapedNiftiArraySeed(
      filepath, name = name,
      dim = dim,
      header_name = header_name, type = type,
      header = header)
  }
  DelayedArray::DelayedArray(seed)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### ReshapedHDF5Matrix objects
###

setClass("ReshapedNiftiMatrix", contains = c("ReshapedNiftiArray"))

### Required for DelayedArray internal business.
setMethod("matrixClass", "ReshapedNiftiArray", function(x) "ReshapedHDF5Matrix")


#' @rdname NiftiArray
#' @aliases coerce,ReshapedNiftiArray,ReshapedNiftiMatrix-method
#' @export
#' @name coerce
setAs("ReshapedNiftiArray", "ReshapedNiftiMatrix",
      function(from) new("ReshapedNiftiMatrix", from)
)

#' @rdname NiftiArray
#' @aliases coerce,ReshapedNiftiMatrix,ReshapedNiftiArray-method
#' @export
#' @name coerce
setAs("ReshapedNiftiMatrix", "ReshapedNiftiArray", function(from) from)  # no-op

#' @rdname NiftiArray
#' @aliases coerce,ANY,ReshapedNiftiMatrix-method
#' @export
#' @name coerce
setAs("ANY", "ReshapedNiftiMatrix",
      function(from) as(as(from, "ReshapedNiftiArray"), "ReshapedNiftiMatrix")
)

#' @rdname nifti_header
#' @export
#' @aliases nifti_header,ReshapedNiftiArray-method
setMethod("nifti_header", "ReshapedNiftiArray", function(image) {
  nifti_header(image@seed)
})

#' @rdname nifti_header
#' @export
#' @aliases nifti_header,ReshapedNiftiArraySeed-method
setMethod("nifti_header", "ReshapedNiftiArraySeed", .nifti_header_from_seed)
