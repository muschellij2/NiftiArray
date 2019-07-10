has_nifti_header = function(x) {
  xx = is(x, "NiftiArray") | is(x, "NiftiArraySeed")
  xx = xx | is(x, "ReshapedNiftiArraySeed") | is(x, "ReshapedNiftiArray")
  xx
}

.nifti_seed = function(image, caller = "DelayedArray", stop_on_fail = TRUE) {
  seeds = DelayedArray::seedApply(image, identity)
  if (length(seeds) == 0) {
    return(list())
  }
  if (!is.null(seeds)) {
    nii_seeds = vapply(seeds, has_nifti_header, FUN.VALUE = logical(1))
    if (!any(nii_seeds)) {
      if (stop_on_fail) {
        stop(paste0("No seeds in ", caller, " are NiftiArray ",
                    "or NiftiArraySeed"))
      }
      return(list())
    }
    seeds = seeds[ nii_seeds ]
    seeds = seeds[[ length(seeds) ]]
  } else {
    seeds = slot(image, "seed")
  }
  seeds
}

#' Dump a NIfTI header
#'
#' @rdname nifti_header
#' @aliases nifti_header,NiftiArraySeed-method
#' @param image An image or NiftiArray object.
#'
#' @return A list of class `niftiHeader`, which has
#' the header information.
#' @export
#' @examples
#' nii_fname = system.file("extdata",
#' "example.nii.gz", package = "RNifti")
#' nifti_header(nii_fname)
#' res = writeNiftiArray(nii_fname)
#' nifti_header(res)
setGeneric("nifti_header", function(image) {
  standardGeneric("nifti_header")
})

#' @rdname nifti_header
#' @export
#' @aliases nifti_header,NiftiArray-method
setMethod("nifti_header", "NiftiArray", function(image) {
  nifti_header(image@seed)
})

#' @rdname nifti_header
#' @export
#' @aliases nifti_header,NiftiArrayList-method
setMethod("nifti_header", "NiftiArrayList", function(image) {
  nii_seeds = vapply(image, has_nifti_header, FUN.VALUE = logical(1))
  if (!any(nii_seeds)) {
    stop("No images in NiftiArrayList are NiftiArray or NiftiArraySeed")
  }
  image = image[ nii_seeds ]
  image = image[[ length(image) ]]
  image
  nifti_header(image)
})



#' @rdname nifti_header
#' @export
#' @aliases nifti_header,DelayedArray-method
setMethod("nifti_header", "DelayedArray", function(image) {
  seeds = .nifti_seed(image, caller = "DelayedArray")
  nifti_header(seeds)
})

#' @rdname nifti_header
#' @export
#' @aliases nifti_header,HDF5Array-method
setMethod("nifti_header", "HDF5Array", function(image) {
  seeds = .nifti_seed(image, caller = "HDF5Array", stop_on_fail = FALSE)
  if (has_nifti_header(seeds)) {
    return(nifti_header(seeds))
  }
  warning("No header available, giving default header")
  RNifti::niftiHeader()
})

#' @rdname nifti_header
#' @export
#' @aliases nifti_header,HDF5ArraySeed-method
setMethod("nifti_header", "HDF5ArraySeed", function(image) {
  warning("No header available, giving default header")
  RNifti::niftiHeader()
})

#' @rdname nifti_header
#' @export
#' @aliases nifti_header,ANY-method
#' @importFrom RNifti niftiHeader
setMethod("nifti_header", "ANY", function(image) {
  RNifti::niftiHeader(image)
})

.nifti_header_from_seed = function(image) {
  out = list(
    sizeof_hdr = image@sizeof_hdr,
    dim_info = image@dim_info,
    dim = image@dim_,
    intent_p1 = image@intent_p1,
    intent_p2 = image@intent_p2,
    intent_p3 = image@intent_p3,
    intent_code = image@intent_code,
    datatype = image@datatype,
    bitpix = image@bitpix,
    slice_start = image@slice_start,
    pixdim = image@pixdim,
    vox_offset = image@vox_offset,
    scl_slope = image@scl_slope,
    scl_inter = image@scl_inter,
    slice_end = image@slice_end,
    slice_code = image@slice_code,
    xyzt_units = image@xyzt_units,
    cal_max = image@cal_max,
    cal_min = image@cal_min,
    slice_duration = image@slice_duration,
    toffset = image@toffset,
    descrip = image@descrip,
    aux_file = image@aux_file,
    qform_code = image@qform_code,
    sform_code = image@sform_code,
    quatern_b = image@quatern_b,
    quatern_c = image@quatern_c,
    quatern_d = image@quatern_d,
    qoffset_x = image@qoffset_x,
    qoffset_y = image@qoffset_y,
    qoffset_z = image@qoffset_z,
    srow_x = image@srow_x,
    srow_y = image@srow_y,
    srow_z = image@srow_z,
    intent_name = image@intent_name,
    magic = image@magic)
  ndim = length(dim(image))
  attr(out, "imagedim") = dim(image)
  attr(out, "pixdim") = image@pixdim[2:(2 + ndim - 1)]
  attr(out, "pixunits") = pixunits(out$xyzt_units)
  class(out) = "niftiHeader"
  out
}

#' @rdname nifti_header
#' @aliases nifti_header,NiftiArraySeed-method
#' @export
setMethod("nifti_header", "NiftiArraySeed", .nifti_header_from_seed)


# setMethod("sizeof_hdr", "NiftiArraySeed", function(object) object@sizeof_hdr)
# setMethod("dim_info", "NiftiArraySeed", function(object) object@dim_info)
# setMethod("dim_", "NiftiArraySeed", function(object) object@dim_)
# setMethod("intent_p1", "NiftiArraySeed", function(object) object@intent_p1)
# setMethod("intent_p2", "NiftiArraySeed", function(object) object@intent_p2)
# setMethod("intent_p3", "NiftiArraySeed", function(object) object@intent_p3)
# setMethod("intent_code", "NiftiArraySeed",
# function(object) object@intent_code)
# setMethod("datatype", "NiftiArraySeed", function(object) object@datatype)
# setMethod("bitpix", "NiftiArraySeed", function(object) object@bitpix)
# setMethod("slice_start", "NiftiArraySeed",
# function(object) object@slice_start)
# setMethod("pixdim", "NiftiArraySeed", function(object) object@pixdim)
# setMethod("vox_offset", "NiftiArraySeed", function(object) object@vox_offset)
# setMethod("scl_slope", "NiftiArraySeed", function(object) object@scl_slope)
# setMethod("scl_inter", "NiftiArraySeed", function(object) object@scl_inter)
# setMethod("slice_end", "NiftiArraySeed", function(object) object@slice_end)
# setMethod("slice_code", "NiftiArraySeed", function(object) object@slice_code)
# setMethod("xyzt_units", "NiftiArraySeed", function(object) object@xyzt_units)
# setMethod("cal_max", "NiftiArraySeed", function(object) object@cal_max)
# setMethod("cal_min", "NiftiArraySeed", function(object) object@cal_min)
# setMethod("slice_duration", "NiftiArraySeed",
# function(object) object@slice_duration)
# setMethod("toffset", "NiftiArraySeed", function(object) object@toffset)
# setMethod("descrip", "NiftiArraySeed", function(object) object@descrip)
# setMethod("aux_file", "NiftiArraySeed", function(object) object@aux_file)
# setMethod("qform_code", "NiftiArraySeed", function(object) object@qform_code)
# setMethod("sform_code", "NiftiArraySeed", function(object) object@sform_code)
# setMethod("quatern_b", "NiftiArraySeed", function(object) object@quatern_b)
# setMethod("quatern_c", "NiftiArraySeed", function(object) object@quatern_c)
# setMethod("quatern_d", "NiftiArraySeed", function(object) object@quatern_d)
# setMethod("qoffset_x", "NiftiArraySeed", function(object) object@qoffset_x)
# setMethod("qoffset_y", "NiftiArraySeed", function(object) object@qoffset_y)
# setMethod("qoffset_z", "NiftiArraySeed", function(object) object@qoffset_z)
# setMethod("srow_x", "NiftiArraySeed", function(object) object@srow_x)
# setMethod("srow_y", "NiftiArraySeed", function(object) object@srow_y)
# setMethod("srow_z", "NiftiArraySeed", function(object) object@srow_z)
# setMethod("intent_name", "NiftiArraySeed",
# function(object) object@intent_name)
# setMethod("magic", "NiftiArraySeed", function(object) object@magic)
