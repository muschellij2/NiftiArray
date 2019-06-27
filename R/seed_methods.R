#' Dump a NIfTI header
#'
#' @rdname nifti_header
#' @aliases nifti_header,NiftiArraySeed-method
#' @param image An image or NiftiArray object.
#' @export
#' @examples
#' nii_fname = system.file("extdata", "example.nii.gz", package = "RNifti")
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
#' @aliases nifti_header,ANY-method
setMethod("nifti_header", "ANY", function(image) {
  RNifti::niftiHeader(image)
})


#' @rdname nifti_header
#' @aliases nifti_header,NiftiArraySeed-method
#' @import RNifti
#' @export
setMethod("nifti_header", "NiftiArraySeed", function(image) {
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
  class(out) = "niftiHeader"
  # aa = attributes(RNifti::niftiHeader(out))
  # attr(out, "pixdim") = attr(aa, "pixdim")
  # attr(out, "imagedim") = attr(aa, "imagedim")
  # attr(out, "pixunits") = attr(aa, "pixunits")
  out
})

# setMethod("sizeof_hdr", "NiftiArraySeed", function(object) object@sizeof_hdr)
# setMethod("dim_info", "NiftiArraySeed", function(object) object@dim_info)
# setMethod("dim_", "NiftiArraySeed", function(object) object@dim_)
# setMethod("intent_p1", "NiftiArraySeed", function(object) object@intent_p1)
# setMethod("intent_p2", "NiftiArraySeed", function(object) object@intent_p2)
# setMethod("intent_p3", "NiftiArraySeed", function(object) object@intent_p3)
# setMethod("intent_code", "NiftiArraySeed", function(object) object@intent_code)
# setMethod("datatype", "NiftiArraySeed", function(object) object@datatype)
# setMethod("bitpix", "NiftiArraySeed", function(object) object@bitpix)
# setMethod("slice_start", "NiftiArraySeed", function(object) object@slice_start)
# setMethod("pixdim", "NiftiArraySeed", function(object) object@pixdim)
# setMethod("vox_offset", "NiftiArraySeed", function(object) object@vox_offset)
# setMethod("scl_slope", "NiftiArraySeed", function(object) object@scl_slope)
# setMethod("scl_inter", "NiftiArraySeed", function(object) object@scl_inter)
# setMethod("slice_end", "NiftiArraySeed", function(object) object@slice_end)
# setMethod("slice_code", "NiftiArraySeed", function(object) object@slice_code)
# setMethod("xyzt_units", "NiftiArraySeed", function(object) object@xyzt_units)
# setMethod("cal_max", "NiftiArraySeed", function(object) object@cal_max)
# setMethod("cal_min", "NiftiArraySeed", function(object) object@cal_min)
# setMethod("slice_duration", "NiftiArraySeed", function(object) object@slice_duration)
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
# setMethod("intent_name", "NiftiArraySeed", function(object) object@intent_name)
# setMethod("magic", "NiftiArraySeed", function(object) object@magic)
