#' "niftiImage" class
#'
#' @name niftiImage-class
#' @aliases niftiImage
#' @family niftiImage
#'
setOldClass("niftiImage")



.as_NiftiArray <- function(from) writeNiftiArray(from)  # write to current dump

#' @aliases coerce,ANY,NiftiArray-method
#' @rdname NiftiArray
#' @name coerce
#' @export
setAs("ANY", "NiftiArray", .as_NiftiArray)

#' @aliases coerce,DelayedArray,NiftiArray-method
#' @rdname NiftiArray
#' @name coerce
#' @export
setAs("DelayedArray", "NiftiArray", .as_NiftiArray)

#' @aliases coerce,DelayedMatrix,NiftiMatrix-method
#' @rdname NiftiArray
#' @name coerce
#' @export
setAs("DelayedMatrix", "NiftiMatrix", .as_NiftiArray)


#' @importMethodsFrom DelayedArray matrixClass
#' @rdname NiftiMatrix
#' @aliases matrixClass,NiftiArray-method
#' @name matrixClass
#' @param x Typically a DelayedArray object.
setMethod("matrixClass", "NiftiArray", function(x) "NiftiMatrix")

#' @aliases coerce,NiftiArray,NiftiMatrix-method
#' @importMethodsFrom methods coerce
#' @rdname NiftiArray
#' @name coerce
#' @export
setAs("NiftiArray", "NiftiMatrix", function(from) {
  dfrom = dim(from)
  nd = length(dfrom)
  if (nd > 2) {
    hdr = nifti_header(from)
    mat = matrix(from, ncol = 1)
    writeNiftiArray(mat, header = hdr)
  } else {
    new("NiftiMatrix", from)
  }
})

#' @aliases coerce,NiftiArray,niftiImage-method
#' @importMethodsFrom methods coerce
#' @rdname NiftiArray
#' @name coerce
#' @export
setAs("NiftiArray", "niftiImage", function(from) {
  hdr = nifti_header(from)
  out_img = RNifti::updateNifti(as.array(from), template = hdr)
  out_img
})

#' @aliases coerce,NiftiMatrix,niftiImage-method
#' @importMethodsFrom methods coerce
#' @rdname NiftiArray
#' @name coerce
#' @export
setAs("NiftiMatrix", "niftiImage", function(from) {
  as(as(from, "NiftiArray"), "niftiImage")
})


#' @aliases coerce,NiftiMatrix,NiftiArray-method
#' @rdname NiftiArray
#' @export
#' @name coerce
setAs("NiftiMatrix", "NiftiArray", function(from) {
  hdr = nifti_header(from)
  d = hdr$dim
  d = d[ 2:(2 + d[1] - 1)]
  mat = array(from, dim = d)
  writeNiftiArray(mat, header = hdr)
})  # no-op

#' @rdname NiftiArray
#' @aliases coerce,ANY,NiftiMatrix-method
#' @export
#' @name coerce
setAs("ANY", "NiftiMatrix",
      function(from) as(as(from, "NiftiArray"), "NiftiMatrix")
)
