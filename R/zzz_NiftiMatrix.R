#' "niftiImage" class
#'
#' @name niftiImage-class
#' @aliases niftiImage
#' @family niftiImage
#'
setOldClass("niftiImage")



# write to current dump
.as_NiftiArray <- function(from) writeNiftiArray(from)

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
setAs(
  "DelayedMatrix", "NiftiMatrix",
  function(from) as(as(from, "NiftiArray"), "NiftiMatrix"))


#' @aliases coerce,HDF5Array,NiftiArray-method
#' @rdname NiftiArray
#' @name coerce
#' @export
setAs("HDF5Array", "NiftiArray", .as_NiftiArray)

#' @aliases coerce,HDF5Array,NiftiMatrix-method
#' @rdname NiftiArray
#' @name coerce
#' @export
setAs(
  "HDF5Array",
  "NiftiMatrix",
  function(from) as(as(from, "NiftiArray"), "NiftiMatrix"))

#' @aliases coerce,HDF5Matrix,NiftiMatrix-method
#' @rdname NiftiArray
#' @name coerce
#' @export
setAs(
  "HDF5Matrix", "NiftiMatrix",
  function(from) as(as(from, "NiftiArray"), "NiftiMatrix"))


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
  if (nd > 4) {
    stop(paste0("NiftiMatrix from NiftiArray not ",
                "defined for > 4 dimensions!"))
  }
  dfrom = c(dfrom, rep(1, 4 - nd))
  if (nd > 2) {
    hdr = nifti_header(from)
    mat = matrix(from, ncol = dfrom[4])
    writeNiftiArray(mat, header = hdr)
  } else {
    new("NiftiMatrix", from)
  }
})


#' @aliases coerce,NiftiArrayList,NiftiMatrix-method
#' @rdname NiftiArray
#' @name coerce
#' @export
setAs("NiftiArrayList", "NiftiMatrix", function(from) {
  verbose = attr(from, "verbose")
  if (is.null(verbose)) {
    verbose = FALSE
  }
  applier = lapply
  if (verbose) {
    if (requireNamespace("pbapply", quietly = TRUE)) {
      applier = pbapply::pblapply
    }
  }
  from = applier(from, function(x) {
    as(x, "NiftiMatrix")
  })
  hdr = nifti_header(from[[1]])
  from = do.call(DelayedArray::acbind, args = from)
  writeNiftiArray(from, header = hdr)
})


#' @aliases coerce,numeric,NiftiMatrix-method
#' @rdname NiftiArray
#' @name coerce
#' @export
setAs("numeric", "NiftiMatrix", function(from) {
  from = matrix(from, ncol = 1)
  as(as(from, "NiftiArray"), "NiftiMatrix")
})

#' @rdname NiftiArray
#' @aliases coerce,numeric,NiftiArray-method
#' @export
#' @name coerce
setAs("numeric", "NiftiArray",
      function(from) as(as(from, "NiftiMatrix"), "NiftiArray")
)

#' @aliases coerce,NiftiArray,niftiImage-method
#' @rdname NiftiArray
#' @name coerce
#' @importFrom RNifti updateNifti
#' @export
setAs("NiftiArray", "niftiImage", function(from) {
  hdr = nifti_header(from)
  out_img = RNifti::updateNifti(as.array(from), template = hdr)
  out_img
})

#' @aliases coerce,NiftiMatrix,niftiImage-method
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

#' @rdname NiftiArray
#' @aliases coerce,NiftiArrayList,NiftiArray-method
#' @export
#' @name coerce
setAs(
  "NiftiArrayList", "NiftiArray",
  function(from) {
    ndims = lapply(from, dim)
    ndims = vapply(ndims, length, FUN.VALUE = integer(1))
    stopifnot(all(ndims == ndims[1]))
    ndims = unique(ndims)
    hdr = nifti_header(from[[1]])

    verbose = attr(from, "verbose")
    if (is.null(verbose)) {
      verbose = FALSE
    }
    applier = lapply
    if (verbose) {
      if (requireNamespace("pbapply", quietly = TRUE)) {
        applier = pbapply::pblapply
      }
    }

    # Adapted from
    # https://support.bioconductor.org/p/107051/
    from = applier(from, function(x) {
      dim(x) = c(dim(x), 1)
      x = DelayedArray::aperm(x, perm = (ndims + 1):1)
      x
    })
    # 1 for
    if (verbose) {
      message("Binding data together")
    }
    res = do.call(DelayedArray::arbind, args = from)
    res = aperm(res, (ndims + 1):1)
    hdr$dim[ndims + 1 + 1] = dim(res)[ndims + 1]
    hdr$pixdim[ndims + 1 + 1] = 1
    if (verbose) {
      message("Running writeNiftiArray")
    }
    res = writeNiftiArray(res, header = hdr, verbose = verbose)
    res
  })


