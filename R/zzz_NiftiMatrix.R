
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

#' @aliases coerce,DelayedMatrix,NiftiArray-method
#' @rdname NiftiMatrix
#' @name coerce
#' @export
setAs("DelayedMatrix", "NiftiMatrix", .as_NiftiArray)


#' @importMethodsFrom DelayedArray matrixClass
#' @rdname NiftiMatrix
#' @aliases matrixClass,NiftiArray-method
#' @name matrixClass
setMethod("matrixClass", "NiftiArray", function(x) "NiftiMatrix")

#' @aliases coerce,NiftiArray,NiftiMatrix-method
#' @importMethodsFrom methods coerce
#' @rdname NiftiMatrix
#' @name coerce
#' @export
setAs("NiftiArray", "NiftiMatrix", function(from) {
  # mat = matrix(x, ncol = 1)
  # writeNiftiArray(mat, )
  new("NiftiMatrix", from)
})


#' @aliases coerce,NiftiMatrix,NiftiArray-method
#' @rdname NiftiMatrix
#' @export
#' @name coerce
setAs("NiftiMatrix", "NiftiArray", function(from) from)  # no-op

#' @rdname NiftiMatrix
#' @aliases coerce,ANY,NiftiMatrix-method
#' @export
#' @name coerce
setAs("ANY", "NiftiMatrix",
      function(from) as(as(from, "NiftiArray"), "NiftiMatrix")
)
