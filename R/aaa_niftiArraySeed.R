### ========================
### NiftiArraySeed objects
### ----------------------


#' "NiftiArrayList" class
#'
#' @name NiftiArrayList-class
#' @family NiftiArrayList
#'
setOldClass("NiftiArrayList")


#' @importClassesFrom HDF5Array HDF5ArraySeed
#' @aliases DelayedArray,NiftiArraySeed-method
#' @exportClass NiftiArraySeed
#' @rdname NiftiArraySeed
setClass(
    "NiftiArraySeed",
    contains = "HDF5ArraySeed",
    slots = c(
        header_name = "character",
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
        magic = "character",
        extendible = "logical"))

#' @importClassesFrom HDF5Array HDF5Array
#' @rdname NiftiArray
#' @exportClass NiftiArray
setClass(
    "NiftiArray",
    contains = "HDF5Array",
    slots = c(
        seed = "NiftiArraySeed"))

#' NiftiMatrix Class
#'
#' @importClassesFrom DelayedArray DelayedMatrix
#' @rdname NiftiMatrix
#'
#' @return A `NiftiMatrix` object.
#' @exportClass NiftiMatrix
setClass("NiftiMatrix", contains = c("NiftiArray", "DelayedMatrix"))



#' Seed for NiftiArray Class
#'
#' @param filepath The path (as a single character string) to the NIfTI or HDF5
#'  file where the dataset is located. If a path to the NIfTI is provided we call
#'  [RNifti::readNifti()] and [NiftiArray::writeNiftiArray()] to convert to the HDF5
#'  and more memory and time are used. If a path to a HDF5 file the data is simply loaded
#'  into R as an object of class [NiftiArray]. A path to the HDF5 file is more memory and time efficient.
#' @param name The name of the group for the NIfTI image in the HDF5 file. Default is set to "image".
#' Unless you have to other "image" groups in the HDF5 file there is no need to change default settings.
#' @param header_name The name of the group for the NIfTI header in the HDF5 file. Default is set to "header".
#' Unless you have to other "header" groups in the HDF5 file there is no need to change default settings.
#' @param type `NA` or the R atomic type, passed to
#' [HDF5Array::HDF5Array()] corresponding to the type of the HDF5 dataset. Default is set to `NA`.
#' Unless you want different types of HDF5 storage files there is no need to change default settings.
#' @param header List of NIfTI header information to override call of
#' [nifti_header].
#' @param extendible Should a single empty dimension be added to the array?
#' Currently necessary for easy reshaping.
#'
#' @return A `NiftiArraySeed` object
#' @export
#' @importFrom HDF5Array HDF5ArraySeed
#' @importFrom rhdf5 h5read
#' @importFrom S4Vectors new2
#' @examples
#' nii_fname = system.file("extdata",
#' "example.nii.gz", package = "RNifti")
#' res = NiftiArraySeed(nii_fname)
#' hdr = nifti_header(res)
#' res2 = NiftiArraySeed(nii_fname, header = hdr)
NiftiArraySeed <- function(
    filepath,
    name = "image", header_name = "hdr",
    type = NA,
    header = NULL,
    extendible = FALSE) {
    # for filepath for .nii.gz
    fe = tools::file_ext(filepath)
    if (fe == "gz") {
        fe = tools::file_ext(sub("[.]gz$", "", filepath))
    }
    if (fe == "nii") {
        x = RNifti::readNifti(filepath)
        filepath = tempfile(fileext = ".h5")
        writeNiftiArray(x, filepath = filepath,
                        name = name, header_name = header_name,
                        header = header,
                        extendible = extendible)
        rm(x); gc()
    }

    seed = HDF5Array::HDF5ArraySeed(
        filepath, name = name, type = type)
    .niftiArraySeed_from_HDF5ArraySeed(
        seed,
        header_name = header_name,
        header = header,
        seed_type = "NiftiArraySeed",
        extendible = extendible)
}


.niftiArraySeed_from_HDF5ArraySeed = function(seed,
                                              header_name = "hdr",
                                              header = NULL,
                                              args = list(),
                                              seed_type = "NiftiArraySeed",
                                              extendible = FALSE) {

    args$filepath = seed@filepath
    args$name = seed@name
    args$dim = seed@dim
    args$first_val = seed@first_val
    args$chunkdim = seed@chunkdim

    if (is.null(header)) {
        if (header_name %in% rhdf5::h5ls(seed@filepath)$name) {
            hdr = rhdf5::h5read(seed@filepath, name = header_name)
        } else {
            warning("Header not found in filepath, using default header")
            hdr = RNifti::niftiHeader()
        }
    } else {
        hdr = header
    }
    if ("dim" %in% names(hdr)) {
        if (!("dim_" %in% names(hdr))) {
            hdr$dim_ = hdr$dim
        }
        hdr$dim = NULL
    }
    hdr = lapply(hdr, as.vector)
    args = c(args, hdr)
    args$header_name = header_name
    args$extendible = extendible
    args = c(seed_type, args )
    do.call(S4Vectors::new2, args = args)
}
