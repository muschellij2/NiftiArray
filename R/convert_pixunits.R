# from oro.nifti
# pixunits <- function(xyzt_units) {
#   xyzt = c(
#     bitops::bitAnd(xyzt_units, 7),
#     bitops::bitAnd(xyzt_units, 56)
#   )
#
#   res = sapply(xyzt, function(x) {
#     switch(as.character(x),
#          "0" = "Unknown",       # unspecified units
#          "1" = "m",         # meters
#          "2" = "mm",            # millimeters
#          "3" = "um",        # micrometers
#          "8" = "s",           # seconds
#          "16" = "ms",         # milliseconds
#          "24" = "us",         # microseconds
#          "32" = "Hz",           # Hertz
#          "40" = "ppm",          # parts per million
#          "48" = "rad/s")         # radians per second
#   })
#   res
# }


pixunits = function(xyzt_units) {
  img = updateNifti(image = array(1, dim = rep(1, 3)),
              template = list(xyzt_units = xyzt_units))
  attributes(RNifti::niftiHeader(img))$pixunits
}
