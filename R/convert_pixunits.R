pixunits = function(xyzt_units) {
  img = updateNifti(image = array(1, dim = rep(1, 3)),
              template = list(xyzt_units = xyzt_units))
  attributes(RNifti::niftiHeader(img))$pixunits
}
