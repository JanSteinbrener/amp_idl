;; the purpose of this routine is to calculate the scaling factor with
;; which the intensity declines as a function of radius of a given
;; image.
PRO commie_intens_scaling_alt, this_array, threshold_scaling

  compile_opt idl2, hidden

  svec = size(this_array)

  ;; smooth over the array to find the peak value
  this_array = Smooth(this_array,3)
  dummy = max(this_array, max_i)
  n = svec[4]
  ;; get the 2D indices for roi_index
  twod_ref = Array_Indices([svec[1],svec[2]],max_i,/Dimensions)
  print,twod_ref
  ;; compute an array of distances
  distances = Dblarr(svec[1],svec[2])
  ;distances = Shift(distances,twod_ref[0],twod_ref[1])
  twod_inds = $
     Double(Array_Indices([svec[1],svec[2]],findgen(n),/Dimensions))
  FOR i=Long(0),n-1 DO BEGIN
     distances[i] = $
        Sqrt((twod_ref[0]-twod_inds[0,i])^2+(twod_ref[1]-twod_inds[1,i])^2)
  ENDFOR

  ;; round the distances array to next integer
  distances = Round(distances)
  ;; get the maximum distance
  max_rad = max(distances)

  ;; determine the slope of the fall-off
  maxdim = double(max([svec[1],svec[2]]))
  slope = 10.^5/((maxdim/2.)^3)
  threshold_scaling = Dblarr(svec[1],svec[2])
  threshold_scaling += 1
  FOR i=1,max_rad DO BEGIN
     these_inds = Where(distances EQ i, count)
     IF count NE 0 THEN BEGIN
        threshold_scaling[these_inds] = 1/(slope*i^3)
     ENDIF
  ENDFOR
  ;; don't forget the 0 value
  threshold_scaling[0] = 1/slope
END
