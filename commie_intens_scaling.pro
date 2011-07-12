;; the purpose of this routine is to calculate the scaling factor with
;; which the intensity declines as a function of radius of a given
;; image.
PRO commie_intens_scaling, this_array, roi_index, threshold_scaling

  compile_opt idl2, hidden

  svec = size(this_array)
  n = svec[4]
  ;; get the 2D indices for roi_index
  twod_ref = Array_Indices([svec[1],svec[2]],roi_index,/Dimensions)
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
  ;; let's get the maximum radius.
  max_rad = max(distances)
  averages = Dblarr(max_rad+1)
  ;; now go from 0 to max_rad, averaging over all the pixels with the
  ;; respective radius.
  FOR i=0,max_rad DO BEGIN
     dummy = Double(0)
     these_inds = Where(distances EQ i, count)
     IF count NE 0 THEN BEGIN
        FOR j=0,count-1 DO $
           dummy += this_array[these_inds[j]]
        averages[i] = dummy/count
     ENDIF
  ENDFOR
  
  ;; now need to determine the maximum value of the averages.This
  ;; presumably should be right at the edge of the beamstop. 
  dummy = min(averages,subscript_max=max_i)
  ;; we don't really need fancy scaling cranking algorithms, let's
  ;; just assume 1 from 0 to max_i and then an exponential drop from 1
  ;; to 1/4.
  steepness = 1
  maxdim = double(max([svec[1],svec[2]]))
  slope = 10.^5/((maxdim/2.)^3)
  threshold_scaling = Dblarr(svec[1],svec[2])
  threshold_scaling += 1
  ;FOR i=1,max_rad DO BEGIN
  ;   these_inds = Where(distances EQ i, count)
  ;   IF count NE 0 THEN BEGIN
  ;      threshold_scaling[these_inds] = 1/(slope*i^3)
  ;   ENDIF
  ;ENDFOR
  ;; don't forget the 0 value
  ;threshold_scaling[0] = 1/slope
END


