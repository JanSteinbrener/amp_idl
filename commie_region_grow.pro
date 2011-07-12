;; this routine will attempt to grow an area in this_array starting
;; from this_index and determining all indices that are below
;; max_threshold/r^3, where r is the distance from this_index.
PRO commie_region_grow, this_array, ref_index, max_threshold, $
                        threshold_scaling, roi_indices, bs_array

  compile_opt idl2, hidden

  ;; rotate the array by 90 degrees ccw. This is necessary so that the
  ;; filling up of the inner indices below works. Will be rotated back
  ;; in the end. Also rotate our roi_indices if we need to 
  IF ptr_valid(roi_indices) THEN BEGIN
     dummy_array = intarr(size(this_array,/dimensions))
     dummy_array[*roi_indices] = 1
     dummy_array = Rotate(dummy_array,1)
     rroi_indices = Where(dummy_array)
  ENDIF

  ;; now we can rotate the other arrays
  this_array = Rotate(this_array,1)
  threshold_scaling = Rotate(threshold_scaling,1)
 
  max_threshold = Double(max_threshold)
  ;; get the size
  svec = size(this_array)
  ;; its a 2D array
  n = svec[4]

  ;; convert the ref_index to x and y in the old unrotated array, then
  ;; calculate the new ref_index for the rotated array by hand
  twod_ref = Array_Indices([svec[2],svec[1]],ref_index,/Dimensions)
  temp_ref = ref_index
  ref_index = Long(svec[1])*Long(twod_ref[0]) + Long(svec[1]-twod_ref[1])
  
  ;; define thresholds as max_threshold times threshold scaling.
  thresholds = threshold_scaling*max_threshold
  print,max(thresholds),min(thresholds)

  ;; reduce this to the region of interest if user selected one
  IF ptr_valid(roi_indices) THEN BEGIN
     bel_threshold = $
        Where((this_array[rroi_indices]) LT (thresholds[rroi_indices]), count)
     IF count NE 0 THEN $
        bel_threshold = (rroi_indices)[bel_threshold]
  ENDIF ELSE BEGIN
     ;; now get all indices that are below their threshold
     bel_threshold = Where(this_array LT thresholds,count)
  ENDELSE

  IF count NE 0 THEN BEGIN
     ;; now we need to find the subset of pixels that is connected to
     ;; the reference pixel by next neighbors
     array = Intarr(svec[1],svec[2])
     array[bel_threshold] = 1
     regions = Label_Region(array,/ulong,/all_neighbors)

     ;; select all pixels that belong to same region as ref_index
     bs_indices = Where(regions EQ regions[ref_index])
     
     ;; now create the bs_array
     bs_indices=bs_indices[Uniq(bs_indices,Sort(bs_indices))]
     bs_array = Bytarr(svec[1],svec[2])
     bs_array += 1
     bs_array[bs_indices] = 0
     
     ;; now find the pixels inside the beamstop that have been wrongly
     ;; assigned. Start off by finding the left-right extend of the
     ;; beamstop, and then the bottom end. NOTE: bottom left corner is
     ;; (0,0).
     min_bs_x = min(bs_indices MOD svec[1],max=max_bs_x)
     min_bs_y = Fix((min(bs_indices))/svec[1])
     ;; assume the diagonals have the same length
     max_bs_y = svec[2] -1
     ;; in this square, scan line by line
     ;; and find the first and the last pixel that is beamstop. All
     ;; pixels between those two set to 0.
     
     FOR i=min_bs_y, max_bs_y DO BEGIN
        these_indices = Where(bs_array[min_bs_x:max_bs_x,i] EQ 0,count)
        IF count GT 1 THEN BEGIN
           ;; determine min/max of those indices
           this_min = min(these_indices, max=this_max)
           FOR j=(min_bs_x+this_min),(min_bs_x+this_max) DO BEGIN
              this_index = i*svec[1]+j
              bs_array[this_index] = 0
           ENDFOR
        ENDIF
     ENDFOR
  ENDIF ELSE BEGIN
     ;; in this case our bs_array is just black
     bs_array = Bytarr(svec[1],svec[2])
     bs_array += 1
  ENDELSE
  ;; rotate arrays back to normal
  this_array = Rotate(this_array,3)
  bs_array = Rotate(bs_array, 3)
  threshold_scaling = Rotate(threshold_scaling,3)
  ;; restore the old index
  ref_index = temp_ref
END

 
