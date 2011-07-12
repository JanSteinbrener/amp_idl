PRO commie_find_bs_arr, commie_script_struct, commie_dataset_struct

  compile_opt idl2, hidden

  these_xshifts = (*commie_dataset_struct.bs_xs)
  these_yshifts = (*commie_dataset_struct.bs_ys)

  ;; see if user loaded bs_array from disk
  commie_saveread_bs, commie_script_struct, commie_dataset_struct, /read
  IF NOT ptr_valid(commie_dataset_struct.bs_array) THEN BEGIN
     ;; define the temp_arrays
     temp_arrays = ptrarr(n_elements(*commie_dataset_struct.position_names))
     ;; choose position 1 as our reference
     these_xshifts -= these_xshifts[0]
     these_yshifts -= these_yshifts[0]

     FOR i=0,(n_elements((*commie_dataset_struct.position_names))-1) DO BEGIN
        ;; also add up the no_sample files to smooth out the
        ;; diffraction pattern.
        these_indices = $
           Where(((*commie_dataset_struct.position_indices) EQ i), count)
        IF count NE 0 THEN BEGIN
           ;; now add up all arrays for each position. Need to remove
           ;; saturation first.
           temp_arr=double(0)
           FOR j=0,count-1 DO BEGIN
              commie_read_dt, commie_script_struct, commie_dataset_struct, $
                         (*commie_dataset_struct.filenames)[these_indices[j]], $
                              this_dt_par,this_array
              ;; get the saturated indices first
              sat_indices = $
                 where_saturated(this_array,sat_count,threshold= $
                                 (*commie_script_struct.sat_threshold_high), $
                                 low_threshold= $
                                 (*commie_script_struct.sat_threshold_low))
              
              IF sat_count NE 0 THEN $
                 this_array[sat_indices] = 0
              ;; don't add long exposures here.
              ;IF this_dt_par.image_seconds LT 10.0 THEN $
              temp_arr += this_array
           ENDFOR
           temp_arrays[i] = ptr_new(temp_arr)
        ENDIF  
     ENDFOR

     ;; now add all positions shifted wrt zero position. Make sure that
     ;; we only sum over the parts of the shifted arrays that overlap
     ;; with the reference array. 
     final_array = *(temp_arrays[0])
     FOR i=1, n_elements(temp_arrays)-1 DO BEGIN
        ;; only if we were using a beamstop
        if (*commie_dataset_struct.bs_xs)[i] ne 0 then begin
           IF (these_yshifts[i] GT 0) THEN BEGIN
              IF (these_xshifts[i] GT 0) THEN BEGIN
                 FOR j=0,(commie_dataset_struct.nx-(these_xshifts[i])-1) DO $
                    BEGIN
                    FOR k=0,(commie_dataset_struct.ny-(these_yshifts[i])-1) DO $
                       BEGIN
                       final_array[j,k] += $
                          (*(temp_arrays[i]))[j+these_xshifts[i], $
                                              k+these_yshifts[i]]
                    ENDFOR
                 ENDFOR
              ENDIF
              IF (these_xshifts[i] LT 0) THEN BEGIN
                 FOR j=-these_xshifts[i],(commie_dataset_struct.nx-1) DO BEGIN
                    FOR k=0,(commie_dataset_struct.ny-(these_yshifts[i])-1) DO $
                       BEGIN
                       final_array[j,k] += $
                          (*(temp_arrays[i]))[j+these_xshifts[i], $
                                              k+these_yshifts[i]]
                    ENDFOR
                 ENDFOR
              ENDIF
              ;; two different positions can have the same bs_x!!
              IF (these_xshifts[i] EQ 0) THEN BEGIN
                 FOR j=0,(commie_dataset_struct.nx-1) DO BEGIN
                    FOR k=0,(commie_dataset_struct.ny-(these_yshifts[i])-1) DO $
                       BEGIN
                       final_array[j,k] += $
                          (*(temp_arrays[i]))[j,k+these_yshifts[i]]
                    ENDFOR
                 ENDFOR
              ENDIF
           ENDIF
           IF (these_yshifts[i] LT 0) THEN BEGIN
              IF (these_xshifts[i] GT 0) THEN BEGIN
                 FOR j=0,(commie_dataset_struct.nx-(these_xshifts[i])-1) DO $
                    BEGIN
                    FOR k=-these_yshifts[i],(commie_dataset_struct.ny-1) DO $
                       BEGIN
                       final_array[j,k] += $
                          (*(temp_arrays[i]))[j+these_xshifts[i], $
                                              k+these_yshifts[i]]
                    ENDFOR
                 ENDFOR
              ENDIF
              IF (these_xshifts[i] LT 0) THEN BEGIN
                 FOR j=-these_xshifts[i],(commie_dataset_struct.nx-1) DO BEGIN
                    FOR k=-these_yshifts[i],(commie_dataset_struct.ny-1) DO $
                       BEGIN
                       final_array[j,k] += $
                          (*(temp_arrays[i]))[j+these_xshifts[i], $
                                              k+these_yshifts[i]]
                    ENDFOR
                 ENDFOR
              ENDIF
              ;; two different positions can have the same bs_x!!
              IF (these_xshifts[i] EQ 0) THEN BEGIN
                 FOR j=0,(commie_dataset_struct.nx-1) DO BEGIN
                    FOR k=-these_yshifts[i],(commie_dataset_struct.ny-1) DO $
                       BEGIN
                       final_array[j,k] += $
                          (*(temp_arrays[i]))[j,k+these_yshifts[i]]
                    ENDFOR
                 ENDFOR
              ENDIF
           ENDIF
        endif
     ENDFOR

     ;; now let's try to grow the region within the beamstop. 
     commie_find_bs_widget,commie_dataset_struct, final_array, bs_array
  
     ;; now store bs in commie_dataset_struct
     commie_dataset_struct.bs_array = ptr_new(bs_array)
     
     ;; write the new bs_array to disk if the user specified a
     ;; filename
     commie_saveread_bs, commie_script_struct, commie_dataset_struct, /write

     ;; free the temp_arrays
     FOR i=0,n_elements(temp_arrays)-1 DO $
        ptr_free, temp_arrays[i]
  ENDIF ;; ptr_valid(commie_dataset_struct.bs_array)

  ;; print out some additional stuff if the user specified verbose
  ;; output. 
  IF *commie_script_struct.verbose THEN BEGIN
     commie_verbose_output, commie_dataset_struct, commie_script_struct, $
                            /find_bs
  ENDIF ;; verbose
END
