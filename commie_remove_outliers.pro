;; this routine is heavily based on the routine despike.pro from
;; idl_local/x1. It uses a MEDIAN filtering scheme to find pixels
;; above a certain threshold and replaces those with the average of
;; their neighbors.

PRO commie_remove_outliers, commie_dataset_struct, commie_script_struct

  compile_opt idl2, hidden

  nx=commie_dataset_struct.nx
  ny=commie_dataset_struct.ny
  FOR i=0,(n_elements(*(commie_dataset_struct.dummy_arrays))-1) DO BEGIN
     this_array = *((*commie_dataset_struct.dummy_arrays)[i])
     
     median_max = max(median(this_array,3))
     threshold = (1.+0.01* (*commie_script_struct.median_percent))*median_max
 
     spikes = where(this_array GT threshold, n_spikes)
     resulting_array = this_array
     IF (n_spikes NE 0) THEN BEGIN
        FOR i_spike=0,(n_spikes-1) DO BEGIN
           ix = spikes[i_spike] mod nx
           iy = spikes[i_spike] / nx
           IF (ix EQ 0) THEN BEGIN
              ix1 = 1
              ix2 = 2
           ENDIF ELSE IF (ix EQ (nx-1)) THEN BEGIN
              ix1 = nx-2
              ix2 = nx-3
           ENDIF ELSE BEGIN
              ix1 = ix-1
              ix2 = ix+1
           ENDELSE
           
           IF (iy EQ 0) THEN BEGIN
              iy1 = 1
              iy2 = 2
           ENDIF ELSE IF (iy EQ (ny-1)) THEN BEGIN
              iy1 = ny-2
              iy2 = ny-3
           ENDIF ELSE BEGIN
              iy1 = iy+1
              iy2 = iy-1
           ENDELSE
           
           resulting_array[ix,iy] = (0.25*(this_array[ix1,iy]+ $
                                           this_array[ix2,iy]+ $
                                           this_array[ix,iy1]+ $
                                           this_array[ix,iy2]))
        ENDFOR
        ;; free this dummy_array and cast a new pointer immediately
        ptr_free, (*commie_dataset_struct.dummy_arrays)[i]
        (*commie_dataset_struct.dummy_arrays)[i] = ptr_new(resulting_array)
     ENDIF
  ENDFOR
  ;; Give us an update b/c this may take a while
  status_string = 'Removed outliers.'
  commie_messages,status=status_string

END
