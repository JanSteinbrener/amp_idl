;; this routine removes saturated pixels using mergers
;; where_saturated. 

PRO commie_remove_saturation, commie_dataset_struct, commie_script_struct

  compile_opt idl2, hidden

  FOR i=0, n_elements(*commie_dataset_struct.dummy_arrays)-1 DO BEGIN
     sat_indices = $
        where_saturated(*((*commie_dataset_struct.dummy_arrays)[i]),sat_count, $
                        threshold=(*commie_script_struct.sat_threshold_high), $
                        low_threshold=(*commie_script_struct.sat_threshold_low))

     IF sat_count NE 0 THEN $
        (*((*commie_dataset_struct.dummy_arrays)[i]))[sat_indices]=0
  ENDFOR
END
