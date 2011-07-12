PRO commie_find_pos_shifts, commie_script_struct, commie_dataset_struct

  compile_opt idl2, hidden
  
  ;; make sure the input from the scriptfile is not compromised
  IF NOT (ptr_valid(commie_dataset_struct.bs_xs) AND $
          ptr_valid(commie_dataset_struct.bs_ys)) THEN BEGIN
     write = 1
     commie_find_shifts, commie_script_struct, commie_dataset_struct
  ENDIF ELSE BEGIN 
     write = 0
     IF ((n_elements((*commie_dataset_struct.bs_xs)) NE $
          n_elements((*commie_dataset_struct.bs_ys))) OR $
         (n_elements((*commie_dataset_struct.bs_xs)) NE $
          n_elements((*commie_dataset_struct.position_names))))THEN BEGIN
        commie_find_shifts, commie_script_struct, commie_dataset_struct
     ENDIF
  ENDELSE
  
  ;; Write the pos to the scriptfile only if they are not already
  ;; there
  IF write THEN $
     commie_scriptfile_io,(*commie_script_struct.script_filename), $
                          commie_script_struct, commie_dataset_struct, $
                          error_string, /bs_pos
END
