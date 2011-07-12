PRO commie_remove_bs, commie_dataset_struct, commie_script_struct

  compile_opt idl2, hidden

  ;; set up loop through positions, subtract bs_array from each
  ;; position, shifted by bs_xs[0] - bs_xs[i], same for bs_ys.
  pos_count = n_elements(*commie_dataset_struct.pos_merged_samples)
  ;svec = [commie_dataset_struct.nx,commie_dataset_struct.ny]
  FOR i=0,pos_count -1 DO BEGIN
     ;; only if we want to subtract a beamstop
     if (*commie_dataset_struct.bs_xs)[i] ne 0 then begin
        zero_indices = WHERE((SHIFT((*commie_dataset_struct.bs_array), $
                                    (*commie_dataset_struct.bs_xs)[i] - $
                                    (*commie_dataset_struct.bs_xs)[0], $
                                    (*commie_dataset_struct.bs_ys)[i] - $
                                    (*commie_dataset_struct.bs_ys)[0])) EQ 0)
        IF ptr_valid((*commie_dataset_struct.pos_merged_samples)[i]) THEN BEGIN
           (*((*commie_dataset_struct.pos_merged_samples)[i]))[zero_indices] = 0
        ENDIF
        if ptr_valid((*commie_dataset_struct.pos_merged_nosamples)[i]) then $
           begin
         (*((*commie_dataset_struct.pos_merged_nosamples)[i]))[zero_indices] = 0
        endif
     endif
  ENDFOR
  
  ;; print out some more information if the user desires so.
  IF *commie_script_struct.verbose THEN BEGIN
     commie_verbose_output, commie_dataset_struct, commie_script_struct, $
                            /remove_bs
  ENDIF ;; verbose
END
