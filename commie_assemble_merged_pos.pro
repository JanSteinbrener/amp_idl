;; NOTE: corner scatter NOT subtracted currently

PRO commie_assemble_merged_pos, commie_dataset_struct, commie_script_struct

  compile_opt idl2, hidden

  ;; first normalize the files 
  commie_flux_normalization, commie_dataset_struct, commie_script_struct, $
                             /assemble_merged_pos

  ;; now throw everything together, keeping track of how many
  ;; positions contributed to what pixels.
  svec = [commie_dataset_struct.nx,commie_dataset_struct.ny]
                                
  count = n_elements((*commie_dataset_struct.position_names))
 
  ;; now scale the different contributions and sum them up.
  these_arrays = ptr_new()
  these_relerrs = ptr_new()
  FOR i=0,(count-1) DO BEGIN
     this_array = *((*commie_dataset_struct.pos_merged_samples)[i])
     this_relerr = *((*commie_dataset_struct.relerror_samples)[i])
     ptr_free,(*commie_dataset_struct.relerror_samples)[i]
     these_arrays = [these_arrays,ptr_new(this_array)]
     these_relerrs = [these_relerrs,ptr_new(this_relerr)]
  ENDFOR
  these_arrays = these_arrays[1:*]
  these_relerrs = these_relerrs[1:*]
  ptr_free, commie_dataset_struct.relerror_samples

  commie_dataset_struct.assembled_sample = $
     ptr_new(commie_weighted_averages(these_arrays,these_relerrs))  

  ;; store the relerror towards using it as adi_error_array
  commie_dataset_struct.relerror_samples = ptr_new(these_relerrs)

  ;; free the arrays
  FOR i=0,n_elements(these_arrays)-1 DO BEGIN
     ptr_free,these_arrays[i]
  ENDFOR

  ;; same for the nosamples if it is necessary
  IF ptr_valid((*commie_dataset_struct.pos_merged_nosamples)[0]) THEN BEGIN
     ;; now scale the different contributions and sum them up.
     these_arrays = ptr_new()
     these_relerrs = ptr_new()
     FOR i=0,(count-1) DO BEGIN
        this_array = *((*commie_dataset_struct.pos_merged_nosamples)[i])
        this_relerr = *((*commie_dataset_struct.relerror_no_samples)[i])
        ptr_free,(*commie_dataset_struct.relerror_no_samples)[i]
        these_arrays = [these_arrays,ptr_new(this_array)]
        these_relerrs = [these_relerrs,ptr_new(this_relerr)]
     ENDFOR
     these_arrays = these_arrays[1:*]
     these_relerrs = these_relerrs[1:*]
     ptr_free, commie_dataset_struct.relerror_no_samples

     commie_dataset_struct.assembled_nosample = $
        ptr_new(commie_weighted_averages(these_arrays,these_relerrs))
     
     ;; keep track of relerrors to put them into adi_error_array
     commie_dataset_struct.relerror_no_samples = ptr_new(these_relerrs)

     ;; free the relerrs and the arrays
     FOR i=0,n_elements(these_arrays)-1 DO BEGIN
        ptr_free,these_arrays[i]
     ENDFOR
  ENDIF

  ;; print out some more info if the user desires so.
  IF *commie_script_struct.verbose THEN BEGIN
     commie_verbose_output, commie_dataset_struct, commie_script_struct, $
                            /assemble_merged_pos
  ENDIF ;; verbose
END
